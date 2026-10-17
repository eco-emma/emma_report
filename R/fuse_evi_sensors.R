#' Multi-sensor EVI fusion (VIIRS → Terra → Aqua)
#'
#' Priority fill per composite date / pixel: first non-NA wins.
#' Order: VIIRS NOAA-20 (4) → VIIRS S-NPP (3) → Terra (1) → Aqua (2).
#' Climatologies (monthly / long-term) fuse per (pid, date) then mean with
#' `na.rm = TRUE`. Do not average raw multi-sensor rows without the fuse step.

#' Named sensor tokens in fuse priority order (highest first)
EVI_FUSE_PRIORITY <- c(
  "viirs_noaa20",
  "viirs_snpp",
  "modis_terra",
  "modis_aqua"
)

#' Parquet / STAC sensor integer codes matching emma_envdata writers
EVI_SENSOR_IDS <- c(
  viirs_noaa20 = 4L,
  viirs_snpp = 3L,
  modis_terra = 1L,
  modis_aqua = 2L
)

#' STAC asset keys for VI band-1 COGs (one composite date)
EVI_STAC_ASSET_KEYS <- c(
  viirs_noaa20 = "viirs_noaa20_vi",
  viirs_snpp = "viirs_snpp_vi",
  modis_terra = "modis_terra_vi",
  modis_aqua = "modis_aqua_vi"
)

#' Release tags hosting each sensor's COG
EVI_SENSOR_RELEASE_TAGS <- c(
  viirs_noaa20 = "vi_viirs_raster",
  viirs_snpp = "vi_viirs_raster",
  modis_terra = "vi_modis_terra_raster",
  modis_aqua = "vi_modis_aqua_raster"
)

#' Priority-fill one vector of EVI values by sensor id
#'
#' @param sensor Integer sensor codes (1 Terra, 2 Aqua, 3 S-NPP, 4 NOAA-20).
#' @param value Numeric EVI values (same length as `sensor`); may be NA.
#' @param priority Sensor ids in fill order (default 4 → 3 → 1 → 2).
#' @return Single numeric value, or `NA_real_` if all sensors are missing.
fuse_evi_sensor_values <- function(sensor,
                                   value,
                                   priority = c(4L, 3L, 1L, 2L)) {
  sensor <- as.integer(sensor)
  value <- as.numeric(value)
  if (!length(value)) {
    return(NA_real_)
  }
  for (sid in as.integer(priority)) {
    hit <- !is.na(sensor) & sensor == sid & !is.na(value)
    if (any(hit)) {
      return(value[which(hit)[[1L]]])
    }
  }
  NA_real_
}

#' Fuse multi-sensor EVI rows for each (pid, date)
#'
#' @param data Data frame / tibble with columns `pid`, `date`, `sensor`, `value`.
#' @param priority Sensor ids in fill order.
#' @return Tibble with `pid`, `date`, `value` (one fused value per group).
#'
#' Uses a wide coalesce (not per-group R callbacks) so all-history
#' `mean_evi.tif` builds stay within GHA time budgets once VIIRS parquets land.
fuse_evi_by_pid_date <- function(data, priority = c(4L, 3L, 1L, 2L)) {
  required <- c("pid", "date", "sensor", "value")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("fuse_evi_by_pid_date() missing columns: ", paste(missing, collapse = ", "))
  }
  priority <- as.integer(priority)
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  df$pid <- as.integer(df$pid)
  df$date <- as.Date(df$date)
  df$sensor <- as.integer(df$sensor)
  df$value <- as.numeric(df$value)
  # Last write wins within a sensor; keep first non-NA via coalesce below.
  df <- df[!is.na(df$pid) & !is.na(df$date) & !is.na(df$sensor), , drop = FALSE]
  if (!nrow(df)) {
    return(data.frame(
      pid = integer(),
      date = as.Date(character()),
      value = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  # One column per sensor id present; coalesce in priority order.
  # Avoid dplyr::group_by + per-group R callbacks (too slow for ~600 dates).
  wide <- tidyr::pivot_wider(
    df,
    id_cols = c("pid", "date"),
    names_from = "sensor",
    names_prefix = "s",
    values_from = "value",
    values_fn = function(x) x[!is.na(x)][1]
  )
  cols <- paste0("s", priority)
  present <- cols[cols %in% names(wide)]
  if (!length(present)) {
    wide$value <- NA_real_
  } else if (length(present) == 1L) {
    wide$value <- as.numeric(wide[[present]])
  } else {
    wide$value <- as.numeric(
      do.call(dplyr::coalesce, unname(as.list(wide[present])))
    )
  }
  as.data.frame(wide[c("pid", "date", "value")], stringsAsFactors = FALSE)
}

#' Priority-fill a named list / SpatRaster stack of sensor EVI layers
#'
#' Uses `terra::cover()` so NA in higher-priority layers is filled from lower
#' ones. Layer names (or list names) should be tokens in `EVI_FUSE_PRIORITY`.
#'
#' @param layers Named list of SpatRaster, or a multi-layer SpatRaster with
#'   layer names matching sensor tokens.
#' @param priority Character vector of sensor tokens, highest first.
#' @return Single-layer SpatRaster named `"evi"`.
fuse_evi_rasters <- function(layers, priority = EVI_FUSE_PRIORITY) {
  if (inherits(layers, "SpatRaster")) {
    nm <- names(layers)
    layer_list <- lapply(seq_len(terra::nlyr(layers)), function(i) layers[[i]])
    names(layer_list) <- nm
    layers <- layer_list
  }
  if (!is.list(layers) || !length(layers)) {
    stop("fuse_evi_rasters() needs a named list or SpatRaster of sensor layers")
  }
  if (is.null(names(layers)) || any(!nzchar(names(layers)))) {
    stop("fuse_evi_rasters() layers must be named with sensor tokens")
  }

  available <- priority[priority %in% names(layers)]
  if (!length(available)) {
    stop(
      "fuse_evi_rasters() found no layers matching priority tokens. ",
      "Have: ", paste(names(layers), collapse = ", ")
    )
  }

  out <- layers[[available[[1L]]]]
  if (length(available) > 1L) {
    for (tok in available[-1L]) {
      out <- terra::cover(out, layers[[tok]])
    }
  }
  names(out) <- "evi"
  out
}

#' Parse tag + file from a GitHub release download URL
parse_envdata_release_href <- function(href) {
  href <- as.character(href[[1L]])
  # …/releases/download/<tag>/<file>
  m <- regexec("/releases/download/([^/]+)/([^/?#]+)$", href)
  parts <- regmatches(href, m)[[1L]]
  if (length(parts) < 3L) {
    stop("Cannot parse envdata release href: ", href)
  }
  list(tag = parts[[2L]], file = utils::URLdecode(parts[[3L]]))
}
