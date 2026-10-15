#' Current envdata accessors for the report pipeline
#'
#' These functions consume the eco-emma/emma_envdata public release contract
#' (see R/envdata_contract.R). Vegetation product is EVI throughout targets and report templates.

get_temp_directory <- function(temp_directory = "data/temp") {
  if (!dir.exists(file.path(temp_directory))) {
    dir.create(file.path(temp_directory), recursive = TRUE)
  }
  temp_directory
}

#' List envdata release assets used by the report pipeline
#'
#' Returns a piggyback-style data frame (file_name, tag, ...) across the
#' active public tags. Prefer STAC helpers for "latest VI" discovery.
get_env_files <- function(repo = ENVDATA_REPO,
                          tags = NULL) {
  if (is.null(tags)) {
    # Only the public data tags the report pipeline reads
    tags <- c(
      ENVDATA_RELEASE_TAGS$static,
      ENVDATA_RELEASE_TAGS$vi_modis_terra,
      ENVDATA_RELEASE_TAGS$vi_modis_aqua,
      ENVDATA_RELEASE_TAGS$vi_modis_parquet,
      ENVDATA_RELEASE_TAGS$vi_viirs,
      ENVDATA_RELEASE_TAGS$vi_viirs_parquet,
      ENVDATA_RELEASE_TAGS$fireage,
      ENVDATA_RELEASE_TAGS$stac_vi,
      ENVDATA_RELEASE_TAGS$stac_burn
    )
  }
  rows <- lapply(tags, function(tag) {
    tryCatch(
      list_release_assets(repo = repo, tag = tag),
      error = function(e) {
        message("Skipping tag ", tag, ": ", conditionMessage(e))
        NULL
      }
    )
  })
  dplyr::bind_rows(rows)
}

get_report_files <- function(repo = REPORT_REPO, create_park_tag = FALSE) {
  # List across known report tags; fall back to empty frame.
  # park_data is retired as SoT (parks come from emma_envdata@static_data); do not
  # create/upload that release tag anymore (Phase C / R2).
  tags <- tryCatch({
    # releases listing via API
    token <- Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GITHUB_TOKEN", unset = ""))
    headers <- c(Accept = "application/vnd.github+json", `User-Agent` = "eco-emma/emma_report")
    if (nzchar(token)) headers <- c(headers, Authorization = paste("Bearer", token))
    tf <- tempfile(fileext = ".json")
    utils::download.file(
      sprintf("https://api.github.com/repos/%s/releases?per_page=100", repo),
      tf, quiet = TRUE, headers = headers
    )
    rels <- jsonlite::fromJSON(tf, simplifyVector = TRUE)
    unlink(tf)
    unique(as.character(rels$tag_name))
  }, error = function(e) character())

  rows <- lapply(tags, function(tag) {
    tryCatch(list_release_assets(repo = repo, tag = tag), error = function(e) NULL)
  })
  if (isTRUE(create_park_tag)) {
    message(
      "get_report_files(): create_park_tag=TRUE ignored; ",
      "park_data release creation is retired"
    )
  }
  dplyr::bind_rows(rows)
}

#' Years since fire from fireage_current.tif (fireage release)
#'
#' Band 1 of fireage_current.tif is fire_age_days. Converted to years for the
#' existing report symbology (column/layer name "Years").
get_years_since_fire.tif <- function(env_files,
                                     temp_directory,
                                     most_recent_fire_date = NULL,
                                     piggyback_push = FALSE) {
  path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$fireage,
    file = "fireage_current.tif",
    dest_dir = temp_directory,
    overwrite = TRUE
  )
  fireage <- terra::rast(path)
  age_days <- fireage[[1]]
  years_since_fire.tif <- age_days / 365.25
  names(years_since_fire.tif) <- "Years"
  years_since_fire.tif[years_since_fire.tif < 0] <- NA

  if (isTRUE(piggyback_push)) {
    out <- file.path(temp_directory, "years_since_fire.tif")
    terra::writeRaster(years_since_fire.tif, out, overwrite = TRUE)
    robust_pb_upload(
      file = out,
      repo = REPORT_REPO,
      tag = "current",
      max_attempts = 10,
      sleep_time = 10,
      temp_directory = temp_directory,
      overwrite = TRUE
    )
  }

  years_since_fire.tif
}

#' Crop years-since-fire to remnants and return WGS84 polygons for leaflet
generate_fires_vector <- function(years_since_fire.tif) {
  remnants_path <- "data/misc/remnants.tif"
  if (!file.exists(remnants_path)) {
    write_remnants_from_domain(dest = remnants_path)
  }
  remnants <- terra::rast(remnants_path)
  years_since_fire.tif <- terra::mask(years_since_fire.tif, remnants)

  fires_wgs <- terra::as.polygons(x = years_since_fire.tif) |>
    sf::st_as_sf() |>
    dplyr::rename(Years = 1) |>
    sf::st_transform(crs = sf::st_crs(4326))

  fires_wgs
}

#' Most recent multi-sensor VI COG metadata (via STAC)
#'
#' Returns one row per available sensor asset on the latest STAC item, ordered
#' by fuse priority (NOAA-20 → S-NPP → Terra → Aqua). Columns: sensor, file_name,
#' tag, href, file_date, asset_key.
get_most_recent_evi_file <- function(env_files = NULL) {
  item <- get_latest_vi_stac_item()
  assets <- item$assets
  rows <- list()
  for (tok in EVI_FUSE_PRIORITY) {
    asset_key <- EVI_STAC_ASSET_KEYS[[tok]]
    asset <- assets[[asset_key]]
    if (is.null(asset) || is.null(asset$href)) {
      next
    }
    href <- asset$href
    parsed <- tryCatch(
      parse_envdata_release_href(href),
      error = function(e) {
        list(
          tag = unname(EVI_SENSOR_RELEASE_TAGS[[tok]]),
          file = basename(href)
        )
      }
    )
    rows[[length(rows) + 1L]] <- tibble::tibble(
      sensor = tok,
      sensor_id = unname(EVI_SENSOR_IDS[[tok]]),
      asset_key = asset_key,
      file_name = parsed$file,
      tag = parsed$tag %||% unname(EVI_SENSOR_RELEASE_TAGS[[tok]]),
      href = href,
      file_date = parse_envdata_yyyymmdd(parsed$file)
    )
  }
  if (!length(rows)) {
    stop(
      "Latest VI STAC item has no sensor COG assets among: ",
      paste(EVI_STAC_ASSET_KEYS, collapse = ", ")
    )
  }
  dplyr::bind_rows(rows)
}

#' Read, scale, and clamp one EVI COG band to approximately [-1, 1]
#'
#' Integer storage on release assets is ×100. Zeros are masked to NA.
scale_evi_cog_band <- function(path) {
  r <- terra::rast(path)
  evi <- if (terra::nlyr(r) >= 1L) r[[1]] else r
  evi <- scale_evi_integer(evi)
  evi[evi > 1] <- 1
  evi[evi < -1] <- -1
  evi <- terra::mask(evi, mask = evi, maskvalue = 0)
  names(evi) <- "evi"
  evi
}

#' Download and fuse multi-sensor EVI COGs for the latest STAC composite
#'
#' Priority fill: VIIRS NOAA-20 → VIIRS S-NPP → Terra → Aqua. Remaining NA
#' means all available sensors were missing after QA (water / cloud / etc.).
get_most_recent_evi.tif <- function(most_recent_evi_file, temp_directory) {
  if (!nrow(most_recent_evi_file)) {
    stop("most_recent_evi_file is empty")
  }
  layers <- list()
  for (i in seq_len(nrow(most_recent_evi_file))) {
    row <- most_recent_evi_file[i, , drop = FALSE]
    path <- download_envdata_asset(
      tag = row$tag[[1]],
      file = row$file_name[[1]],
      dest_dir = temp_directory,
      overwrite = TRUE
    )
    tok <- row$sensor[[1]]
    layers[[tok]] <- scale_evi_cog_band(path)
  }
  fuse_evi_rasters(layers, priority = EVI_FUSE_PRIORITY)
}

get_most_recent_evi_date <- function(most_recent_evi_file) {
  if ("file_date" %in% names(most_recent_evi_file)) {
    dates <- most_recent_evi_file$file_date
    dates <- dates[!is.na(dates)]
    if (length(dates)) {
      return(dates[[1L]])
    }
  }
  parse_envdata_yyyymmdd(most_recent_evi_file$file_name[[1]])
}

#' Assert two EVI SpatRasters share CRS and extent, then subtract
evi_delta_rasters <- function(current, mean_rast, label = "VI") {
  delta <- current - mean_rast
  if (terra::crs(current, proj = TRUE) != terra::crs(mean_rast, proj = TRUE)) {
    stop(label, " CRS mismatch")
  }
  if (terra::ext(current) != terra::ext(mean_rast)) {
    stop(label, " extent mismatch")
  }
  if (terra::crs(delta, proj = TRUE) != terra::crs(current, proj = TRUE)) {
    terra::crs(delta) <- terra::crs(current)
  }
  names(delta) <- "evi"
  delta
}

#' Monthly mean EVI minus current observation (same units)
get_monthly_delta_evi.tif <- function(most_recent_evi.tif, monthly_mean_evi.tif) {
  evi_delta_rasters(
    most_recent_evi.tif,
    monthly_mean_evi.tif,
    label = "Monthly VI"
  )
}

#' Long-term mean EVI minus current observation (same units)
get_delta_evi.tif <- function(most_recent_evi.tif, mean_evi.tif) {
  evi_delta_rasters(
    most_recent_evi.tif,
    mean_evi.tif,
    label = "Long-term VI"
  )
}
