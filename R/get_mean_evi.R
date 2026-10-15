library(terra)

#' Long-term mean EVI from vi_modis_parquet releases (MODIS Terra+Aqua interim)
#'
#' Parquet schema: pid, date, sensor, value (EVI × 100). Returns a SpatRaster
#' on the domain grid in EVI units (value / 100), matching get_most_recent_evi.tif
#' and get_monthly_mean_evi.tif.
#'
#' W0 interim: MODIS Terra+Aqua only, ~2 composites per calendar year (keeps
#' report CI tractable vs ~3 GB all-history download). W4 replaces this with
#' VIIRS→Terra→Aqua fuse-then-mean over all `vi_modis_parquet` +
#' `vi_viirs_parquet` dates.
get_mean_evi.tif <- function(env_files,
                             temp_directory = "data/temp/",
                             composites_per_year = 2L) {

  if (is.null(env_files) || !nrow(env_files)) {
    env_files <- list_release_assets(
      repo = ENVDATA_REPO,
      tag = ENVDATA_RELEASE_TAGS$vi_modis_parquet
    )
  }

  vi_files <- env_files |>
    dplyr::filter(
      .data$tag == ENVDATA_RELEASE_TAGS$vi_modis_parquet,
      grepl("^vi_modis_[0-9]{8}\\.parquet$", .data$file_name)
    ) |>
    dplyr::mutate(file_date = parse_envdata_yyyymmdd(.data$file_name)) |>
    dplyr::filter(!is.na(.data$file_date)) |>
    dplyr::arrange(dplyr::desc(.data$file_date))

  if (nrow(vi_files) == 0L) {
    stop(
      "No vi_modis_YYYYMMDD.parquet assets found on tag ",
      ENVDATA_RELEASE_TAGS$vi_modis_parquet
    )
  }

  # Spread samples across each year (newest first within year).
  vi_files <- vi_files |>
    dplyr::mutate(year = lubridate::year(.data$file_date)) |>
    dplyr::group_by(.data$year) |>
    dplyr::slice_head(n = as.integer(composites_per_year)) |>
    dplyr::ungroup() |>
    dplyr::select(-"year") |>
    dplyr::arrange(.data$file_date)

  for (f in vi_files$file_name) {
    download_envdata_asset(
      tag = ENVDATA_RELEASE_TAGS$vi_modis_parquet,
      file = f,
      dest_dir = temp_directory,
      overwrite = FALSE
    )
  }

  mean_values <- arrow::open_dataset(
    file.path(temp_directory, vi_files$file_name)
  ) |>
    dplyr::group_by(pid) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect()

  domain_path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$static,
    file = "domain.tif",
    dest_dir = temp_directory,
    overwrite = FALSE
  )
  domain <- terra::rast(domain_path)
  pid_layer <- if ("pid" %in% names(domain)) domain[["pid"]] else domain[[1]]

  out <- pid_layer
  terra::values(out) <- NA_real_
  pid_vals <- terra::values(pid_layer)[, 1]
  idx <- match(mean_values$pid, pid_vals)
  keep <- !is.na(idx)
  terra::values(out)[idx[keep]] <- scale_evi_integer(mean_values$value[keep])
  names(out) <- "evi"
  out <- terra::mask(out, mask = out, maskvalue = 0)
  out
}

#' Long-term delta EVI: current observation minus long-term mean (same units)
get_delta_evi.tif <- function(most_recent_evi.tif, mean_evi.tif) {
  delta_evi.tif <- most_recent_evi.tif - mean_evi.tif

  if (terra::crs(most_recent_evi.tif, proj = TRUE) !=
      terra::crs(mean_evi.tif, proj = TRUE)) {
    stop("VI CRS mismatch")
  }
  if (terra::ext(most_recent_evi.tif) != terra::ext(mean_evi.tif)) {
    stop("VI extent mismatch")
  }
  if (terra::crs(delta_evi.tif, proj = TRUE) !=
      terra::crs(most_recent_evi.tif, proj = TRUE)) {
    terra::crs(delta_evi.tif) <- terra::crs(most_recent_evi.tif)
  }
  names(delta_evi.tif) <- "evi"
  delta_evi.tif
}
