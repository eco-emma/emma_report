library(terra)

#' Mean EVI for the current calendar month from vi_modis_parquet releases
#'
#' Parquet schema: pid, date, sensor, value (EVI × 100). Returns a SpatRaster
#' on the domain grid in EVI units (value / 100), matching get_most_recent_ndvi.tif.
get_monthly_mean_ndvi.tif <- function(env_files,
                                      temp_directory = "data/temp/",
                                      current_month = current_month) {

  # Prefer the pre-fetched env_files listing; fall back to listing the parquet tag
  if (is.null(env_files) || !nrow(env_files)) {
    env_files <- piggyback::pb_list(
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
    dplyr::filter(!is.na(.data$file_date),
                  lubridate::month(.data$file_date) == current_month)

  if (nrow(vi_files) == 0L) {
    stop(
      "No vi_modis_YYYYMMDD.parquet assets found for month ",
      current_month,
      " on tag ",
      ENVDATA_RELEASE_TAGS$vi_modis_parquet
    )
  }

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
