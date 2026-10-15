#' Park-cell fire history from the fireage release
#'
#' Downloads fireage_modis_*.parquet files and domain.tif, then returns
#' fire_age_days / last_burn_date rows for pixels intersecting protected areas.
#' Schema uses `pid` (not legacy cellID).
#'
#' History depth combines:
#' - the most recent `n_recent` composites (fine temporal ridgeplots), and
#' - one composite per calendar year for the last `n_years` years (multi-year
#'   calendar burned-area series without downloading the full fireage archive).
get_fire_history <- function(temp_directory = "data/temp/fire_history",
                             max_attempts = 10,
                             sleep_time = 10,
                             protected_areas = protected_areas,
                             n_recent = 24L,
                             n_years = 20L) {

  if (!dir.exists(temp_directory)) {
    dir.create(temp_directory, recursive = TRUE, showWarnings = FALSE)
  }

  # Use GitHub API listing — piggyback::pb_list fails on large fireage releases
  all_assets <- list_release_assets(
    repo = ENVDATA_REPO,
    tag = ENVDATA_RELEASE_TAGS$fireage
  ) |>
    dplyr::filter(grepl("^fireage_modis_[0-9]{8}\\.parquet$", .data$file_name)) |>
    dplyr::mutate(file_date = parse_envdata_yyyymmdd(.data$file_name)) |>
    dplyr::filter(!is.na(.data$file_date)) |>
    dplyr::arrange(dplyr::desc(.data$file_date))

  if (nrow(all_assets) == 0L) {
    stop(
      "No fireage_modis_*.parquet assets found on tag ",
      ENVDATA_RELEASE_TAGS$fireage
    )
  }

  recent <- all_assets |>
    dplyr::slice_head(n = as.integer(n_recent))

  max_year <- max(lubridate::year(all_assets$file_date), na.rm = TRUE)
  annual <- all_assets |>
    dplyr::mutate(year = lubridate::year(.data$file_date)) |>
    dplyr::filter(.data$year >= (max_year - as.integer(n_years) + 1L)) |>
    dplyr::group_by(.data$year) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::select(-"year")

  fire_assets <- dplyr::bind_rows(recent, annual) |>
    dplyr::distinct(.data$file_name, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(.data$file_date))

  for (f in fire_assets$file_name) {
    download_envdata_asset(
      tag = ENVDATA_RELEASE_TAGS$fireage,
      file = f,
      dest_dir = temp_directory,
      overwrite = FALSE,
      max_attempts = max_attempts,
      sleep_time = sleep_time
    )
  }

  domain_path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$static,
    file = "domain.tif",
    dest_dir = temp_directory,
    overwrite = FALSE,
    max_attempts = max_attempts,
    sleep_time = sleep_time
  )
  domain <- terra::rast(domain_path)
  pid_layer <- if ("pid" %in% names(domain)) domain[["pid"]] else domain[[1]]

  all_parks <- sf::st_as_sf(protected_areas)
  park_extract <- terra::extract(
    x = pid_layer,
    y = terra::vect(all_parks),
    touches = TRUE
  )
  park_pids <- unique(stats::na.omit(park_extract[[2]]))

  fire_dataset <- arrow::open_dataset(
    sources = file.path(temp_directory, fire_assets$file_name)
  ) |>
    dplyr::filter(pid %in% park_pids) |>
    dplyr::collect() |>
    dplyr::mutate(
      days_since_fire = .data$fire_age_days,
      cellID = .data$pid # alias for any legacy report code still expecting cellID
    )

  fire_dataset
}
