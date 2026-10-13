#' Join VI + fireage parquet directories into model-style dynamic rows
#'
#' Legacy input was long `dynamic_parquet-*` files with variable/value columns.
#' New contract: vi_* parquet (pid, date, sensor, value) left-joined to
#' fireage_* parquet on (pid, date, sensor).
tidy_dynamic_data <- function(data, date_window, envdata_dir = "data/envdata") {

  vi_paths <- list.files(
    envdata_dir,
    pattern = "^vi_(modis|viirs)_.*\\.parquet$",
    full.names = TRUE
  )
  fire_paths <- list.files(
    envdata_dir,
    pattern = "^fireage_(modis|viirs)_.*\\.parquet$",
    full.names = TRUE
  )
  if (length(vi_paths) == 0L) {
    stop("No VI parquet files found under ", envdata_dir)
  }

  cells_to_keep <- data |>
    dplyr::filter(.data$sample) |>
    dplyr::select(pid = dplyr::any_of(c("pid", "cellID"))) |>
    dplyr::rename(pid = 1)

  start_date <- as.numeric(lubridate::as_date(date_window[1]))
  stop_date <- as.numeric(lubridate::as_date(date_window[2]))

  vi_ds <- arrow::open_dataset(sources = vi_paths) |>
    dplyr::semi_join(cells_to_keep, by = "pid") |>
    dplyr::filter(date > start_date & date < stop_date)

  if (length(fire_paths) > 0L) {
    fire_ds <- arrow::open_dataset(sources = fire_paths) |>
      dplyr::select(
        "pid", "date", "sensor",
        "last_burn_date", "fire_age_days"
      )
    joined <- vi_ds |>
      dplyr::left_join(fire_ds, by = c("pid", "date", "sensor"))
  } else {
    joined <- vi_ds |>
      dplyr::mutate(
        last_burn_date = NA_integer_,
        fire_age_days = NA_integer_
      )
  }

  dyndata <- joined |>
    dplyr::collect() |>
    dplyr::mutate(
      cellID = .data$pid,
      firemonth = ifelse(
        is.na(.data$last_burn_date) | .data$last_burn_date == 0,
        NA_integer_,
        lubridate::month(lubridate::as_date(.data$last_burn_date))
      ),
      date = lubridate::as_date(.data$date),
      evi = scale_evi_integer(.data$value), # vegetation index column
      age = .data$fire_age_days / 365.23
    ) |>
    tibble::as_tibble()

  dyndata
}
