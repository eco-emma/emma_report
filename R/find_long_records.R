#' Find pixels with long fire-age records and dense VI observations
#'
#' Updated for fireage_* / vi_* parquet naming and `pid` join key.
find_long_records <- function(env_files,
                              age_range = c(2, 10),
                              evi_prob = 0.8,
                              envdata_dir = "data/envdata") {

  age_paths <- list.files(
    envdata_dir,
    pattern = "^fireage_.*\\.parquet$",
    full.names = TRUE
  )
  vi_paths <- list.files(
    envdata_dir,
    pattern = "^vi_.*\\.parquet$",
    full.names = TRUE
  )
  if (length(age_paths) == 0L || length(vi_paths) == 0L) {
    stop(
      "Expected fireage_*.parquet and vi_*.parquet under ",
      envdata_dir,
      ". Run sync_envdata() or download those release tags first."
    )
  }

  age_files <- arrow::open_dataset(sources = age_paths)
  vi_files <- arrow::open_dataset(sources = vi_paths)

  long_aged_pixels <- age_files |>
    dplyr::filter(
      !is.na(fire_age_days),
      fire_age_days > age_range[1] * 365,
      fire_age_days > age_range[2] * 365
    ) |>
    dplyr::collect() |>
    dplyr::group_by(pid) |>
    dplyr::summarise(n_age = dplyr::n(), .groups = "drop") |>
    tibble::as_tibble()

  good_vi_pixels <- vi_files |>
    dplyr::filter(!is.na(value)) |>
    dplyr::collect() |>
    tibble::as_tibble() |>
    dplyr::group_by(pid) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  max_number_vi <- max(good_vi_pixels$n, na.rm = TRUE)

  keep_pixels <- long_aged_pixels |>
    dplyr::left_join(good_vi_pixels, by = "pid") |>
    dplyr::filter(.data$n > (max_number_vi * evi_prob)) |>
    dplyr::transmute(cellID = .data$pid, pid = .data$pid)

  keep_pixels
}
