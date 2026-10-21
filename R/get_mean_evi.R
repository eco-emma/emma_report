library(terra)

#' Long-term mean EVI across all available composite dates (fuse-then-mean)
#'
#' Report-side climatology from `vi_modis_parquet` (+ `vi_viirs_parquet` when
#' published). Per `(pid, date)` priority-fill NOAA-20 → S-NPP → Terra → Aqua,
#' then `mean(..., na.rm = TRUE)` across dates. Uses shared helpers in
#' `R/get_monthly_mean_evi.R` (`list_vi_parquet_assets`,
#' `evi_mean_from_parquet_files`).
get_mean_evi.tif <- function(env_files,
                             temp_directory = "data/temp/") {
  vi_files <- list_vi_parquet_assets(
    env_files = env_files,
    current_month = NULL
  )

  if (nrow(vi_files) == 0L) {
    stop(
      "No vi_*_YYYYMMDD.parquet assets found on tags ",
      ENVDATA_RELEASE_TAGS$vi_modis_parquet,
      " / ",
      ENVDATA_RELEASE_TAGS$vi_viirs_parquet
    )
  }

  families <- unique(vi_files$family)
  n_dates <- length(unique(vi_files$file_date))
  n_months <- length(unique(as.integer(lubridate::month(vi_files$file_date))))
  message(
    "Long-term mean EVI fuse-then-mean using: ",
    paste(families, collapse = "+"),
    " (", nrow(vi_files), " parquet files, ",
    n_dates, " composite dates across ", n_months, " calendar months; ",
    "longterm_climatology_v3_content_align)"
  )
  if (n_months < 2L && n_dates >= 24L) {
    warning(
      "get_mean_evi.tif(): expected multi-month long-term climatology but only ",
      n_months, " calendar month(s) present in parquet list",
      call. = FALSE
    )
  }

  evi_mean_from_parquet_files(
    vi_files,
    temp_directory = temp_directory,
    current_month = NULL
  )
}
