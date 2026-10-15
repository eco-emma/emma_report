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
  message(
    "Long-term mean EVI fuse-then-mean using: ",
    paste(families, collapse = "+"),
    " (", nrow(vi_files), " parquet files)"
  )

  evi_mean_from_parquet_files(vi_files, temp_directory = temp_directory)
}
