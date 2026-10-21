#!/usr/bin/env Rscript
# Unit tests for multi-sensor EVI fusion (W4.0). No network required.
# Run: Rscript scripts/test_fuse_evi_sensors.R

suppressPackageStartupMessages({
  library(dplyr)
})
source("R/fuse_evi_sensors.R")

assert_equal <- function(actual, expected, label) {
  ok <- identical(actual, expected) ||
    (length(actual) == 1L && length(expected) == 1L &&
       is.na(actual) && is.na(expected)) ||
    (is.numeric(actual) && is.numeric(expected) &&
       length(actual) == length(expected) &&
       all(abs(actual - expected) < 1e-9 | (is.na(actual) & is.na(expected))))
  if (!isTRUE(ok)) {
    stop(
      label, " failed.\n  expected: ", paste(expected, collapse = ", "),
      "\n  actual:   ", paste(actual, collapse = ", ")
    )
  }
  message("PASS: ", label)
}

# --- Tabular priority fill ---------------------------------------------------

assert_equal(
  fuse_evi_sensor_values(c(4L, 3L), c(NA_real_, 0.42)),
  0.42,
  "NOAA-20 NA + S-NPP value → S-NPP"
)

assert_equal(
  fuse_evi_sensor_values(c(4L, 3L, 1L), c(NA_real_, NA_real_, 0.31)),
  0.31,
  "both VIIRS NA + Terra → Terra"
)

assert_equal(
  fuse_evi_sensor_values(c(4L, 3L, 1L, 2L), c(NA_real_, NA_real_, NA_real_, 0.22)),
  0.22,
  "all VIIRS+Terra NA + Aqua → Aqua"
)

assert_equal(
  fuse_evi_sensor_values(c(4L, 3L, 1L, 2L), rep(NA_real_, 4L)),
  NA_real_,
  "all sensors NA → NA"
)

assert_equal(
  fuse_evi_sensor_values(c(1L, 4L), c(0.10, 0.55)),
  0.55,
  "NOAA-20 preferred over Terra when both present"
)

assert_equal(
  fuse_evi_sensor_values(c(3L, 4L), c(0.33, NA_real_)),
  0.33,
  "S-NPP used when NOAA-20 NA"
)

fused <- fuse_evi_by_pid_date(
  data.frame(
    pid = c(1L, 1L, 1L, 2L, 2L),
    date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01",
                     "2020-01-01", "2020-01-01")),
    sensor = c(4L, 3L, 1L, 1L, 2L),
    value = c(NA_real_, 40, 10, NA_real_, 20)
  )
)
assert_equal(fused$value[fused$pid == 1L], 40, "pid-date fuse picks S-NPP")
assert_equal(fused$value[fused$pid == 2L], 20, "pid-date fuse Terra NA → Aqua")

# Mean after fuse: one date with dual sensors must not double-weight
raw <- data.frame(
  pid = rep(1L, 4L),
  date = as.Date(c("2020-01-01", "2020-01-01", "2020-02-01", "2020-02-01")),
  sensor = c(4L, 1L, 1L, 2L),
  value = c(100, 10, 40, NA_real_)
)
means <- fuse_evi_by_pid_date(raw) |>
  dplyr::group_by(pid) |>
  dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
# dates: Jan fused=100, Feb fused=40 → mean 70 (not (100+10+40)/3)
assert_equal(means$value[[1]], 70, "fuse-then-mean does not double-weight sensors")

# --- Raster cover path (optional if terra present) ----------------------------
if (requireNamespace("terra", quietly = TRUE)) {
  template <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2,
                          ymin = 0, ymax = 2, crs = "local")
  noaa20 <- template
  terra::values(noaa20) <- c(NA, NA, 0.5, NA)
  snpp <- template
  terra::values(snpp) <- c(0.3, NA, 0.9, NA)
  terra_r <- template
  terra::values(terra_r) <- c(0.1, 0.2, 0.1, NA)
  aqua <- template
  terra::values(aqua) <- c(NA, NA, NA, 0.05)

  fused_r <- fuse_evi_rasters(
    list(
      viirs_noaa20 = noaa20,
      viirs_snpp = snpp,
      modis_terra = terra_r,
      modis_aqua = aqua
    )
  )
  got <- as.vector(terra::values(fused_r))
  # cell1: NOAA NA, S-NPP 0.3 → 0.3
  # cell2: both VIIRS NA, Terra 0.2 → 0.2
  # cell3: NOAA 0.5 (wins over S-NPP 0.9)
  # cell4: all higher NA, Aqua 0.05
  expect <- c(0.3, 0.2, 0.5, 0.05)
  assert_equal(got, expect, "raster fuse NOAA→S-NPP→Terra→Aqua")

  all_na <- fuse_evi_rasters(
    list(
      viirs_noaa20 = terra::setValues(template, NA_real_),
      modis_terra = terra::setValues(template, NA_real_)
    )
  )
  assert_equal(
    as.vector(terra::values(all_na)),
    rep(NA_real_, 4L),
    "raster fuse all NA → NA"
  )
} else {
  message("SKIP: terra not installed — raster fuse tests skipped")
}

# --- Monthly parquet month filter (no network) --------------------------------
source("R/envdata_contract.R")
# Stub VIIRS release lookup so list_vi_parquet_assets stays offline.
list_release_assets <- function(...) {
  data.frame(
    tag = character(),
    file_name = character(),
    stringsAsFactors = FALSE
  )
}
source("R/get_monthly_mean_evi.R")

# Synthetic multi-year archive: filter must drop non-target months.
dates <- seq.Date(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month")
fake_env <- data.frame(
  tag = ENVDATA_RELEASE_TAGS$vi_modis_parquet,
  file_name = sprintf("vi_modis_%s.parquet", format(dates, "%Y%m%d")),
  stringsAsFactors = FALSE
)
sept <- list_vi_parquet_assets(env_files = fake_env, current_month = 9L)
stopifnot(nrow(sept) >= 1L)
stopifnot(all(as.integer(lubridate::month(sept$file_date)) == 9L))
stopifnot(length(unique(sept$file_date)) < length(unique(dates)))
# NULL month = all dates (long-term path)
all_dates <- list_vi_parquet_assets(env_files = fake_env, current_month = NULL)
stopifnot(nrow(all_dates) == length(dates))
message("PASS: monthly VI parquet month filter")

# Filename vs internal-date alignment (corrupt historical duplicates).
stopifnot(isTRUE(vi_parquet_content_matches_file_date(
  as.Date(c("2020-08-28", "2020-09-04", "2020-09-12")),
  as.Date("2020-09-04")
)))
stopifnot(!isTRUE(vi_parquet_content_matches_file_date(
  as.Date(c("2000-07-19", "2000-08-01", "2000-08-11")),
  as.Date("2000-09-13")
)))
stopifnot(!isTRUE(vi_parquet_content_matches_file_date(
  as.Date(c("2020-07-19", "2020-08-11")),
  as.Date("2020-01-08")
)))
message("PASS: vi_parquet_content_matches_file_date")

# Multi-date accumulate must count each (pid, date) once (no duplicate-key loss).
acc0 <- accumulate_fused_evi_means(
  numeric(0),
  numeric(0),
  data.frame(
    pid = c(1L, 1L, 2L),
    date = as.Date(c("2020-09-01", "2020-09-02", "2020-09-01")),
    value = c(10, 30, 40)
  )
)
stopifnot(identical(unname(acc0$sum_vec[c("1", "2")]), c(40, 40)))
stopifnot(identical(unname(acc0$count_vec[c("1", "2")]), c(2, 1)))
message("PASS: accumulate_fused_evi_means")

message("All fuse_evi_sensors tests passed.")
