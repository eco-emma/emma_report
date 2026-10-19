#!/usr/bin/env Rscript
# Lightweight tests for park manager KPI helpers (no network, no geotargets).
# Run: Rscript scripts/test_park_manager_kpis.R

source("R/envdata_contract.R")
source("R/coverage_metrics.R")
source("R/park_manager_kpis.R")

# --- CV / stress flag --------------------------------------------------------
stopifnot(is.na(cv_anomaly_series(c(1, 2))))
cv <- cv_anomaly_series(c(-0.2, -0.1, 0, 0.1, 0.2, -0.05, 0.05))
stopifnot(is.finite(cv), cv > 0)

# Strong brown tail → flag ON with k=0.25, N=3
series_flag <- c(0.05, 0.02, -0.01, 0.0, -0.4, -0.45, -0.5)
st <- evaluate_stress_flag(series_flag, k = 0.25, n = 3L)
stopifnot(isTRUE(st$flagged), is.finite(st$threshold), st$threshold < 0)
stopifnot(st$n_below == 3L)

# Greening-only → never flag
st_green <- evaluate_stress_flag(c(0.1, 0.2, 0.15, 0.3), k = 0.25, n = 3L)
stopifnot(!isTRUE(st_green$flagged))

# --- Fire age bins -----------------------------------------------------------
stopifnot(identical(fire_outline_age_bin(2), "Burns <5 y"))
stopifnot(identical(fire_outline_age_bin(7), "Burns 5–10 y"))
stopifnot(identical(fire_outline_age_bin(15), "Burns 10–20 y"))
stopifnot(identical(fire_outline_age_bin(25), "Burns >20 y"))
stopifnot("Burns <5 y" %in% FIRE_OUTLINE_GROUPS)

# --- Invasive watch list -----------------------------------------------------
watch <- invasive_watch_list(data.frame(
  scientific_name = c("Acacia saligna", "Acacia mearnsii", "Pinus pinaster"),
  observed_on = as.Date(c("2026-01-01", "2026-06-01", "2025-12-01")),
  stringsAsFactors = FALSE
))
stopifnot(nrow(watch) == 2L)
stopifnot(watch$genus[[1]] == "Acacia", watch$n[[1]] == 2L)
stopifnot(watch$newest_obs[watch$genus == "Acacia"] == as.Date("2026-06-01"))

md_watch <- format_invasive_watch_md(watch)
stopifnot(grepl("Acacia", md_watch), grepl("Caveats", md_watch))
stopifnot(!grepl("pressure score", md_watch, ignore.case = TRUE) ||
            grepl("Not a systematic survey or clearing priority score", md_watch))

# --- Weather anomaly ---------------------------------------------------------
set.seed(1)
dates <- seq.Date(as.Date("2020-01-01"), as.Date("2026-09-01"), by = "day")
fw <- data.frame(
  date_time = dates,
  station_name = "TEST_STN",
  mean_precip_mm = abs(rnorm(length(dates), 1, 0.5)),
  temp_c = rnorm(length(dates), 18, 3),
  stringsAsFactors = FALSE
)
anom <- weather_12mo_anomaly(fw, as_of = as.Date("2026-09-01"))
stopifnot(isTRUE(anom$has_weather))
stopifnot(is.finite(anom$precip_anomaly_mm), is.finite(anom$temp_anomaly_c))
md_w <- format_weather_anomaly_md(anom)
stopifnot(grepl("Rainfall", md_w), grepl("TEST_STN", md_w))

# --- KPI / age-class markdown ------------------------------------------------
st_na <- evaluate_stress_flag(numeric(0))
md_kpi <- format_delta_evi_kpi_md(-0.042, st_na, evi_as_of = as.Date("2026-09-21"))
stopifnot(grepl("Park-mean monthly", md_kpi), grepl("-0.042", md_kpi))

age <- list(
  pct_lt5 = 10, pct_5_10 = 20, pct_10_20 = 30, pct_gt20 = 40, pct_censored = 55
)
md_age <- format_age_class_md(age)
stopifnot(grepl("Age-class structure", md_age), grepl("censored", md_age))

# --- Monthly delta series from EVI means -------------------------------------
evi_s <- data.frame(
  date = as.Date(c("2024-01-15", "2024-02-15", "2025-01-15", "2025-02-15")),
  park_mean_evi = c(0.30, 0.28, 0.25, 0.27)
)
d_s <- park_mean_monthly_delta_series(evi_s)
stopifnot(nrow(d_s) == 4L, all(c("monthly_delta") %in% names(d_s)))
# Jan clim = mean(0.30, 0.25) = 0.275 → deltas 0.025, -0.025
stopifnot(abs(d_s$monthly_delta[d_s$date == as.Date("2024-01-15")] - 0.025) < 1e-9)

message("All park manager KPI tests passed.")
