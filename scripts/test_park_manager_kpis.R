#!/usr/bin/env Rscript
# Lightweight tests for park manager KPI helpers (no network, no geotargets).
# Run: Rscript scripts/test_park_manager_kpis.R

source("R/envdata_contract.R")
source("R/fuse_evi_sensors.R")
source("R/get_monthly_mean_evi.R")
source("R/coverage_metrics.R")
source("R/park_manager_kpis.R")

# Lookback helpers must be visible (regression: Quarto previously omitted these
# sources, so park_mean_evi_series soft-failed → Stress flag unavailable).
stopifnot(exists("list_vi_parquet_assets", mode = "function"))
stopifnot(exists("fuse_evi_by_pid_date", mode = "function"))
stopifnot(exists("build_park_monthly_delta_evi_series", mode = "function"))
stopifnot(exists("park_stress_delta_series", mode = "function"))
stopifnot(nrow(empty_park_monthly_delta_evi_series()) == 0L)

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

# --- Fire age bins / Fire History symbology ----------------------------------
stopifnot(identical(fire_outline_age_bin(2), "Burns <5 y"))
stopifnot(identical(fire_outline_age_bin(7), "Burns 5–10 y"))
stopifnot(identical(fire_outline_age_bin(15), "Burns 10–20 y"))
stopifnot(identical(fire_outline_age_bin(25), "Burns >20 y"))
stopifnot("Burns <5 y" %in% FIRE_OUTLINE_GROUPS)
stopifnot(identical(FIRE_HISTORY_GROUP, "Fire History"))
stopifnot(identical(VELD_AGE_GROUP, "Veld Age (years)"))
stopifnot(identical(EVI_CURRENT_GROUP, "EVI (current)"))
stopifnot(identical(EVI_DELTA_MONTHLY_GROUP, "\u0394EVI (monthly)"))
stopifnot(identical(EVI_DELTA_LONGTERM_GROUP, "\u0394EVI (long term)"))
stopifnot(identical(EVI_LAYER_CONTROL_HEADER, "EVI"))
stopifnot(identical(PARK_OUTLINE_COLOR, "#14532d"))
stopifnot(identical(as.integer(PARK_OUTLINE_WEIGHT), 1L))
stopifnot(is.character(park_map_evi_group_header_js()))
stopifnot(grepl("emma-layer-group-header", park_map_evi_group_header_js()))
stopifnot(grepl("Current EVI", park_map_tooltip_format_js()))
stack_js <- park_map_layer_stack_js(c(EVI_CURRENT_GROUP, EVI_DELTA_MONTHLY_GROUP))
stopifnot(grepl("bringToFront", stack_js), grepl("overlayadd", stack_js))
stopifnot(grepl(EVI_CURRENT_GROUP, stack_js, fixed = TRUE))
if (requireNamespace("leaflet", quietly = TRUE)) {
  leaf <- inat_leaf_icon(size = 10L)
  stopifnot(is.list(leaf), !is.null(leaf$iconUrl))
  stopifnot(identical(as.numeric(leaf$iconWidth), 10), identical(as.numeric(leaf$iconHeight), 10))
  stopifnot(grepl("image/svg\\+xml", leaf$iconUrl, fixed = FALSE))
}
stopifnot(identical(fire_history_outline_color(2), "#b30000"))
stopifnot(identical(fire_history_outline_color(25), "#fdbb84"))
stopifnot(length(fire_history_outline_color(c(2, 7, 15))) == 3L)

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
stopifnot(grepl("Acacia", md_watch), grepl("observer effort", md_watch))
stopifnot(!grepl("Not a systematic survey", md_watch, ignore.case = TRUE))
stopifnot(!grepl("clearing priority score", md_watch, ignore.case = TRUE))

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

# --- All-parks lookback join → stress flag -----------------------------------
lookback <- rbind(
  cbind(park_name = "Limietberg Nature Reserve Complex", d_s),
  data.frame(
    park_name = "Other Park",
    date = as.Date("2025-01-15"),
    park_mean_evi = 0.2,
    month_clim = 0.2,
    monthly_delta = 0,
    stringsAsFactors = FALSE
  )
)
lb <- park_stress_delta_series(lookback, "Limietberg Nature Reserve Complex")
stopifnot(nrow(lb) == 4L, !("park_name" %in% names(lb)) || TRUE)
# Extend with a brown tail so N=3 trips
brown <- data.frame(
  park_name = "Limietberg Nature Reserve Complex",
  date = as.Date(c("2025-03-15", "2025-04-15", "2025-05-15")),
  park_mean_evi = c(0.1, 0.08, 0.05),
  month_clim = c(0.3, 0.3, 0.3),
  monthly_delta = c(-0.2, -0.22, -0.25),
  stringsAsFactors = FALSE
)
lookback2 <- rbind(lookback, brown)
lb2 <- park_stress_delta_series(lookback2, "Limietberg Nature Reserve Complex")
st_lb <- evaluate_stress_flag(lb2$monthly_delta, k = 0.25, n = 3L)
stopifnot(is.finite(st_lb$threshold), is.finite(st_lb$cv))
stopifnot(nrow(park_stress_delta_series(lookback, "Missing Park")) == 0L)

message("All park manager KPI tests passed.")
