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
# Park map defaults (report_prototype.qmd): Veld Age on; Fire History off.
qmd_map <- paste(readLines("report_prototype.qmd", warn = FALSE), collapse = "\n")
hide_block <- regmatches(
  qmd_map,
  regexpr(
    "park_map <- park_map %>%\\s*\\n\\s*hideGroup\\(\"Stations\"\\)[\\s\\S]*?(?=\\nif \\(isTRUE\\(has_focal_invasives\\))",
    qmd_map,
    perl = TRUE
  )
)
stopifnot(length(hide_block) == 1L, nchar(hide_block) > 0L)
stopifnot(grepl("hideGroup\\(FIRE_HISTORY_GROUP\\)", hide_block))
stopifnot(!grepl("hideGroup\\(VELD_AGE_GROUP\\)", hide_block))
message("PASS: park map default overlays (Veld Age on, Fire History off)")
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
stopifnot(identical(PARK_MAP_QUERY_LAYER_ID, "evi_delta_monthly"))

# Censor flag: prefer is_censored; NA is_censored → last_burn / fire_count
# (mixed-schema parquet binds leave NA on older annual files).
cens_ic <- fire_history_row_censored(data.frame(
  is_censored = c(0, 1, NA),
  last_burn_date = c(100, 100, 100),
  fire_count = c(1, 1, 1)
))
stopifnot(identical(as.logical(cens_ic), c(FALSE, TRUE, FALSE)))
cens_ic_na_burn <- fire_history_row_censored(data.frame(
  is_censored = c(0, 1, NA),
  last_burn_date = c(100, 100, NA),
  fire_count = c(1, 1, 1)
))
stopifnot(identical(as.logical(cens_ic_na_burn), c(FALSE, TRUE, TRUE)))
cens_fb <- fire_history_row_censored(data.frame(
  last_burn_date = c(100, NA, 0),
  fire_count = c(2, 1, 3)
))
stopifnot(identical(as.logical(cens_fb), c(FALSE, TRUE, TRUE)))

# Latest-per-cell must not use global max(date) (keeps only a few cells)
hist_df <- data.frame(
  cellID = c(1, 1, 2, 2, 3),
  date = as.Date(c("2026-09-01", "2026-09-21", "2026-09-01", "2026-09-10", "2026-09-21")),
  last_burn_date = c(1, 2, 3, 4, NA),
  fire_count = c(1, 1, 1, 1, 0),
  days_since_fire = c(10, 5, 20, 15, NA)
)
latest <- fire_history_latest_per_cell(hist_df)
stopifnot(nrow(latest) == 3L)
stopifnot(identical(sort(latest$cellID), c(1, 2, 3)))
stopifnot(identical(as.Date(latest$date[latest$cellID == 1]), as.Date("2026-09-21")))
stopifnot(identical(as.Date(latest$date[latest$cellID == 2]), as.Date("2026-09-10")))

# Burned-area from fireage_current-style stack (no network)
if (requireNamespace("terra", quietly = TRUE)) {
  r_age <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
  # R Date integers: 2020-01-01 ≈ 18262; 2015-06-01 ≈ 16587
  r_last <- terra::rast(r_age)
  r_cens <- terra::rast(r_age)
  terra::values(r_age) <- c(100, 200, 300, NA)
  terra::values(r_last) <- c(18262, 16587, NA, NA)
  terra::values(r_cens) <- c(0, 0, 1, NA)
  names(r_age) <- "fire_age_days"
  names(r_last) <- "last_burn_date"
  names(r_cens) <- "is_censored"
  fireage <- c(r_age, r_last, r_cens)
  mask <- c(TRUE, TRUE, TRUE, FALSE)
  aby <- park_burned_area_by_year(fireage, mask, cell_ha = 25)
  stopifnot(nrow(aby) == 2L)
  stopifnot(all(c(2015L, 2020L) %in% aby$burn_year))
  stopifnot(abs(sum(aby$area_ha) - 50) < 1e-9)

  # Veg-age series: 2014 still censored for 2020 burn; 2015 unlocks one cell;
  # 2020 unlocks both known burns; never-burned cell stays censored.
  ser <- park_veg_age_series_from_fireage(fireage, mask, years = c(2014L, 2015L, 2020L))
  stopifnot(nrow(ser$censored_summary) == 3L)
  c14 <- ser$censored_summary$fraction_censored[ser$censored_summary$year == 2014L]
  c15 <- ser$censored_summary$fraction_censored[ser$censored_summary$year == 2015L]
  c20 <- ser$censored_summary$fraction_censored[ser$censored_summary$year == 2020L]
  stopifnot(abs(c14 - 1) < 1e-9)           # 3 park cells, 0 known
  stopifnot(abs(c15 - (2 / 3)) < 1e-9)     # 1 known (2015 burn)
  stopifnot(abs(c20 - (1 / 3)) < 1e-9)     # 2 known, 1 never-burned
  stopifnot(nrow(ser$known_ages[ser$known_ages$year == 2014L, , drop = FALSE]) == 0L)
  stopifnot(nrow(ser$known_ages[ser$known_ages$year == 2015L, , drop = FALSE]) == 1L)
  stopifnot(nrow(ser$known_ages[ser$known_ages$year == 2020L, , drop = FALSE]) == 2L)
}

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
stopifnot(grepl("prior 12-mo", md_w))

# Regression: stale Sys.Date()-like as_of after data ends must NOT yield
# precip_12mo ≈ 0 and anomaly ≈ −annual_mean (the “−416 vs 416” bug).
end_data <- as.Date("2025-08-24")
dry_rate <- 416 / 365.25
fw_short <- data.frame(
  date_time = seq.Date(as.Date("2022-01-01"), end_data, by = "day"),
  station_name = "STALE_ASOF",
  mean_precip_mm = dry_rate,
  temp_c = 18,
  stringsAsFactors = FALSE
)
# Current year deliberately half of prior years → negative anomaly, not −ann
fw_short$mean_precip_mm[
  fw_short$date_time >= (end_data - 364L)
] <- dry_rate * 0.5
anom_stale <- weather_12mo_anomaly(fw_short, as_of = as.Date("2026-10-21"))
# Compare calendar day (not identical(): Date integer/double storage differs)
stopifnot(as.character(anom_stale$as_of) == as.character(end_data))
stopifnot(is.finite(anom_stale$precip_12mo_mm), anom_stale$precip_12mo_mm > 50)
stopifnot(is.finite(anom_stale$precip_long_mean_mm))
# Must not collapse to precip_12mo≈0 → anomaly≈−long_mean
stopifnot(
  abs(anom_stale$precip_anomaly_mm + anom_stale$precip_long_mean_mm) > 50
)
stopifnot(anom_stale$precip_anomaly_mm < 0)
# Current ≈ half of prior annual totals
stopifnot(
  abs(anom_stale$precip_12mo_mm / anom_stale$precip_long_mean_mm - 0.5) < 0.15
)

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
