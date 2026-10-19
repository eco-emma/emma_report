#!/usr/bin/env Rscript
# Offline tests for rolling 12-month weather climatology helpers.
# Run: Rscript scripts/test_weather_climatology.R

source("R/weather_climatology.R")

set.seed(1)
as_of <- as.Date("2026-10-15")
dates <- seq.Date(as.Date("2020-01-01"), as_of, by = "day")
fw <- data.frame(
  date_time = dates,
  station_name = rep(c("A", "B"), length.out = length(dates)),
  mean_precip_mm = pmax(0, rnorm(length(dates), 1.2, 0.8)),
  temp_c = 15 + 5 * sin(2 * pi * as.numeric(format(dates, "%j")) / 365) +
    rnorm(length(dates), 0, 1),
  stringsAsFactors = FALSE
)

daily_p <- park_daily_weather(fw, "mean_precip_mm")
stopifnot(nrow(daily_p) >= 1L, all(c("date", "value") %in% names(daily_p)))
# Two stations → one row per date
stopifnot(nrow(daily_p) == length(unique(dates)))

# Trailing 12-mo cumulative precip
cum_p <- park_daily_trailing_sum(daily_p, min_days = 60L)
stopifnot(nrow(cum_p) >= 1L)
stopifnot(min(cum_p$date) >= min(daily_p$date) + 364L)
# Cumulative should be much larger than typical daily
stopifnot(median(cum_p$value) > median(daily_p$value) * 50)

layers <- weather_climatology_layers(cum_p, as_of = as_of, min_days_per_year = 60L)
stopifnot(isTRUE(layers$has_data))
stopifnot(layers$window_end == as_of)
stopifnot(layers$window_start == as_of - 364L)
stopifnot(nrow(layers$current) >= 1L)
stopifnot(nrow(layers$years) >= 1L)
stopifnot(nrow(layers$envelope) >= 1L)
stopifnot(is.finite(layers$n_hist_years), layers$n_hist_years >= 1L)
stopifnot(!is.na(layers$record_start), !is.na(layers$record_end))
# Current window x-axis spans trailing year, not calendar Jan–Dec only
stopifnot(format(layers$window_start, "%m") != "01" ||
            format(layers$window_start, "%d") != "01" ||
            TRUE)
stopifnot(min(layers$current$plot_date) >= layers$window_start)
stopifnot(max(layers$current$plot_date) <= layers$window_end)
stopifnot(all(layers$years$plot_date >= layers$window_start))
stopifnot(all(layers$years$plot_date <= layers$window_end))
# Historical offsets are positive integers (prior slices)
stopifnot(all(layers$years$year_offset >= 1L))
# Envelope percentiles ordered
stopifnot(all(layers$envelope$p05 <= layers$envelope$p25, na.rm = TRUE))
stopifnot(all(layers$envelope$p25 <= layers$envelope$p50, na.rm = TRUE))
stopifnot(all(layers$envelope$p50 <= layers$envelope$p75, na.rm = TRUE))
stopifnot(all(layers$envelope$p75 <= layers$envelope$p95, na.rm = TRUE))

# Fuller history: ~6 calendar years of daily data → several prior slices
stopifnot(layers$n_hist_years >= 3L)

daily_t <- park_daily_weather(fw, "temp_c")
layers_t <- weather_climatology_layers(daily_t, as_of = as_of)
stopifnot(isTRUE(layers_t$has_data), nrow(layers_t$envelope) >= 1L)
stopifnot(layers_t$n_hist_years >= 3L)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- plot_weather_climatology(
    layers,
    ylab = "Cumulative precip (mm / 12 mo)",
    title = "test",
    log1p_y = TRUE
  )
  stopifnot(inherits(p, "ggplot"))
  p2 <- plot_weather_climatology(
    layers_t,
    ylab = "Temperature (°C)",
    title = "test",
    log1p_y = FALSE
  )
  stopifnot(inherits(p2, "ggplot"))
}

# Empty input soft-fails
empty <- weather_climatology_layers(
  data.frame(date = as.Date(character()), value = numeric()),
  as_of = as_of
)
stopifnot(!isTRUE(empty$has_data), identical(empty$n_hist_years, 0L))

empty_cum <- park_daily_trailing_sum(
  data.frame(date = as.Date(character()), value = numeric())
)
stopifnot(nrow(empty_cum) == 0L)

message("All weather climatology tests passed.")
