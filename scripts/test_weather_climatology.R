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

# In-window cumulative starts at 0 and is non-decreasing
win <- seq.Date(as_of - 364L, as_of, by = "day")
vbd <- stats::setNames(daily_p$value, as.character(daily_p$date))
cum_win <- park_window_cumsum(win, vbd, min_days = 60L)
stopifnot(!is.null(cum_win), nrow(cum_win) == length(win) + 1L)
stopifnot(isTRUE(all.equal(cum_win$value[[1]], 0)))
stopifnot(all(diff(cum_win$value) >= -1e-9))
# Final value = sum of daily precip in window (NA days as 0)
filled <- as.numeric(vbd[as.character(win)])
filled[!is.finite(filled)] <- 0
stopifnot(isTRUE(all.equal(cum_win$value[[nrow(cum_win)]], sum(filled))))
# Mid-window pulse still ends at the pulse total
zero_dates <- seq.Date(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "day")
zero_vbd <- stats::setNames(rep(0, length(zero_dates)), as.character(zero_dates))
zero_vbd[as.character(as.Date("2019-06-01"))] <- 10
zcum <- park_window_cumsum(zero_dates, zero_vbd, min_days = 60L)
stopifnot(isTRUE(all.equal(zcum$value[[1]], 0)))
stopifnot(max(zcum$value) == 10)

layers <- weather_climatology_layers(
  daily_p,
  as_of = as_of,
  min_days_per_year = 60L,
  cumulative = TRUE
)
stopifnot(isTRUE(layers$has_data))
stopifnot(isTRUE(layers$cumulative))
stopifnot(layers$window_end == as_of)
stopifnot(layers$window_start == as_of - 364L)
stopifnot(nrow(layers$current) >= 1L)
stopifnot(nrow(layers$years) >= 1L)
stopifnot(nrow(layers$envelope) >= 1L)
stopifnot(is.finite(layers$n_hist_years), layers$n_hist_years >= 1L)
stopifnot(!is.na(layers$record_start), !is.na(layers$record_end))
stopifnot(isTRUE(all.equal(layers$current$value[[1]], 0)))
stopifnot(all(diff(layers$current$value) >= -1e-9))
# Historical grey years are also in-window cumulative from 0
by_off <- split(layers$years$value, layers$years$year_offset)
stopifnot(all(vapply(by_off, function(v) {
  isTRUE(all.equal(v[[1]], 0)) && all(diff(v) >= -1e-9)
}, logical(1))))
stopifnot(min(layers$current$plot_date) >= layers$window_start)
stopifnot(max(layers$current$plot_date) <= layers$window_end)
stopifnot(all(layers$years$plot_date >= layers$window_start))
stopifnot(all(layers$years$plot_date <= layers$window_end))
stopifnot(all(layers$years$year_offset >= 1L))
stopifnot(all(layers$envelope$p05 <= layers$envelope$p25, na.rm = TRUE))
stopifnot(all(layers$envelope$p25 <= layers$envelope$p50, na.rm = TRUE))
stopifnot(all(layers$envelope$p50 <= layers$envelope$p75, na.rm = TRUE))
stopifnot(all(layers$envelope$p75 <= layers$envelope$p95, na.rm = TRUE))
stopifnot(layers$n_hist_years >= 3L)

daily_t <- park_daily_weather(fw, "temp_c")
layers_t <- weather_climatology_layers(
  daily_t,
  as_of = as_of,
  cumulative = FALSE,
  smooth_days = 11L
)
stopifnot(isTRUE(layers_t$has_data), nrow(layers_t$envelope) >= 1L)
stopifnot(layers_t$n_hist_years >= 3L)
stopifnot(identical(as.integer(layers_t$smooth_days), 11L))
# Temperature is daily, not forced to start at 0
stopifnot(!isTRUE(all.equal(layers_t$current$value[[1]], 0)))
# Envelope smoothing: median should be less jagged than unsmoothed
raw_t <- weather_climatology_layers(
  daily_t,
  as_of = as_of,
  cumulative = FALSE,
  smooth_days = 1L
)
rough <- sum(abs(diff(raw_t$envelope$p50)), na.rm = TRUE)
smooth <- sum(abs(diff(layers_t$envelope$p50)), na.rm = TRUE)
stopifnot(smooth < rough)
# Rolling helper sanity
stopifnot(identical(park_roll_mean(1:5, window = 1L), as.numeric(1:5)))
stopifnot(length(park_roll_mean(1:11, window = 11L)) == 11L)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- plot_weather_climatology(
    layers,
    ylab = "Cumulative precip (mm)",
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
  # Combined two-panel: shared x window + cowplot stack
  stopifnot(identical(layers$window_start, layers_t$window_start))
  stopifnot(identical(layers$window_end, layers_t$window_end))
  if (requireNamespace("cowplot", quietly = TRUE)) {
    combo <- plot_weather_climatology_combo(layers, layers_t)
    stopifnot(!is.null(combo))
  }
  # Shared x-scale helper returns a Scale object
  xs <- weather_climatology_x_scale(layers$window_start, layers$window_end)
  stopifnot(inherits(xs, "Scale"))
}

# Empty input soft-fails
empty <- weather_climatology_layers(
  data.frame(date = as.Date(character()), value = numeric()),
  as_of = as_of,
  cumulative = TRUE
)
stopifnot(!isTRUE(empty$has_data), identical(empty$n_hist_years, 0L))

empty_cum <- park_window_cumsum(
  as.Date(character()),
  stats::setNames(numeric(), character())
)
stopifnot(is.null(empty_cum))

message("All weather climatology tests passed.")
