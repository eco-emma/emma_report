# Rolling 12-month climatology envelope plots for park weather
# (GSOD via emma_envdata `weather_data`).
#
# Alignment: prior 12-month slices, calendar-shifted onto the current
# trailing window that ends at the latest available weather date. Not a
# fixed Jan–Dec calendar-year / DOY axis.
#
# Precipitation: true in-window cumulative sum starting at 0 at the window
# start and summing forward (not a trailing 365-day rolling sum).
#
# Envelope smoothing: after stacking prior years, median / percentile ribbons
# are rolled with a centred ~11-day mean (see `smooth_days`) to damp day-of-
# window noise. The current-year (red) series stays daily unless callers smooth
# it separately.

#' Centred rolling mean for a numeric vector (NA-aware).
#'
#' @param x numeric
#' @param window odd integer window length (even values are bumped up by 1)
#' @return numeric same length as `x`
park_roll_mean <- function(x, window = 11L) {
  x <- as.numeric(x)
  n <- length(x)
  w <- as.integer(window)[[1]]
  if (!is.finite(w) || w < 1L || n < 1L) {
    return(x)
  }
  if (w %% 2L == 0L) {
    w <- w + 1L
  }
  if (w == 1L || n < w) {
    return(x)
  }
  half <- (w - 1L) %/% 2L
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - half)
    hi <- min(n, i + half)
    chunk <- x[lo:hi]
    chunk <- chunk[is.finite(chunk)]
    if (length(chunk)) {
      out[[i]] <- mean(chunk)
    }
  }
  out
}

#' Smooth climatology envelope percentile columns with a rolling mean.
#'
#' @param envelope tibble with `plot_date` and `p05`…`p95`
#' @param smooth_days window length (default 11)
#' @return envelope with smoothed percentile columns
smooth_climatology_envelope <- function(envelope, smooth_days = 11L) {
  if (!is.data.frame(envelope) || nrow(envelope) < 1L) {
    return(envelope)
  }
  env <- envelope
  env <- env[order(env$plot_date), , drop = FALSE]
  for (col in c("p05", "p25", "p50", "p75", "p95")) {
    if (col %in% names(env)) {
      env[[col]] <- park_roll_mean(env[[col]], window = smooth_days)
    }
  }
  env
}

#' Station-mean daily series for one weather variable.
#'
#' @param focal_weather data.frame with `date_time`, `station_name`, and value column
#' @param value_col character name of the numeric column (`mean_precip_mm` or `temp_c`)
#' @return tibble with `date`, `value` (mean across stations per day)
park_daily_weather <- function(focal_weather, value_col) {
  empty <- tibble::tibble(
    date = as.Date(character()),
    value = numeric()
  )
  if (!is.data.frame(focal_weather) || nrow(focal_weather) < 1L) {
    return(empty)
  }
  need <- c("date_time", value_col)
  if (!all(need %in% names(focal_weather))) {
    return(empty)
  }
  df <- focal_weather
  df$date <- as.Date(df$date_time)
  df$value <- as.numeric(df[[value_col]])
  df <- df[is.finite(df$value) & !is.na(df$date), , drop = FALSE]
  if (!nrow(df)) {
    return(empty)
  }
  df |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$date)
}

#' In-window cumulative sum for one daily series on a fixed date grid.
#'
#' Starts at 0 on the first day of `ref_dates`, then sums forward. Days with no
#' observation contribute 0 (GSOD often omits zero-precip days). Returns NULL
#' when fewer than `min_days` finite daily values fall in the window.
#'
#' @param ref_dates Date vector (window calendar)
#' @param value_by_date named numeric vector (names = `as.character(date)`)
#' @param min_days minimum observed days required
#' @return tibble with `plot_date`, `value`, or NULL
park_window_cumsum <- function(ref_dates, value_by_date, min_days = 60L) {
  ref_dates <- as.Date(ref_dates)
  if (!length(ref_dates)) {
    return(NULL)
  }
  vals <- as.numeric(value_by_date[as.character(ref_dates)])
  obs <- is.finite(vals)
  if (sum(obs) < as.integer(min_days)) {
    return(NULL)
  }
  filled <- vals
  filled[!obs] <- 0
  # Explicit origin at window start (0 mm), then cumulative through each day.
  cs <- as.numeric(cumsum(filled))
  tibble::tibble(
    plot_date = c(ref_dates[[1]], ref_dates),
    value = c(0, cs)
  )
}

#' Shift dates back by an integer number of years (month-day preserving).
shift_dates_years <- function(dates, n_years) {
  dates <- as.Date(dates)
  if (n_years == 0L) {
    return(dates)
  }
  if (requireNamespace("lubridate", quietly = TRUE)) {
    return(lubridate::`%m-%`(dates, lubridate::years(as.integer(n_years))))
  }
  # Fallback: POSIXlt year arithmetic
  lt <- as.POSIXlt(dates)
  lt$year <- lt$year - as.integer(n_years)
  as.Date(lt)
}

#' Build rolling 12-month climatology layers from a daily series.
#'
#' The plot window is the trailing 365 days ending at `as_of` (inclusive):
#' `[as_of - 364, as_of]`. Historical series are prior 12-month slices with the
#' same month–day bounds, shifted forward onto that window for seasonal
#' alignment. The current trailing window is the red line and is excluded from
#' the historical envelope / grey lines.
#'
#' When `cumulative = TRUE` (precipitation), each window is converted to a true
#' cumulative curve starting at 0 at the window start and summing forward.
#' Median / ribbons / grey years use the same definition. When `cumulative =
#' FALSE` (temperature), daily values are plotted as-is.
#'
#' Uses every prior slice that meets `min_days_per_year` back to the earliest
#' date in `daily` (capped at 80 offsets). Returns `n_hist_years` for captions.
#'
#' @param daily tibble from [park_daily_weather()]
#' @param as_of Date; window end (latest weather date)
#' @param min_days_per_year minimum non-NA days for a historical slice to keep
#' @param cumulative if TRUE, in-window cumulative sum from 0
#' @param smooth_days centred rolling mean window for median/percentile ribbons
#'   (default 11). Set `<= 1` to disable. Does not smooth the current (red) line.
#' @return list with `years`, `envelope`, `current`, window dates, `has_data`,
#'   `n_hist_years`, `record_start`, `record_end`, `smooth_days`
weather_climatology_layers <- function(daily,
                                       as_of = Sys.Date(),
                                       min_days_per_year = 60L,
                                       cumulative = FALSE,
                                       smooth_days = 11L) {
  empty_years <- tibble::tibble(
    year_offset = integer(),
    plot_date = as.Date(character()),
    value = numeric()
  )
  empty_env <- tibble::tibble(
    plot_date = as.Date(character()),
    p05 = numeric(),
    p25 = numeric(),
    p50 = numeric(),
    p75 = numeric(),
    p95 = numeric()
  )
  empty_cur <- tibble::tibble(
    plot_date = as.Date(character()),
    value = numeric()
  )
  as_of <- as.Date(as_of)
  window_end <- as_of
  window_start <- window_end - 364L
  out <- list(
    years = empty_years,
    envelope = empty_env,
    current = empty_cur,
    window_start = window_start,
    window_end = window_end,
    as_of = as_of,
    has_data = FALSE,
    n_hist_years = 0L,
    record_start = as.Date(NA),
    record_end = as.Date(NA),
    cumulative = isTRUE(cumulative),
    smooth_days = as.integer(smooth_days)[[1]]
  )
  if (!is.data.frame(daily) || nrow(daily) < 1L) {
    return(out)
  }
  if (!all(c("date", "value") %in% names(daily))) {
    return(out)
  }

  df <- daily
  df$date <- as.Date(df$date)
  df$value <- as.numeric(df$value)
  df <- df[is.finite(df$value) & !is.na(df$date), , drop = FALSE]
  if (!nrow(df)) {
    return(out)
  }

  out$record_start <- min(df$date)
  out$record_end <- max(df$date)

  ref_dates <- seq.Date(window_start, window_end, by = "day")
  value_by_date <- stats::setNames(df$value, as.character(df$date))

  # Pull values on `source_dates`; return rows aligned to `plot_dates`.
  extract_window <- function(source_dates, plot_dates) {
    source_dates <- as.Date(source_dates)
    plot_dates <- as.Date(plot_dates)
    if (isTRUE(cumulative)) {
      series <- park_window_cumsum(
        source_dates,
        value_by_date,
        min_days = min_days_per_year
      )
      if (is.null(series)) {
        return(NULL)
      }
      # Same origin + daily grid, remapped onto the plot window dates.
      series$plot_date <- c(plot_dates[[1]], plot_dates)
      return(series)
    }
    vals <- as.numeric(value_by_date[as.character(source_dates)])
    ok <- is.finite(vals)
    if (sum(ok) < as.integer(min_days_per_year)) {
      return(NULL)
    }
    tibble::tibble(
      plot_date = plot_dates[ok],
      value = as.numeric(vals[ok])
    )
  }

  # Current trailing window (red)
  cur <- extract_window(ref_dates, ref_dates)
  if (!is.null(cur)) {
    out$current <- cur
  }

  # Historical prior 12-month slices → aligned onto ref_dates
  min_date <- min(df$date)
  max_offset <- 0L
  probe <- window_start
  while (TRUE) {
    next_start <- shift_dates_years(probe, 1L)
    if (is.na(next_start) || next_start < min_date) {
      break
    }
    max_offset <- max_offset + 1L
    probe <- next_start
    if (max_offset > 80L) {
      break
    }
  }

  hist_rows <- list()
  if (max_offset >= 1L) {
    for (i in seq_len(max_offset)) {
      hist_dates <- shift_dates_years(ref_dates, i)
      series <- extract_window(hist_dates, ref_dates)
      if (is.null(series)) {
        next
      }
      hist_rows[[length(hist_rows) + 1L]] <- tibble::tibble(
        year_offset = as.integer(i),
        plot_date = series$plot_date,
        value = series$value
      )
    }
  }

  if (length(hist_rows) >= 1L) {
    out$years <- dplyr::bind_rows(hist_rows) |>
      dplyr::arrange(.data$year_offset, .data$plot_date)
    out$n_hist_years <- length(unique(out$years$year_offset))
    out$envelope <- out$years |>
      dplyr::group_by(.data$plot_date) |>
      dplyr::summarise(
        p05 = as.numeric(stats::quantile(.data$value, 0.05, na.rm = TRUE, names = FALSE)),
        p25 = as.numeric(stats::quantile(.data$value, 0.25, na.rm = TRUE, names = FALSE)),
        p50 = as.numeric(stats::quantile(.data$value, 0.50, na.rm = TRUE, names = FALSE)),
        p75 = as.numeric(stats::quantile(.data$value, 0.75, na.rm = TRUE, names = FALSE)),
        p95 = as.numeric(stats::quantile(.data$value, 0.95, na.rm = TRUE, names = FALSE)),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$plot_date)
    if (is.finite(out$smooth_days) && out$smooth_days > 1L) {
      out$envelope <- smooth_climatology_envelope(
        out$envelope,
        smooth_days = out$smooth_days
      )
    }
  }

  out$has_data <- nrow(out$envelope) >= 1L || nrow(out$current) >= 1L
  out
}

#' Shared month-break Date scale for stacked climate panels.
#'
#' Ensures precip and temp panels use identical limits and month ticks so
#' cowplot / plot_grid vertical alignment matches Fig. 2.
weather_climatology_x_scale <- function(window_start, window_end) {
  window_start <- as.Date(window_start)
  window_end <- as.Date(window_end)
  ggplot2::scale_x_date(
    limits = c(window_start, window_end),
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = c(0.01, 0)
  )
}

#' ggplot2 rolling 12-month climatology envelope.
#'
#' @param layers list from [weather_climatology_layers()]
#' @param ylab y-axis label
#' @param title plot title
#' @param log1p_y if TRUE, plot log1p(value) with expm1 tick labels (precip)
#' @param show_x_axis if FALSE, hide x tick labels/ticks (for upper stacked panel)
#' @param show_subtitle if FALSE, omit the envelope subtitle (combined caption)
#' @param x_limits optional length-2 Date vector overriding layer window limits
#' @return ggplot object (or empty annotated plot when no data)
plot_weather_climatology <- function(layers,
                                     ylab,
                                     title,
                                     log1p_y = FALSE,
                                     show_x_axis = TRUE,
                                     show_subtitle = TRUE,
                                     x_limits = NULL) {
  empty_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg) +
      ggplot2::theme_void() +
      ggplot2::labs(title = title)
  }
  if (!is.list(layers) || !isTRUE(layers$has_data)) {
    return(empty_plot("Climate figure unavailable for this park"))
  }

  years <- layers$years
  env <- layers$envelope
  cur <- layers$current
  transform_y <- if (isTRUE(log1p_y)) {
    function(x) log1p(pmax(x, 0))
  } else {
    identity
  }

  x_lo <- if (!is.null(x_limits) && length(x_limits) == 2L) {
    as.Date(x_limits[[1]])
  } else {
    as.Date(layers$window_start)
  }
  x_hi <- if (!is.null(x_limits) && length(x_limits) == 2L) {
    as.Date(x_limits[[2]])
  } else {
    as.Date(layers$window_end)
  }

  subtitle <- NULL
  if (isTRUE(show_subtitle)) {
    n_hist <- suppressWarnings(as.integer(layers$n_hist_years))
    smooth_days <- suppressWarnings(as.integer(layers$smooth_days))
    smooth_txt <- if (length(smooth_days) == 1L && is.finite(smooth_days) &&
                        smooth_days > 1L) {
      sprintf("; median/ribbons: %d-day rolling mean", smooth_days)
    } else {
      ""
    }
    if (length(n_hist) == 1L && is.finite(n_hist) && n_hist >= 1L) {
      rs <- layers$record_start
      re <- layers$record_end
      if (!is.null(rs) && !is.null(re) && !is.na(rs) && !is.na(re)) {
        subtitle <- sprintf(
          "Envelope from %d prior 12-mo slices (record %s–%s)%s",
          n_hist,
          format(as.Date(rs), "%Y"),
          format(as.Date(re), "%Y"),
          smooth_txt
        )
      } else {
        subtitle <- sprintf(
          "Envelope from %d prior 12-mo slices%s",
          n_hist,
          smooth_txt
        )
      }
    }
  }

  p <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = ylab
    ) +
    weather_climatology_x_scale(x_lo, x_hi) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.margin = ggplot2::margin(4, 8, 2, 8)
    )
  if (!isTRUE(show_x_axis)) {
    p <- p +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  }

  if (nrow(env) >= 1L) {
    env_plot <- env
    env_plot$p05 <- transform_y(env_plot$p05)
    env_plot$p25 <- transform_y(env_plot$p25)
    env_plot$p50 <- transform_y(env_plot$p50)
    env_plot$p75 <- transform_y(env_plot$p75)
    env_plot$p95 <- transform_y(env_plot$p95)
    p <- p +
      ggplot2::geom_ribbon(
        data = env_plot,
        ggplot2::aes(x = .data$plot_date, ymin = .data$p05, ymax = .data$p95),
        fill = "grey70",
        alpha = 0.35
      ) +
      ggplot2::geom_ribbon(
        data = env_plot,
        ggplot2::aes(x = .data$plot_date, ymin = .data$p25, ymax = .data$p75),
        fill = "grey50",
        alpha = 0.45
      )
  }

  if (nrow(years) >= 1L) {
    years_plot <- years
    years_plot$value <- transform_y(years_plot$value)
    p <- p +
      ggplot2::geom_line(
        data = years_plot,
        ggplot2::aes(
          x = .data$plot_date,
          y = .data$value,
          group = .data$year_offset
        ),
        color = "grey55",
        linewidth = 0.25,
        alpha = 0.55
      )
  }

  if (nrow(env) >= 1L) {
    env_med <- env
    env_med$p50 <- transform_y(env_med$p50)
    p <- p +
      ggplot2::geom_line(
        data = env_med,
        ggplot2::aes(x = .data$plot_date, y = .data$p50),
        color = "black",
        linewidth = 1.05
      )
  }

  if (nrow(cur) >= 1L) {
    cur_plot <- cur
    cur_plot$value <- transform_y(cur_plot$value)
    p <- p +
      ggplot2::geom_line(
        data = cur_plot,
        ggplot2::aes(x = .data$plot_date, y = .data$value),
        color = "#b91c1c",
        linewidth = 1.1
      )
  }

  if (isTRUE(log1p_y)) {
    p <- p +
      ggplot2::scale_y_continuous(
        labels = function(x) {
          pretty_vals <- expm1(x)
          ifelse(
            abs(pretty_vals) >= 10,
            format(round(pretty_vals), scientific = FALSE, trim = TRUE),
            format(
              round(pretty_vals, 1),
              scientific = FALSE,
              trim = TRUE,
              nsmall = 1
            )
          )
        }
      )
  }

  p
}

#' Two-panel precip + temperature climatology with aligned x-axis ticks.
#'
#' Stacks cumulative precip (top) and daily temperature (bottom) via
#' `cowplot::plot_grid(align = "v", axis = "lr")`, sharing the same rolling
#' 12-month date limits and month breaks (same pattern as Fig. 2).
#'
#' @param precip_layers list from [weather_climatology_layers()] (`cumulative`)
#' @param temp_layers list from [weather_climatology_layers()] (daily temp)
#' @return cowplot / ggplot grob
plot_weather_climatology_combo <- function(precip_layers, temp_layers) {
  # Prefer precip window; fall back to temp if precip missing
  x_lo <- NA
  x_hi <- NA
  if (is.list(precip_layers) && isTRUE(precip_layers$has_data)) {
    x_lo <- as.Date(precip_layers$window_start)
    x_hi <- as.Date(precip_layers$window_end)
  } else if (is.list(temp_layers) && isTRUE(temp_layers$has_data)) {
    x_lo <- as.Date(temp_layers$window_start)
    x_hi <- as.Date(temp_layers$window_end)
  }
  x_limits <- if (is.finite(x_lo) && is.finite(x_hi)) {
    c(x_lo, x_hi)
  } else {
    NULL
  }

  p_precip <- plot_weather_climatology(
    precip_layers,
    ylab = "Cumulative precip (mm)",
    title = "Cumulative precipitation",
    log1p_y = TRUE,
    show_x_axis = FALSE,
    show_subtitle = FALSE,
    x_limits = x_limits
  )
  p_temp <- plot_weather_climatology(
    temp_layers,
    ylab = "Temperature (°C)",
    title = "Temperature",
    log1p_y = FALSE,
    show_x_axis = TRUE,
    show_subtitle = FALSE,
    x_limits = x_limits
  )

  if (!requireNamespace("cowplot", quietly = TRUE)) {
    # Soft fallback: return precip only if cowplot missing
    return(p_precip)
  }
  cowplot::plot_grid(
    p_precip,
    p_temp,
    ncol = 1,
    align = "v",
    axis = "lr",
    rel_heights = c(1, 1),
    labels = NULL
  )
}
