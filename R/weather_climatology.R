# Rolling 12-month climatology envelope plots for park weather
# (GSOD via emma_envdata `weather_data`).
#
# Alignment: prior 12-month slices, calendar-shifted onto the current
# trailing window that ends at the latest available weather date. Not a
# fixed Jan–Dec calendar-year / DOY axis.

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
#' @param daily tibble from [park_daily_weather()]
#' @param as_of Date; window end (latest weather date)
#' @param min_days_per_year minimum non-NA days for a historical slice to keep
#' @return list with `years`, `envelope`, `current`, window dates, `has_data`
weather_climatology_layers <- function(daily,
                                       as_of = Sys.Date(),
                                       min_days_per_year = 60L) {
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
    has_data = FALSE
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

  ref_dates <- seq.Date(window_start, window_end, by = "day")
  value_by_date <- stats::setNames(df$value, as.character(df$date))

  # Current trailing window (red)
  cur_vals <- value_by_date[as.character(ref_dates)]
  cur_ok <- is.finite(cur_vals)
  if (any(cur_ok)) {
    out$current <- tibble::tibble(
      plot_date = ref_dates[cur_ok],
      value = as.numeric(cur_vals[cur_ok])
    )
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
      vals <- value_by_date[as.character(hist_dates)]
      ok <- is.finite(vals)
      if (sum(ok) < as.integer(min_days_per_year)) {
        next
      }
      hist_rows[[length(hist_rows) + 1L]] <- tibble::tibble(
        year_offset = as.integer(i),
        plot_date = ref_dates[ok],
        value = as.numeric(vals[ok])
      )
    }
  }

  if (length(hist_rows) >= 1L) {
    out$years <- dplyr::bind_rows(hist_rows) |>
      dplyr::arrange(.data$year_offset, .data$plot_date)
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
  }

  out$has_data <- nrow(out$envelope) >= 1L || nrow(out$current) >= 1L
  out
}

#' ggplot2 rolling 12-month climatology envelope.
#'
#' @param layers list from [weather_climatology_layers()]
#' @param ylab y-axis label
#' @param title plot title
#' @param log1p_y if TRUE, plot log1p(value) with expm1 tick labels (precip)
#' @return ggplot object (or empty annotated plot when no data)
plot_weather_climatology <- function(layers,
                                     ylab,
                                     title,
                                     log1p_y = FALSE) {
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

  p <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = ylab
    ) +
    ggplot2::scale_x_date(
      limits = c(layers$window_start, layers$window_end),
      date_breaks = "1 month",
      date_labels = "%b %Y",
      expand = c(0.01, 0)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

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
