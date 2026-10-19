#' Park manager KPIs — ranks 3–6 + E1/E2 helpers
#'
#' Rank **3** / **E4**: park-mean monthly ΔEVI + stress flag
#'   (threshold = −k × CV, k = 0.25; N = 3 consecutive composites).
#' Rank **4** / **7A**: iNat invasive watch list (counts + caveats only).
#' Rank **5**: 12-month precip/temp anomaly vs long mean at nearest station.
#' Rank **6**: fire-history outline symbology (single leaflet layer).
#' **E1**: age-class strip (&lt;5 / 5–10 / 10–20 / &gt;20 + % censored).
#' **E2**: continuous years-since-fire × EVI scatter points (known ages only).

STRESS_FLAG_K <- 0.25
STRESS_FLAG_N <- 3L
#' Lookback window (months) for park-mean ΔEVI series / CV
STRESS_SERIES_MONTHS <- 36L

#' Extract park-mean of a SpatRaster over domain cells intersecting the park.
park_raster_mean <- function(rast, park_sf, na.rm = TRUE) {
  stopifnot(inherits(rast, "SpatRaster"))
  parks <- sf::st_as_sf(park_sf)
  if (nrow(parks) < 1L) {
    return(NA_real_)
  }
  domain_crs <- terra::crs(rast)
  if (!is.na(domain_crs) &&
        !identical(sf::st_crs(parks), sf::st_crs(domain_crs))) {
    parks <- sf::st_transform(parks, crs = domain_crs)
  }
  ex <- terra::extract(
    x = rast[[1]],
    y = terra::vect(parks[1, , drop = FALSE]),
    fun = mean,
    na.rm = na.rm,
    touches = TRUE,
    ID = FALSE
  )
  val <- suppressWarnings(as.numeric(ex[[1]][1]))
  if (!is.finite(val)) {
    return(NA_real_)
  }
  val
}

#' Domain pid integers intersecting a park polygon.
park_domain_pids <- function(domain_rast, park_sf) {
  parks <- sf::st_as_sf(park_sf)
  domain_crs <- terra::crs(domain_rast)
  if (!is.na(domain_crs) &&
        !identical(sf::st_crs(parks), sf::st_crs(domain_crs))) {
    parks <- sf::st_transform(parks, crs = domain_crs)
  }
  pid_layer <- if ("pid" %in% names(domain_rast)) {
    domain_rast[["pid"]]
  } else {
    domain_rast[[1]]
  }
  ex <- terra::extract(
    x = pid_layer,
    y = terra::vect(parks[1, , drop = FALSE]),
    touches = TRUE
  )
  unique(stats::na.omit(as.integer(ex[[2]])))
}

#' CV of a near-zero-mean anomaly series.
#'
#' Classical CV = sd/mean is unstable when mean(Δ) ≈ 0. Use
#' `sd(Δ) / mean(|Δ|)` (documented on the park page). Falls back to `sd`
#' when mean(|Δ|) is non-finite or near zero.
cv_anomaly_series <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 3L) {
    return(NA_real_)
  }
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) {
    return(NA_real_)
  }
  denom <- mean(abs(x))
  if (!is.finite(denom) || denom < .Machine$double.eps * 10) {
    return(s)
  }
  s / denom
}

#' Stress flag from an ordered park-mean monthly ΔEVI series.
#'
#' @param delta_series Numeric vector oldest→newest (one value per composite).
#' @param k Multiplier of CV (locked **0.25**).
#' @param n Persistence: consecutive composites below threshold (locked **3**).
#' @return List with flag, threshold, cv, latest, n_below, k, n.
evaluate_stress_flag <- function(delta_series,
                                 k = STRESS_FLAG_K,
                                 n = STRESS_FLAG_N) {
  out <- list(
    flagged = FALSE,
    threshold = NA_real_,
    cv = NA_real_,
    latest = NA_real_,
    n_below = 0L,
    k = k,
    n = as.integer(n),
    n_composites = 0L
  )
  x <- as.numeric(delta_series)
  x <- x[is.finite(x)]
  out$n_composites <- length(x)
  if (!length(x)) {
    return(out)
  }
  out$latest <- x[[length(x)]]
  cv <- cv_anomaly_series(x)
  out$cv <- cv
  if (!is.finite(cv) || cv <= 0) {
    return(out)
  }
  thr <- -as.numeric(k) * cv
  out$threshold <- thr
  n <- as.integer(n)
  if (length(x) < n) {
    return(out)
  }
  tail_x <- x[(length(x) - n + 1L):length(x)]
  below <- sum(tail_x < thr, na.rm = TRUE)
  out$n_below <- as.integer(below)
  out$flagged <- below >= n
  out
}

#' Build park-mean fused EVI series from VI parquets (lookback window).
#'
#' For each composite date in the window: fuse sensors for park pids, then
#' park mean. Returns a data.frame(date, park_mean_evi). Soft-fails to empty
#' when no parquet assets are available.
park_mean_evi_series <- function(park_pids,
                                 env_files = NULL,
                                 temp_directory = "data/temp/",
                                 lookback_months = STRESS_SERIES_MONTHS,
                                 verbose = TRUE) {
  empty <- data.frame(
    date = as.Date(character()),
    park_mean_evi = numeric(),
    stringsAsFactors = FALSE
  )
  park_pids <- unique(stats::na.omit(as.integer(park_pids)))
  if (!length(park_pids)) {
    return(empty)
  }

  vi_files <- tryCatch(
    list_vi_parquet_assets(env_files = env_files, current_month = NULL),
    error = function(e) {
      if (isTRUE(verbose)) {
        message("park_mean_evi_series: list_vi_parquet_assets failed: ",
                conditionMessage(e))
      }
      NULL
    }
  )
  if (is.null(vi_files) || !nrow(vi_files)) {
    return(empty)
  }

  cutoff <- seq.Date(
    Sys.Date(),
    by = paste0("-", as.integer(lookback_months), " months"),
    length.out = 2L
  )[[2]]
  vi_files <- dplyr::filter(vi_files, .data$file_date >= cutoff)
  if (!nrow(vi_files)) {
    return(empty)
  }

  # Ensure downloads (reuse cache from mean_evi when present)
  for (i in seq_len(nrow(vi_files))) {
    download_envdata_asset(
      tag = vi_files$tag[[i]],
      file = vi_files$file_name[[i]],
      dest_dir = temp_directory,
      overwrite = FALSE,
      verbose = FALSE
    )
  }
  vi_files <- vi_files |>
    dplyr::mutate(path = file.path(temp_directory, .data$file_name))
  vi_files <- vi_files[file.exists(vi_files$path), , drop = FALSE]
  if (!nrow(vi_files)) {
    return(empty)
  }

  dates <- sort(unique(vi_files$file_date))
  rows <- vector("list", length(dates))
  pid_set <- as.integer(park_pids)
  for (di in seq_along(dates)) {
    d <- dates[[di]]
    day_files <- vi_files$path[vi_files$file_date == d]
    raw <- tryCatch(
      arrow::open_dataset(day_files) |>
        dplyr::select(dplyr::any_of(c("pid", "date", "sensor", "value"))) |>
        dplyr::filter(.data$pid %in% pid_set) |>
        dplyr::collect(),
      error = function(e) NULL
    )
    if (is.null(raw) || !nrow(raw)) {
      next
    }
    if (!"date" %in% names(raw)) {
      raw$date <- d
    }
    fused <- fuse_evi_by_pid_date(raw)
    fused <- fused[!is.na(fused$value), , drop = FALSE]
    if (!nrow(fused)) {
      next
    }
    # Parquet values are EVI × 100 integers
    mean_evi <- mean(scale_evi_integer(fused$value), na.rm = TRUE)
    if (is.finite(mean_evi)) {
      rows[[di]] <- data.frame(
        date = as.Date(d),
        park_mean_evi = as.numeric(mean_evi),
        stringsAsFactors = FALSE
      )
    }
  }
  dplyr::bind_rows(rows)
}

#' Convert park-mean EVI series → monthly ΔEVI (vs calendar-month climatology).
park_mean_monthly_delta_series <- function(evi_series) {
  if (!is.data.frame(evi_series) || nrow(evi_series) < 1L) {
    return(data.frame(
      date = as.Date(character()),
      park_mean_evi = numeric(),
      month_clim = numeric(),
      monthly_delta = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  df <- evi_series |>
    dplyr::mutate(
      date = as.Date(.data$date),
      month = lubridate::month(.data$date)
    ) |>
    dplyr::arrange(.data$date)
  clim <- df |>
    dplyr::group_by(.data$month) |>
    dplyr::summarise(
      month_clim = mean(.data$park_mean_evi, na.rm = TRUE),
      .groups = "drop"
    )
  df |>
    dplyr::left_join(clim, by = "month") |>
    dplyr::mutate(monthly_delta = .data$park_mean_evi - .data$month_clim) |>
    dplyr::select("date", "park_mean_evi", "month_clim", "monthly_delta")
}

#' Age-class percentages for known-age cells + % censored (**E1** / **9A**).
#'
#' @param years_since_fire SpatRaster (years)
#' @param fireage_rast SpatRaster with is_censored (optional)
#' @param park_mask Logical cell mask (domain ∩ park)
age_class_structure <- function(years_since_fire,
                                fireage_rast = NULL,
                                park_mask) {
  stopifnot(length(park_mask) == terra::ncell(years_since_fire))
  age_v <- terra::values(years_since_fire[[1]], mat = FALSE)
  if (!is.null(fireage_rast)) {
    masks <- fire_age_coverage_masks(fireage_rast, park_mask)
    known <- masks$known
    censored <- masks$censored
  } else {
    known <- park_mask & is.finite(age_v) & age_v >= 0
    censored <- park_mask & !known
  }
  n_park <- sum(park_mask, na.rm = TRUE)
  n_known <- sum(known, na.rm = TRUE)
  n_cens <- sum(censored, na.rm = TRUE)
  ages <- age_v[known]
  bin_counts <- c(
    lt5 = sum(ages < 5, na.rm = TRUE),
    b5_10 = sum(ages >= 5 & ages < 10, na.rm = TRUE),
    b10_20 = sum(ages >= 10 & ages < 20, na.rm = TRUE),
    gt20 = sum(ages >= 20, na.rm = TRUE)
  )
  pct_of_known <- if (n_known > 0) {
    100 * bin_counts / n_known
  } else {
    rep(NA_real_, 4L)
  }
  list(
    n_park = as.integer(n_park),
    n_known = as.integer(n_known),
    n_censored = as.integer(n_cens),
    pct_censored = if (n_park > 0) 100 * n_cens / n_park else NA_real_,
    pct_lt5 = unname(pct_of_known[["lt5"]]),
    pct_5_10 = unname(pct_of_known[["b5_10"]]),
    pct_10_20 = unname(pct_of_known[["b10_20"]]),
    pct_gt20 = unname(pct_of_known[["gt20"]]),
    counts = bin_counts
  )
}

#' Paired EVI × years-since-fire for known-age park cells (**E2**).
evi_tsf_scatter_df <- function(evi_rast,
                               years_since_fire,
                               fireage_rast = NULL,
                               park_mask,
                               max_points = 4000L) {
  stopifnot(length(park_mask) == terra::ncell(evi_rast))
  evi_v <- terra::values(evi_rast[[1]], mat = FALSE)
  age_v <- terra::values(years_since_fire[[1]], mat = FALSE)
  if (!is.null(fireage_rast)) {
    known <- fire_age_coverage_masks(fireage_rast, park_mask)$known
  } else {
    known <- park_mask & is.finite(age_v) & age_v >= 0
  }
  keep <- known & is.finite(evi_v) & is.finite(age_v)
  idx <- which(keep)
  if (!length(idx)) {
    return(data.frame(
      years_since_fire = numeric(),
      evi = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  if (length(idx) > max_points) {
    idx <- sample(idx, max_points)
  }
  data.frame(
    years_since_fire = as.numeric(age_v[idx]),
    evi = as.numeric(evi_v[idx]),
    stringsAsFactors = FALSE
  )
}

#' Invasive watch list by genus (rank **4** / **7A**).
invasive_watch_list <- function(focal_invasives) {
  empty <- data.frame(
    genus = character(),
    n = integer(),
    newest_obs = as.Date(character()),
    stringsAsFactors = FALSE
  )
  if (!inherits(focal_invasives, "sf") && !is.data.frame(focal_invasives)) {
    return(empty)
  }
  if (nrow(focal_invasives) < 1L) {
    return(empty)
  }
  df <- if (inherits(focal_invasives, "sf")) {
    sf::st_drop_geometry(focal_invasives)
  } else {
    as.data.frame(focal_invasives)
  }
  if (!"scientific_name" %in% names(df)) {
    return(empty)
  }
  df$genus <- vapply(
    strsplit(as.character(df$scientific_name), "\\s+"),
    function(parts) {
      if (!length(parts) || !nzchar(parts[[1]])) {
        return(NA_character_)
      }
      parts[[1]]
    },
    character(1)
  )
  if (!"observed_on" %in% names(df)) {
    df$observed_on <- as.Date(NA)
  } else {
    df$observed_on <- as.Date(df$observed_on)
  }
  df |>
    dplyr::filter(!is.na(.data$genus), nzchar(.data$genus)) |>
    dplyr::group_by(.data$genus) |>
    dplyr::summarise(
      n = dplyr::n(),
      newest_obs = suppressWarnings(max(.data$observed_on, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      newest_obs = dplyr::if_else(
        is.infinite(.data$newest_obs),
        as.Date(NA),
        as.Date(.data$newest_obs)
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$n), .data$genus)
}

#' 12-month precip / temp anomaly vs long-term mean (rank **5**).
#'
#' Uses the nearest station (first in `closest_stations` / first column after
#' mean) when multiple stations are present. Long mean = all available history
#' for the same station (GSOD window from emma_envdata, typically ~5 years).
weather_12mo_anomaly <- function(focal_weather,
                                 closest_stations = character(0),
                                 as_of = Sys.Date()) {
  out <- list(
    has_weather = FALSE,
    station_name = NA_character_,
    as_of = as.Date(as_of),
    precip_12mo_mm = NA_real_,
    precip_long_mean_mm = NA_real_,
    precip_anomaly_mm = NA_real_,
    precip_anomaly_pct = NA_real_,
    temp_12mo_c = NA_real_,
    temp_long_mean_c = NA_real_,
    temp_anomaly_c = NA_real_
  )
  if (!is.data.frame(focal_weather) || nrow(focal_weather) < 1L) {
    return(out)
  }
  need <- c("date_time", "station_name")
  if (!all(need %in% names(focal_weather))) {
    return(out)
  }
  df <- focal_weather
  df$date_time <- as.Date(df$date_time)
  # Prefer first closest station when available
  stn <- NA_character_
  if (length(closest_stations) >= 1L) {
    cand <- gsub("\\.gz\\.parquet$", "", closest_stations[[1]])
    # station_name in reformatted weather is often NAME, not STNID
    if (cand %in% df$station_name) {
      stn <- cand
    }
  }
  if (is.na(stn)) {
    # Most complete station by non-NA precip rows
    counts <- df |>
      dplyr::group_by(.data$station_name) |>
      dplyr::summarise(
        n = sum(!is.na(.data$mean_precip_mm) | !is.na(.data$temp_c)),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$n))
    if (nrow(counts) < 1L) {
      return(out)
    }
    stn <- as.character(counts$station_name[[1]])
  }
  st <- df[df$station_name == stn, , drop = FALSE]
  if (!nrow(st)) {
    return(out)
  }
  out$has_weather <- TRUE
  out$station_name <- stn
  end <- as.Date(as_of)
  start_12 <- seq.Date(end, by = "-12 months", length.out = 2L)[[2]]

  precip_col <- if ("mean_precip_mm" %in% names(st)) {
    "mean_precip_mm"
  } else if ("PRCP" %in% names(st)) {
    "PRCP"
  } else {
    NA_character_
  }
  temp_col <- if ("temp_c" %in% names(st)) {
    "temp_c"
  } else if ("TEMP" %in% names(st)) {
    "TEMP"
  } else {
    NA_character_
  }

  if (!is.na(precip_col)) {
    # Rolling 12-mo sum vs mean of all trailing 12-mo windows in history
    daily <- st[, c("date_time", precip_col)]
    names(daily) <- c("date", "precip")
    daily <- daily[is.finite(daily$precip) & daily$precip >= 0, , drop = FALSE]
    if (nrow(daily) >= 30L) {
      recent <- daily[daily$date > start_12 & daily$date <= end, , drop = FALSE]
      out$precip_12mo_mm <- sum(recent$precip, na.rm = TRUE)
      # Long mean: average annual precip from full record (scale to 365 days)
      n_days <- as.numeric(diff(range(daily$date))) + 1
      if (is.finite(n_days) && n_days >= 60) {
        ann <- sum(daily$precip, na.rm = TRUE) * (365.25 / n_days)
        out$precip_long_mean_mm <- ann
        out$precip_anomaly_mm <- out$precip_12mo_mm - ann
        if (is.finite(ann) && abs(ann) > 1e-6) {
          out$precip_anomaly_pct <- 100 * out$precip_anomaly_mm / ann
        }
      }
    }
  }

  if (!is.na(temp_col)) {
    daily_t <- st[, c("date_time", temp_col)]
    names(daily_t) <- c("date", "temp")
    daily_t <- daily_t[is.finite(daily_t$temp), , drop = FALSE]
    if (nrow(daily_t) >= 30L) {
      recent_t <- daily_t[daily_t$date > start_12 & daily_t$date <= end, ,
                          drop = FALSE]
      out$temp_12mo_c <- mean(recent_t$temp, na.rm = TRUE)
      out$temp_long_mean_c <- mean(daily_t$temp, na.rm = TRUE)
      if (is.finite(out$temp_12mo_c) && is.finite(out$temp_long_mean_c)) {
        out$temp_anomaly_c <- out$temp_12mo_c - out$temp_long_mean_c
      }
    }
  }
  out
}

#' Fire outline age-bin label (symbology helper; map uses one "Fire History" layer).
fire_outline_age_bin <- function(years) {
  y <- as.numeric(years)
  dplyr::case_when(
    !is.finite(y) ~ "Burns (age unknown)",
    y < 5 ~ "Burns <5 y",
    y < 10 ~ "Burns 5–10 y",
    y < 20 ~ "Burns 10–20 y",
    TRUE ~ "Burns >20 y"
  )
}

FIRE_OUTLINE_GROUPS <- c(
  "Burns <5 y",
  "Burns 5–10 y",
  "Burns 10–20 y",
  "Burns >20 y"
)

#' Single leaflet overlay group for all burn-outline polygons.
FIRE_HISTORY_GROUP <- "Fire History"

#' Discrete stroke colors for fire-history age bins (one layer, multi-style).
fire_history_outline_color <- function(years) {
  bin <- fire_outline_age_bin(years)
  cols <- c(
    "Burns <5 y" = "#b30000",
    "Burns 5–10 y" = "#e34a33",
    "Burns 10–20 y" = "#fc8d59",
    "Burns >20 y" = "#fdbb84",
    "Burns (age unknown)" = "#636363"
  )
  out <- unname(cols[bin])
  out[is.na(out)] <- "#636363"
  out
}

#' Format rank-3 KPI + stress strip (Markdown).
format_delta_evi_kpi_md <- function(park_mean_delta,
                                    stress,
                                    evi_as_of = NULL,
                                    lookback_months = STRESS_SERIES_MONTHS) {
  delta_txt <- if (is.finite(park_mean_delta)) {
    sprintf("%+.3f", park_mean_delta)
  } else {
    "unavailable"
  }
  as_of_txt <- if (!is.null(evi_as_of) && !is.na(as.Date(evi_as_of))) {
    paste0(" (composite ", format(as.Date(evi_as_of), "%Y-%m-%d"), ")")
  } else {
    ""
  }
  flag_txt <- if (isTRUE(stress$flagged)) {
    '<span class="emma-stress-flag emma-stress-on">Stress flag ON</span>'
  } else if (is.finite(stress$threshold)) {
    '<span class="emma-stress-flag emma-stress-off">Stress flag off</span>'
  } else {
    '<span class="emma-stress-flag emma-stress-na">Stress flag unavailable</span>'
  }
  thr_txt <- if (is.finite(stress$threshold) && is.finite(stress$cv)) {
    sprintf(
      paste0(
        "Threshold = −%.2f × CV = %.4f ",
        "(CV = sd(Δ)/mean(|Δ|) over last %d months of park-mean monthly ΔEVI; ",
        "N = %d consecutive composites below cut)."
      ),
      stress$k,
      stress$threshold,
      as.integer(lookback_months),
      stress$n
    )
  } else {
    paste0(
      "Stress flag needs a park-mean monthly ΔEVI series ",
      "(VI parquet lookback); KPI uses the latest monthly_delta_evi raster."
    )
  }
  paste0(
    '<div class="emma-kpi-strip" markdown="1">\n\n',
    "**Park-mean monthly ΔEVI** — ", delta_txt, as_of_txt, " · ", flag_txt, "\n\n",
    "*", thr_txt, "*\n\n",
    "</div>\n"
  )
}

#' Format invasive watch list + caveats (rank **4**).
format_invasive_watch_md <- function(watch_df) {
  caveat <- paste0(
    "*iNaturalist opportunistic records in the park + 1 km buffer ",
    "(recent ~900 days on the map). Counts reflect observer effort as well ",
    "as invasion pressure. See [Data](../data.html).*\n\n"
  )
  if (!is.data.frame(watch_df) || nrow(watch_df) < 1L) {
    return(paste0(
      '<div class="emma-watch-list" markdown="1">\n\n',
      "**Invasive watch list** — no recent iNaturalist invasive records ",
      "in the park buffer.\n\n",
      caveat,
      "</div>\n"
    ))
  }
  rows <- character(nrow(watch_df))
  for (i in seq_len(nrow(watch_df))) {
    newest <- watch_df$newest_obs[[i]]
    newest_txt <- if (length(newest) != 1L || is.na(newest)) {
      "—"
    } else {
      format(as.Date(newest), "%Y-%m-%d")
    }
    rows[[i]] <- paste0(
      "| ", watch_df$genus[[i]], " | ", watch_df$n[[i]], " | ", newest_txt, " |"
    )
  }
  paste0(
    '<div class="emma-watch-list" markdown="1">\n\n',
    "**Invasive watch list** (iNaturalist, recent records)\n\n",
    "| Genus | Count | Newest obs |\n|---|---:|---|\n",
    paste(rows, collapse = "\n"), "\n\n",
    caveat,
    "</div>\n"
  )
}

#' Format 12-mo climate anomaly card (rank **5**).
format_weather_anomaly_md <- function(anom) {
  if (!isTRUE(anom$has_weather)) {
    return(paste0(
      '<div class="emma-climate-card" markdown="1">\n\n',
      "**Rainfall & temperature context** — unavailable ",
      "(no GSOD rows for nearby stations).\n\n</div>\n"
    ))
  }
  precip_txt <- if (is.finite(anom$precip_anomaly_mm)) {
    sprintf(
      "precip 12-mo **%+.0f mm** vs long-term annualised mean (%.0f mm)",
      anom$precip_anomaly_mm,
      anom$precip_long_mean_mm
    )
  } else {
    "precip anomaly unavailable"
  }
  temp_txt <- if (is.finite(anom$temp_anomaly_c)) {
    sprintf(
      "temp 12-mo mean **%+.2f °C** vs long-term mean (%.2f °C)",
      anom$temp_anomaly_c,
      anom$temp_long_mean_c
    )
  } else {
    "temp anomaly unavailable"
  }
  paste0(
    '<div class="emma-climate-card" markdown="1">\n\n',
    "**Rainfall & temperature context** (nearest station: ",
    anom$station_name %||% "—", ") — ",
    precip_txt, "; ", temp_txt, ". ",
    "*Long mean uses the full GSOD record published in ",
    "`weather_data` (typically ~5 years). Pair with monthly ΔEVI when ",
    "interpreting brown anomalies.*\n\n",
    "</div>\n"
  )
}

#' Format E1 age-class strip.
format_age_class_md <- function(age) {
  pct <- function(x) {
    if (!is.finite(x)) "—" else sprintf("%.0f%%", x)
  }
  paste0(
    '<div class="emma-age-class-strip" markdown="1">\n\n',
    "**Age-class structure** (known-age cells) — ",
    "&lt;5 y: ", pct(age$pct_lt5),
    " · 5–10 y: ", pct(age$pct_5_10),
    " · 10–20 y: ", pct(age$pct_10_20),
    " · &gt;20 y: ", pct(age$pct_gt20),
    " · censored / never-burned: ", pct(age$pct_censored),
    " of park cells.\n\n",
    "</div>\n"
  )
}
