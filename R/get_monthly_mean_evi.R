library(terra)

#' List VI parquet release assets (MODIS + optional VIIRS)
#'
#' Always requires `vi_modis_parquet`. Includes `vi_viirs_parquet` when the
#' release tag exists; if the tag is still unpublished (W1.4), logs a message
#' and returns MODIS-only rows so Terra→Aqua fill still works.
list_vi_parquet_assets <- function(env_files = NULL,
                                   current_month = NULL) {
  if (is.null(env_files) || !nrow(env_files)) {
    env_files <- list_release_assets(
      repo = ENVDATA_REPO,
      tag = ENVDATA_RELEASE_TAGS$vi_modis_parquet
    )
  }

  modis <- env_files |>
    dplyr::filter(
      .data$tag == ENVDATA_RELEASE_TAGS$vi_modis_parquet,
      grepl("^vi_modis_[0-9]{8}\\.parquet$", .data$file_name)
    ) |>
    dplyr::mutate(
      family = "modis",
      file_date = parse_envdata_yyyymmdd(.data$file_name)
    )

  viirs <- env_files |>
    dplyr::filter(
      .data$tag == ENVDATA_RELEASE_TAGS$vi_viirs_parquet,
      grepl("^vi_viirs_[0-9]{8}\\.parquet$", .data$file_name)
    ) |>
    dplyr::mutate(
      family = "viirs",
      file_date = parse_envdata_yyyymmdd(.data$file_name)
    )

  # Tag may be absent from env_files when the release 404s at list time.
  if (!nrow(viirs)) {
    viirs <- tryCatch(
      {
        listed <- list_release_assets(
          repo = ENVDATA_REPO,
          tag = ENVDATA_RELEASE_TAGS$vi_viirs_parquet
        )
        listed |>
          dplyr::filter(grepl("^vi_viirs_[0-9]{8}\\.parquet$", .data$file_name)) |>
          dplyr::mutate(
            family = "viirs",
            file_date = parse_envdata_yyyymmdd(.data$file_name)
          )
      },
      error = function(e) {
        message(
          "vi_viirs_parquet release unavailable (",
          conditionMessage(e),
          "); monthly/long-term EVI means will use MODIS Terra→Aqua only until W1.4 publishes VIIRS parquets."
        )
        modis[0, , drop = FALSE]
      }
    )
  }

  vi_files <- dplyr::bind_rows(modis, viirs) |>
    dplyr::filter(!is.na(.data$file_date))

  if (!is.null(current_month)) {
    month_int <- as.integer(current_month)[[1]]
    if (length(as.integer(current_month)) != 1L ||
          is.na(month_int) || month_int < 1L || month_int > 12L) {
      stop(
        "list_vi_parquet_assets(): current_month must be a single integer 1–12; got ",
        paste(deparse(current_month), collapse = " ")
      )
    }
    n_before <- nrow(vi_files)
    n_dates_before <- length(unique(vi_files$file_date))
    vi_files <- dplyr::filter(
      vi_files,
      as.integer(lubridate::month(.data$file_date)) == month_int
    )
    n_after <- nrow(vi_files)
    n_dates_after <- length(unique(vi_files$file_date))
    message(
      "VI parquet month filter: month=", month_int,
      " files ", n_before, "→", n_after,
      " dates ", n_dates_before, "→", n_dates_after
    )
    # Guard against the historical date-parse recycle bug that left every row
    # with the same date and made monthly mean ≈ all-history mean (ΔEVI layers
    # looked identical on park maps). With a multi-year archive, month filter
    # must drop composites from other calendar months.
    if (n_dates_before >= 24L && n_dates_after >= n_dates_before) {
      stop(
        "list_vi_parquet_assets(): month=", month_int,
        " filter kept all ", n_dates_before,
        " composite dates — refusing to build a bogus monthly climatology"
      )
    }
    if (n_after < 1L) {
      stop(
        "list_vi_parquet_assets(): no VI parquet assets for calendar month ",
        month_int
      )
    }
  }

  vi_files
}

#' Whether parquet row dates match the release filename composite date
#'
#' Historical `vi_*_YYYYMMDD.parquet` assets on emma_envdata are often
#' byte-duplicates of a July–August template with rewritten years: the
#' filename month is wrong and hundreds of “distinct” composites share the
#' same values. Using those for fuse-then-mean made monthly climatology ≈
#' all-history mean, so park-map ΔEVI (monthly) and ΔEVI (long term) looked
#' identical. Require a majority of rows in the filename calendar month+year
#' or within `window_days` of the filename date.
vi_parquet_content_matches_file_date <- function(dates,
                                                 file_date,
                                                 min_frac = 0.5,
                                                 window_days = 16L) {
  dates <- as.Date(dates)
  file_date <- as.Date(file_date)[[1]]
  if (!length(dates) || is.na(file_date)) {
    return(FALSE)
  }
  ok <- !is.na(dates)
  if (!any(ok)) {
    return(FALSE)
  }
  dates <- dates[ok]
  frac_month <- mean(
    lubridate::month(dates) == lubridate::month(file_date) &
      lubridate::year(dates) == lubridate::year(file_date)
  )
  frac_near <- mean(abs(as.numeric(dates - file_date)) <= as.integer(window_days))
  isTRUE(frac_month >= min_frac || frac_near >= min_frac)
}

#' Add one fused (pid, date) table into running sum/count (unique pids per date)
accumulate_fused_evi_means <- function(sum_vec, count_vec, fused) {
  if (!nrow(fused)) {
    return(list(sum_vec = sum_vec, count_vec = count_vec))
  }
  fused <- fused[!is.na(fused$pid) & !is.na(fused$date) & !is.na(fused$value),
                 , drop = FALSE]
  if (!nrow(fused)) {
    return(list(sum_vec = sum_vec, count_vec = count_vec))
  }
  for (fd in sort(unique(fused$date))) {
    sub <- fused[fused$date == fd, , drop = FALSE]
    keys <- as.character(sub$pid)
    if (anyDuplicated(keys)) {
      # fuse_evi_by_pid_date should already be unique per (pid, date)
      sub <- sub[!duplicated(sub$pid), , drop = FALSE]
      keys <- as.character(sub$pid)
    }
    known <- keys %in% names(sum_vec)
    if (any(!known)) {
      sum_vec <- c(sum_vec, stats::setNames(sub$value[!known], keys[!known]))
      count_vec <- c(
        count_vec,
        stats::setNames(rep(1, sum(!known)), keys[!known])
      )
    }
    if (any(known)) {
      k <- keys[known]
      sum_vec[k] <- sum_vec[k] + sub$value[known]
      count_vec[k] <- count_vec[k] + 1
    }
  }
  list(sum_vec = sum_vec, count_vec = count_vec)
}

#' Download parquet assets and return fused-then-mean EVI SpatRaster
#'
#' For each (pid, composite_date): priority-fill sensors 4→3→1→2, then
#' `mean(..., na.rm = TRUE)` across dates for each pid.
#'
#' Processes one composite date at a time (running sum/count) so all-history
#' means do not load ~3 GB of parquets into memory at once.
#'
#' Skips release assets whose internal `date` column does not match the
#' filename composite (corrupt historical duplicates). When `current_month`
#' is set, also keeps only rows in that calendar month after the alignment
#' check (row-level month filter; filename filter alone is not enough).
evi_mean_from_parquet_files <- function(vi_files,
                                        temp_directory = "data/temp/",
                                        current_month = NULL) {
  if (!nrow(vi_files)) {
    stop("No VI parquet assets to aggregate")
  }

  month_int <- NULL
  if (!is.null(current_month)) {
    month_int <- as.integer(current_month)[[1]]
    if (is.na(month_int) || month_int < 1L || month_int > 12L) {
      stop(
        "evi_mean_from_parquet_files(): current_month must be 1–12; got ",
        paste(deparse(current_month), collapse = " ")
      )
    }
  }

  n_files <- nrow(vi_files)
  need <- character(0)
  for (i in seq_len(n_files)) {
    dest <- file.path(temp_directory, vi_files$file_name[[i]])
    if (!file.exists(dest) || file.info(dest)$size <= 0) {
      need <- c(need, vi_files$file_name[[i]])
    }
  }
  message(
    "VI parquet downloads: ", n_files - length(need), " cached, ",
    length(need), " to fetch"
  )
  for (i in seq_len(n_files)) {
    download_envdata_asset(
      tag = vi_files$tag[[i]],
      file = vi_files$file_name[[i]],
      dest_dir = temp_directory,
      overwrite = FALSE,
      verbose = FALSE
    )
    if (i == 1L || i == n_files || i %% 50L == 0L) {
      message("Downloaded ", i, "/", n_files, " VI parquet assets")
    }
  }

  paths <- file.path(temp_directory, vi_files$file_name)
  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths)) {
    stop(
      "Missing downloaded parquet files: ",
      paste(basename(missing_paths), collapse = ", ")
    )
  }

  vi_files <- vi_files |>
    dplyr::mutate(path = file.path(temp_directory, .data$file_name))

  # Running sum / count of fused values per pid (fuse per date first).
  sum_vec <- numeric(0)
  count_vec <- numeric(0)
  n_skipped_misaligned <- 0L
  n_used_files <- 0L
  n_fused_dates <- 0L

  dates <- sort(unique(vi_files$file_date))
  n_dates <- length(dates)
  message(
    "Fuse-then-mean over ", n_dates, " filename composite date(s)",
    if (!is.null(month_int)) paste0(" (row month=", month_int, ")") else "",
    "; skip misaligned parquet content"
  )
  t0 <- proc.time()[["elapsed"]]
  for (di in seq_along(dates)) {
    d <- dates[[di]]
    day_files <- vi_files$path[vi_files$file_date == d]
    raw <- arrow::open_dataset(day_files) |>
      dplyr::select(dplyr::any_of(c("pid", "date", "sensor", "value"))) |>
      dplyr::collect()
    if (!nrow(raw)) {
      next
    }
    required <- c("pid", "sensor", "value")
    missing_cols <- setdiff(required, names(raw))
    if (length(missing_cols)) {
      stop("VI parquet missing columns: ", paste(missing_cols, collapse = ", "))
    }
    if (!"date" %in% names(raw)) {
      raw$date <- d
    }
    raw$date <- as.Date(raw$date)
    if (!vi_parquet_content_matches_file_date(raw$date, d)) {
      n_skipped_misaligned <- n_skipped_misaligned + length(day_files)
      next
    }
    if (!is.null(month_int)) {
      raw <- raw[as.integer(lubridate::month(raw$date)) == month_int, , drop = FALSE]
      if (!nrow(raw)) {
        next
      }
    }
    fused <- fuse_evi_by_pid_date(raw)
    fused <- fused[!is.na(fused$value), , drop = FALSE]
    if (!nrow(fused)) {
      next
    }
    n_used_files <- n_used_files + length(day_files)
    n_fused_dates <- n_fused_dates + length(unique(fused$date))
    acc <- accumulate_fused_evi_means(sum_vec, count_vec, fused)
    sum_vec <- acc$sum_vec
    count_vec <- acc$count_vec
    rm(raw, fused, acc)
    if (di == 1L || di == n_dates || di %% 25L == 0L) {
      elapsed <- proc.time()[["elapsed"]] - t0
      message(
        "Fused ", di, "/", n_dates, " filename dates (",
        round(elapsed, 1), "s elapsed, ",
        length(sum_vec), " pids; skipped_misaligned_files=",
        n_skipped_misaligned, ")"
      )
    }
  }

  message(
    "VI parquet fuse-then-mean kept ", n_used_files, " aligned file(s), ",
    n_fused_dates, " internal date(s); skipped ", n_skipped_misaligned,
    " misaligned file(s)"
  )

  if (!length(sum_vec)) {
    stop(
      "VI parquet assets produced no fused values",
      if (n_skipped_misaligned > 0L) {
        paste0(" (skipped ", n_skipped_misaligned, " misaligned files)")
      } else {
        ""
      }
    )
  }
  mean_values <- data.frame(
    pid = as.integer(names(sum_vec)),
    value = as.numeric(sum_vec / count_vec)
  )
  domain_path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$static,
    file = "domain.tif",
    dest_dir = temp_directory,
    overwrite = FALSE
  )
  domain <- terra::rast(domain_path)
  pid_layer <- if ("pid" %in% names(domain)) domain[["pid"]] else domain[[1]]

  out <- pid_layer
  terra::values(out) <- NA_real_
  pid_vals <- terra::values(pid_layer)[, 1]
  idx <- match(mean_values$pid, pid_vals)
  keep <- !is.na(idx) & !is.na(mean_values$value)
  terra::values(out)[idx[keep]] <- scale_evi_integer(mean_values$value[keep])
  names(out) <- "evi"
  out <- terra::mask(out, mask = out, maskvalue = 0)
  out
}

#' Mean EVI for the current calendar month (fuse-then-mean)
#'
#' Reads `vi_modis_parquet` (+ `vi_viirs_parquet` when published). Parquet
#' schema: pid, date, sensor, value (EVI × 100). Returns a SpatRaster on the
#' domain grid in EVI units (value / 100), matching get_most_recent_evi.tif.
get_monthly_mean_evi.tif <- function(env_files,
                                     temp_directory = "data/temp/",
                                     current_month) {
  if (missing(current_month) || is.null(current_month)) {
    stop(
      "get_monthly_mean_evi.tif(): current_month is required ",
      "(calendar month 1–12 of most_recent_evi_date)"
    )
  }
  month_int <- as.integer(current_month)[[1]]
  if (is.na(month_int) || month_int < 1L || month_int > 12L) {
    stop(
      "get_monthly_mean_evi.tif(): current_month must be 1–12; got ",
      paste(deparse(current_month), collapse = " ")
    )
  }

  vi_files <- list_vi_parquet_assets(
    env_files = env_files,
    current_month = month_int
  )

  if (nrow(vi_files) == 0L) {
    stop(
      "No vi_*_YYYYMMDD.parquet assets found for month ",
      month_int,
      " on tags ",
      ENVDATA_RELEASE_TAGS$vi_modis_parquet,
      " / ",
      ENVDATA_RELEASE_TAGS$vi_viirs_parquet
    )
  }

  months_kept <- sort(unique(as.integer(lubridate::month(vi_files$file_date))))
  if (!identical(months_kept, month_int)) {
    stop(
      "get_monthly_mean_evi.tif(): expected only month ", month_int,
      " but parquet dates have month(s) ",
      paste(months_kept, collapse = ",")
    )
  }

  families <- unique(vi_files$family)
  message(
    "Monthly mean EVI fuse-then-mean (month=", month_int, ") using: ",
    paste(families, collapse = "+"),
    " (", nrow(vi_files), " parquet files, ",
    length(unique(vi_files$file_date)), " composite dates; ",
    "monthly_climatology_v3_content_align)"
  )

  evi_mean_from_parquet_files(
    vi_files,
    temp_directory = temp_directory,
    current_month = month_int
  )
}
