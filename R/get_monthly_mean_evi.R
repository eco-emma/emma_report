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
    vi_files <- dplyr::filter(
      vi_files,
      lubridate::month(.data$file_date) == current_month
    )
  }

  vi_files
}

#' Download parquet assets and return fused-then-mean EVI SpatRaster
#'
#' For each (pid, composite_date): priority-fill sensors 4→3→1→2, then
#' `mean(..., na.rm = TRUE)` across dates for each pid.
#'
#' Processes one composite date at a time (running sum/count) so all-history
#' means do not load ~3 GB of parquets into memory at once.
evi_mean_from_parquet_files <- function(vi_files,
                                        temp_directory = "data/temp/") {
  if (!nrow(vi_files)) {
    stop("No VI parquet assets to aggregate")
  }

  for (i in seq_len(nrow(vi_files))) {
    download_envdata_asset(
      tag = vi_files$tag[[i]],
      file = vi_files$file_name[[i]],
      dest_dir = temp_directory,
      overwrite = FALSE
    )
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

  dates <- sort(unique(vi_files$file_date))
  for (d in dates) {
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
    fused <- fuse_evi_by_pid_date(raw)
    fused <- fused[!is.na(fused$value), , drop = FALSE]
    if (!nrow(fused)) {
      next
    }
    keys <- as.character(fused$pid)
    known <- keys %in% names(sum_vec)
    if (any(!known)) {
      sum_vec <- c(sum_vec, stats::setNames(fused$value[!known], keys[!known]))
      count_vec <- c(
        count_vec,
        stats::setNames(rep(1, sum(!known)), keys[!known])
      )
    }
    if (any(known)) {
      k <- keys[known]
      sum_vec[k] <- sum_vec[k] + fused$value[known]
      count_vec[k] <- count_vec[k] + 1
    }
    rm(raw, fused)
  }

  if (!length(sum_vec)) {
    stop("VI parquet assets produced no fused values")
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
                                     current_month = current_month) {
  vi_files <- list_vi_parquet_assets(
    env_files = env_files,
    current_month = current_month
  )

  if (nrow(vi_files) == 0L) {
    stop(
      "No vi_*_YYYYMMDD.parquet assets found for month ",
      current_month,
      " on tags ",
      ENVDATA_RELEASE_TAGS$vi_modis_parquet,
      " / ",
      ENVDATA_RELEASE_TAGS$vi_viirs_parquet
    )
  }

  families <- unique(vi_files$family)
  message(
    "Monthly mean EVI fuse-then-mean using: ",
    paste(families, collapse = "+"),
    " (", nrow(vi_files), " parquet files)"
  )

  evi_mean_from_parquet_files(vi_files, temp_directory = temp_directory)
}
