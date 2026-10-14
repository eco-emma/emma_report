library(tidyverse)
library(lubridate)
library(arrow)

#' Download GSOD stations from emma_envdata `weather_data`
#'
#' No live NOAA / GSODR harvest on report CI. Missing `stations.gpkg` fails loud.
#' Empty (zero-row) stations → succeed; Quarto empty-weather guards apply.
#'
#' @param temp_directory Download directory
#' @param protected_areas Ignored (stations are pre-filtered on envdata)
#' @return sf of stations (WGS84 or as published)
get_stations_data <- function(temp_directory, protected_areas = NULL) {
  if (!dir.exists(temp_directory)) {
    dir.create(temp_directory, recursive = TRUE, showWarnings = FALSE)
  }

  path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$weather,
    file = "stations.gpkg",
    dest_dir = temp_directory,
    overwrite = TRUE
  )

  stations <- sf::st_read(path, quiet = TRUE)
  need <- ENVDATA_REPORT_ASSETS$weather$station_cols
  missing_cols <- setdiff(need, names(stations))
  if (length(missing_cols) > 0L) {
    stop(
      "stations.gpkg missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  message(
    "Downloaded ", nrow(stations),
    " stations from emma_envdata@weather_data/stations.gpkg"
  )
  stations
}

#' Download GSOD weather table from emma_envdata `weather_data`
#'
#' Download-only. No live `get_GSOD` / GITHUB_ACTIONS empty stub.
#' Missing `weather.parquet` → fail loud. Zero rows → empty tibble (allowed).
#'
#' @param protected_areas Ignored (kept for call-site compatibility)
#' @param stations Ignored (kept for call-site compatibility)
#' @param temp_directory Download directory
#' @return tibble of weather rows
update_climate_data_gsod <- function(protected_areas = NULL,
                                     stations = NULL,
                                     temp_directory) {
  if (!dir.exists(temp_directory)) {
    dir.create(temp_directory, recursive = TRUE, showWarnings = FALSE)
  }

  path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$weather,
    file = "weather.parquet",
    dest_dir = temp_directory,
    overwrite = TRUE
  )

  weather_data <- arrow::read_parquet(path)
  if (!inherits(weather_data, "data.frame")) {
    stop("weather.parquet did not read as a data frame", call. = FALSE)
  }

  need <- ENVDATA_REPORT_ASSETS$weather$weather_cols
  if (nrow(weather_data) > 0L) {
    missing_cols <- setdiff(need, names(weather_data))
    if (length(missing_cols) > 0L) {
      stop(
        "weather.parquet missing required columns: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    message(
      "weather.parquet is empty (0 rows); returning empty tibble ",
      "(Quarto empty-weather path)"
    )
    return(tibble::tibble())
  }

  message(
    "Downloaded ", nrow(weather_data),
    " weather rows from emma_envdata@weather_data/weather.parquet"
  )
  tibble::as_tibble(weather_data)
}
