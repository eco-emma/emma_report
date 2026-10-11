library(GSODR)
library(tidyverse)
library(lubridate)

# load for interactive testing
if (F) tar_load(temp_directory, protected_areas)

# Get the weather station metadata
get_stations_data <- function(temp_directory, protected_areas) {
  # update station md
  update_station_list_nonint()

  # Get list of stations
  load(system.file("extdata", "isd_history.rda", package = "GSODR"))

  # only take stations with data within South Africa

  isd_history %>%
    filter(COUNTRY_NAME == "SOUTH AFRICA") -> isd_history

  # only take stations with relatively recent data

  isd_history %>%
    separate(
      col = END,
      into = c("end_year", "end_month", "end_day"),
      sep = c(4, 6, 8), remove = FALSE
    ) %>%
    separate(
      col = BEGIN,
      into = c("start_year", "start_month", "start_day"),
      sep = c(4, 6, 8), remove = FALSE
    ) %>%
    filter(end_year >= year(Sys.Date()) - 1) -> isd_history

  # filter by stations within 100 km of our parks

  isd_history %>%
    st_as_sf(
      coords = c("LON", "LAT"),
      remove = FALSE
    ) -> isd_history

  st_crs(isd_history) <- st_crs("WGS84")

  park_buffer <- st_as_sf(protected_areas) %>%
    st_buffer(dist = 100000) %>%
    st_union()

  isd_history %>%
    st_filter(y = park_buffer %>%
      st_transform(st_crs(isd_history))) -> isd_history

  rm(park_buffer)


  # update station metadata
  #  stations_sf <- st_transform(stations_sf, crs = st_crs(parks$cape_nature))
  stations <- isd_history |>
    st_transform(st_crs(protected_areas)) |>
    st_make_valid()

  return(stations)
}


update_climate_data_gsod <- function(protected_areas, stations, temp_directory) {
  # # Download station data
  for (i in 1:nrow(stations)) {
    station_i <- stations$STNID[i]

    message("Downloading data for ", station_i, ": station ", i, " of ", nrow(stations))

    data_i <- robust_get_GSOD(
      years = as.numeric(stations$start_year[i]):year(Sys.Date()),
      station = station_i,
      max_attempts = max_attempts,
      sleep_time = sleep_time
    )

    # move on if there was an issue in the download

    if (!inherits(data_i, "data.frame")) {
      next
    }

    # if the data are empty, throw an error

    if (nrow(data_i) < 1) {
      warning("No data downloaded")
      next
    }

    # if(nrow(data_i) < 1){ stop("No data downloaded") }

    # write data as a parquet file

    data_i %>%
      write_parquet(
        sink = file.path(temp_directory, paste("gsod_", station_i, ".gz.parquet", sep = "")),
        compression = "gzip"
      )

    # upload as a release

    #   robust_pb_upload(file = file.path(temp_directory,paste(station_i,".gz.parquet",sep = "")),
    #                    repo = "AdamWilsonLab/emma_report/",
    #                    tag = "GSOD",
    #                    overwrite = TRUE,
    #                    max_attempts = max_attempts,
    #                    sleep_time = sleep_time,
    #                    temp_directory = file.path(temp_directory,"/upload_check/"))
    #
    # #remove the parquet
    #   file.remove(file.path(temp_directory,paste(station_i,".gz.parquet",sep = "")))
  }

  weather_data <- open_dataset(list.files(temp_directory, pattern = "gsod_", full = T)) |>
    as_tibble()
  # clean up temp files

  # unlink(file.path(temp_directory),
  #        recursive = TRUE,
  #        force = TRUE)

  # clean up garbage

  gc()

  return(weather_data)
} # end fx


#########################

robust_get_GSOD <- function(years, station, max_attempts = 10, sleep_time = 1) {
  n_attempts <- 1

  while (n_attempts <= max_attempts) {
    message(
      "attempt ", n_attempts, " of ", max_attempts,
      " to download GSOD data for station ", station
    )

    out_data <- tryCatch(
      get_GSOD(
        years = years,
        station = station
      ),
      error = function(e) {
        e
      }
    )

    # if download completed, check that it contains data
    if (inherits(out_data, c("data.table", "data.frame"))) {
      if (nrow(out_data) > 0) {
        return(out_data)
      }

      if (nrow(out_data) == 0) {
        message("Empty dataframe downloaded, retrying")
      }
    } # if


    # send message if giving up

    if (n_attempts == max_attempts) {
      message(
        "Reached maximum download attempts for station ", station,
        ", giving up."
      )
    }

    # if it failed increment

    n_attempts <- n_attempts + 1

    # sleep if needed

    Sys.sleep(sleep_time)
  } # while loop


  return(out_data)
}
