library(GSODR)
library(tidyverse)
library(lubridate)

# load for interactive testing
if(F) tar_load(temp_directory,protected_areas)

# Get the weather station metadata
get_stations_data <- function(temp_directory, protected_areas){

  # Prefer a fresh NOAA station list; fall back to GSODR's packaged copy if
  # NCEI is down (HTTP 520 / content-type errors are common in CI).
  update_ok <- tryCatch(
    {
      update_station_list_nonint()
      TRUE
    },
    error = function(e) {
      warning(
        "Could not refresh NOAA isd-history.csv; using GSODR packaged station list. ",
        conditionMessage(e),
        call. = FALSE
      )
      FALSE
    }
  )
  if (!isTRUE(update_ok)) {
    message("Proceeding with packaged GSODR isd_history.rda")
  }

  # Get list of stations
  load(system.file("extdata", "isd_history.rda", package = "GSODR"))

  # only take stations with data within South Africa

  isd_history %>%
    filter(COUNTRY_NAME == "SOUTH AFRICA")-> isd_history

  # only take stations with relatively recent data

  isd_history %>%
    separate(col = END,
             into = c("end_year","end_month","end_day"),
             sep = c(4,6,8),remove = FALSE) %>%
    separate(col = BEGIN,
             into = c("start_year","start_month","start_day"),
             sep = c(4,6,8),remove = FALSE) %>%
    filter(end_year >= year(Sys.Date())-1) -> isd_history

  # filter by stations within 100 km of our parks

  isd_history %>%
    st_as_sf(coords = c("LON","LAT"),
             remove = FALSE )-> isd_history

  st_crs(isd_history) <- st_crs("WGS84")

  park_buffer <- st_as_sf(protected_areas) %>%
    st_buffer(dist = 100000) %>%
    st_union()

  isd_history %>%
    st_filter(y = park_buffer %>%
                st_transform(st_crs(isd_history))) -> isd_history

  rm(park_buffer)


  #update station metadata
#  stations_sf <- st_transform(stations_sf, crs = st_crs(parks$cape_nature))
 stations <- isd_history |>
    st_transform(st_crs(protected_areas)) |>
    st_make_valid()

   return(stations)

}


update_climate_data_gsod <- function(protected_areas,stations, temp_directory){

    # PR light CI: GSOD multi-station downloads (~30m) OOM/starve hosted runners.
    if (identical(Sys.getenv("EMMA_CI_LIGHT"), "1")) {
      message("EMMA_CI_LIGHT=1: skipping GSOD station downloads; empty weather_data")
      return(tibble::tibble())
    }

    # Reports discard weather before 1970; do not download decades of unused years.
    min_year <- 1970L
    this_year <- year(Sys.Date())

    # # Download station data
      for(i in 1:nrow(stations)){

        station_i <- stations$STNID[i]

        message("Downloading data for ",station_i,": station ", i, " of ", nrow(stations))

        yr_start <- max(as.integer(stations$start_year[i]), min_year, na.rm = TRUE)
        if (is.na(yr_start) || yr_start > this_year) {
          message("Skipping station ", station_i, ": no years in [", min_year, ", ", this_year, "]")
          next
        }

        data_i <- robust_get_GSOD(years = yr_start:this_year,
                           station = station_i,
                           max_attempts = max_attempts,
                           sleep_time = sleep_time)

        # move on if there was an issue in the download

        if(!inherits(data_i,"data.frame")){next}

        # if the data are empty, throw an error

        if(nrow(data_i) < 1){
          warning("No data downloaded")
          next
          }

        # if(nrow(data_i) < 1){ stop("No data downloaded") }

        # write data as a parquet file

          data_i %>%
            write_parquet(sink = file.path(temp_directory,paste("gsod_",station_i,".gz.parquet",sep = "")),
                          compression = "gzip")

        #upload as a release

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

    gsod_files <- list.files(temp_directory, pattern = "gsod_", full.names = TRUE)
    if (!length(gsod_files)) {
      warning("No GSOD parquet files downloaded; returning empty weather_data")
      return(tibble::tibble())
    }

    weather_data = open_dataset(gsod_files) |>
      collect()
      # clean up temp files

        # unlink(file.path(temp_directory),
        #        recursive = TRUE,
        #        force = TRUE)

      # clean up garbage

        gc()

      return(weather_data)



  }#end fx


#########################

robust_get_GSOD <- function(years, station, max_attempts=10, sleep_time = 1){

  years <- sort(unique(as.integer(years)))
  years <- years[!is.na(years)]
  if (!length(years)) {
    return(NULL)
  }

  n_attempts <- 1

  while(n_attempts <= max_attempts){

    message("attempt ", n_attempts, " of ", max_attempts,
            " to download GSOD data for station ", station,
            " years ", min(years), "-", max(years))

    out_data <- tryCatch(get_GSOD(years = years,
                                  station = station),
                         error = function(e){e})

    # if download completed, check that it contains data
      if(inherits(out_data, c("data.table","data.frame"))){


        if(nrow(out_data) > 0){ return(out_data) }

        if(nrow(out_data) == 0){ message("Empty dataframe downloaded, retrying") }

      }#if

    # Permanent year-window mismatch: adjust once instead of burning all retries.
    # GSODR: "This station, …, only provides data for years 2024 to 2024."
    if (inherits(out_data, "error")) {
      msg <- conditionMessage(out_data)
      m <- regexec("years ([0-9]{4}) to ([0-9]{4})", msg)
      r <- regmatches(msg, m)[[1]]
      if (length(r) == 3L) {
        avail <- as.integer(r[2]):as.integer(r[3])
        new_years <- intersect(years, avail)
        if (!length(new_years)) {
          message(
            "No overlapping GSOD years for station ", station,
            " (available ", min(avail), "-", max(avail), "); skipping"
          )
          return(out_data)
        }
        if (!identical(new_years, years)) {
          message(
            "Adjusting GSOD years for station ", station, " to ",
            min(new_years), "-", max(new_years)
          )
          years <- new_years
          n_attempts <- n_attempts + 1L
          next
        }
      }
    }


    # send message if giving up

      if(n_attempts == max_attempts){

        message("Reached maximum download attempts for station ", station,
                ", giving up.")

        }

    # if it failed increment

      n_attempts = n_attempts + 1

    # sleep if needed

      Sys.sleep(sleep_time)


  } #while loop


  return(out_data)


}
