library(stars)
library(tidyverse)
library(lubridate)
library(arrow)
library(SPEI)
library(ggplot2)
source("R/get_park_polygons.R")

library(targets)
# tar_load(monthly_mean_ndvi)
# tar_load(most_recent_ndvi_date)
# tar_load(parks)
# tar_load(remnants)
# tar_load(park_fire_history)

source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")
source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_upload.R")

#' @param invasive_age_months The number of months for which to include inat data as points.  Older data are converted to hex bins.
#' @param invasive_taxa Taxa names to filter the inat data by.  Works by str_detect, so Genera or partial names are fine. If NULL will use all taxa
# get_current_envdata <- function(
#                             monthly_mean_ndvi = monthly_mean_ndvi,
#                             most_recent_ndvi_date= most_recent_ndvi_date,
#                             time_window_days = 120,
#                             n_stations = 3,
#                             parks,
#                             remnants,
#                             park_fire_history,
#                             sleep_time = 10,
#                             max_attempts = 10,
#                             tag = "current",
#                             min_date = "1970-01-01",
#                             invasive_age_months = 3,
#                             invasive_taxa = c("Acacia", "Pinus", "Hakea", "Eucalyptus", "Leptospermum"),
#                             verbose=TRUE,
#                              ...
# ){


get_temp_directory <- function(temp_directory = "data/temp") {
  #    if(dir.exists(file.path(temp_directory))){
  #      unlink(file.path(temp_directory),recursive = TRUE,force = TRUE) }
  # Create temp directory (needs to come after get_park_polygons if using the same temp_directory, since the temp folder is deleted)
  if (!dir.exists(file.path(temp_directory))) {
    dir.create(file.path(temp_directory), recursive = TRUE)
  }
  return(temp_directory)
}

# Get outputs from model

# model_results <- tar_load(model_results)

# model_prediction <- tar_load(model_prediction)

# spatial_outputs <- tar_load(spatial_outputs)


# Get list of available env data files
get_env_files <- function(repo = "AdamWilsonLab/emma_envdata") {
  env_files <- pb_list(repo)
  return(env_files)
}

# Get list of available report files
get_report_files <- function(repo = "AdamWilsonLab/emma_report", create_park_tag = T) {
  report_files <- pb_list(repo)

  # create output release if needed
  park_data_tag <- "park_data"
  if (create_park_tag & !park_data_tag %in% report_files$tag) {
    pb_release_create(repo,
      tag = park_data_tag
    )
  }


  return(report_files)
}


# get most recent fire data
get_years_since_fire.tif <- function(env_files, temp_directory, most_recent_fire_date, piggyback_push) {
  env_files %>%
    filter(tag == "processed_most_recent_burn_dates") %>%
    mutate(file_date = gsub(pattern = ".tif", replacement = "", x = file_name)) %>%
    mutate(file_date = gsub(pattern = "_", replacement = "-", x = file_date)) %>%
    slice(which.max(as_date(file_date))) -> most_recent_fire_file

  robust_pb_download(
    file = most_recent_fire_file$file_name,
    dest = file.path(temp_directory),
    repo = "AdamWilsonLab/emma_envdata",
    tag = most_recent_fire_file$tag,
    max_attempts = max_attempts,
    sleep_time = 10
  )

  most_recent_fire.tif <- terra::rast(file.path(
    temp_directory,
    most_recent_fire_file$file_name
  ))

  most_recent_fire.tif[most_recent_fire.tif == 0] <- NA # toss NAs

  # convert from date of fire to years since fire

  years_since_fire.tif <-
    terra::app(
      x = most_recent_fire.tif,
      fun = function(x) {
        return(time_length(Sys.Date() - as_date(x, origin = lubridate::origin), unit = "years"))
      }
    )

  if (piggyback_push) {
    robust_pb_upload(
      file = file.path(temp_directory, "years_since_fire.tif"),
      repo = "AdamWilsonLab/emma_report",
      tag = tag,
      max_attempts = 10,
      sleep_time = 10,
      temp_directory = temp_directory,
      overwrite = TRUE
    )
  }

  return(years_since_fire.tif)
}

# crop years since fire raster to the remnants
generate_fires_vector <- function(years_since_fire.tif) {
  remnants <- terra::rast("data/misc/remnants.tif")

  years_since_fire.tif <- terra::mask(years_since_fire.tif, remnants)

  # make a polygon version and convert to WGS84 (for plotting ease)

  fires_wgs <- terra::as.polygons(x = years_since_fire.tif) %>%
    st_as_sf() %>%
    rename(Years = lyr.1) %>%
    st_transform(crs = st_crs(4326))

  return(fires_wgs)
}

# get most recent NDVI data
get_most_recent_ndvi_file <- function(env_files) {
  env_files %>%
    filter(tag == "clean_ndvi_modis") %>%
    filter(grepl(pattern = ".tif", x = file_name)) %>%
    mutate(file_date = gsub(pattern = ".tif", replacement = "", x = file_name)) %>%
    mutate(file_date = gsub(pattern = "_", replacement = "-", x = file_date)) %>%
    slice(which.max(as_date(file_date))) -> most_recent_ndvi_file

  return(most_recent_ndvi_file)
}

get_most_recent_ndvi.tif <- function(most_recent_ndvi_file, temp_directory) {
  robust_pb_download(
    file = most_recent_ndvi_file$file_name,
    dest = file.path(temp_directory),
    repo = "AdamWilsonLab/emma_envdata",
    tag = most_recent_ndvi_file$tag,
    max_attempts = max_attempts,
    sleep_time = 10
  )

  # Load the NDVI raster

  most_recent_ndvi.tif <- terra::rast(file.path(
    temp_directory,
    most_recent_ndvi_file$file_name
  ))

  # most_recent_ndvi.tif <- terra::rast(file.path(temp_directory,"ndvi.tif"))
  # Fix the NDVI values

  most_recent_ndvi.tif <- (most_recent_ndvi.tif / 100) - 1

  most_recent_ndvi.tif[most_recent_ndvi.tif > 1] <- 1
  most_recent_ndvi.tif[most_recent_ndvi.tif < -1] <- -1

  most_recent_ndvi.tif %>%
    terra::mask(
      mask = most_recent_ndvi.tif,
      maskvalue = 0
    ) -> most_recent_ndvi.tif

  return(most_recent_ndvi.tif)
}

# Get NDVI date
get_most_recent_ndvi_date <- function(most_recent_ndvi_file) {
  most_recent_ndvi_file %>%
    mutate(file_name = gsub(pattern = ".tif", replacement = "", x = file_name)) %>%
    pull(file_name) %>%
    as_date() -> most_recent_ndvi_date

  return(most_recent_ndvi_date)
}

# Monthly mean NDVI
# get_monthly_mean_ndvi_raster <- function(monthly_mean_ndvi, temp_directory){
#     robust_pb_download(file = monthly_mean$filename,
#                        dest = file.path(temp_directory),
#                        repo = monthly_mean_ndvi$repo,
#                        tag = monthly_mean_ndvi$tag,
#                        overwrite = TRUE,
#                        max_attempts = max_attempts,
#                        sleep_time = 10)
#
#     monthly_mean_ndvi_raster <- terra::rast(file.path(temp_directory,monthly_mean_ndvi$filename))
#
#     return(monthly_mean_ndvi_raster)
# }

# Create delta NDVI raster

get_monthly_delta_ndvi.tif <- function(most_recent_ndvi.tif, monthly_mean_ndvi.tif) {
  monthly_delta_ndvi.tif <- (most_recent_ndvi.tif - monthly_mean_ndvi.tif)

  if (crs(most_recent_ndvi.tif, proj = TRUE) != crs(monthly_mean_ndvi.tif, proj = TRUE)) {
    stop("NDVI CRS mismatch")
  }

  if (terra::ext(most_recent_ndvi.tif) != terra::ext(monthly_mean_ndvi.tif)) {
    stop("NDVI extent mismatch")
  }


  if (crs(monthly_delta_ndvi.tif, proj = TRUE) != crs(most_recent_ndvi.tif, proj = TRUE)) {
    crs(monthly_delta_ndvi.tif) <- crs(most_recent_ndvi.tif)
  }

  # Double check projections

  if (crs(most_recent_ndvi.tif, proj = TRUE) !=
    crs(monthly_mean_ndvi.tif, proj = TRUE)) {
    stop("NDVI layers have different projections")
  }

  if (crs(most_recent_ndvi.tif, proj = TRUE) !=
    crs(monthly_delta_ndvi.tif, proj = TRUE)) {
    stop("NDVI layers have different projections")
  }

  # Write monthly delta NDVI layer

  return(monthly_delta_ndvi.tif)
  # Upload delta NDVI in case anyone wants it

  # robust_pb_upload(file = file.path(temp_directory,"monthly_delta_NDVI.tif"),
  #                   repo = "AdamWilsonLab/emma_report",
  #                   tag = "current",
  #                   max_attempts = max_attempts,
  #                   sleep_time = 10,
  #                   temp_directory = temp_directory,
  #                   overwrite = TRUE)
  # return(monthly_delta_ndvi.tif)
}


# MODIS NDWI
## Commenting NDWI out for now, as gee hasn't updated to the new version yet.
## uncomment below and adjust the corresponding bits of report_prototype.qmd if this changes

# robust_pb_download(file = "ndwi.tif",
#                    tag = tag,
#                    dest = file.path(temp_directory),
#                    repo = "AdamWilsonLab/emma_envdata",
#                    max_attempts = max_attempts,
#                    sleep_time = sleep_time)
#
# ndwi_rast <- raster::raster(terra::rast(file.path(temp_directory,"ndwi.tif"))) %>%
#   round(digits = 2) #round to save memory

# MODIS NDWI date

# env_files %>%
#   filter(file_name == "ndwi.tif")%>%
#   pull(timestamp) %>%
#   as_date() -> most_recent_ndwi_date

# Other drought layers?


# return(
#   most_recent_fire_raster,
#   years_since_fire_raster,
#   fires_wgs,
#   most_recent_ndvi_raster,
#   most_recent_ndvi_date,
#   monthly_mean_ndvi_raster,
#   monthly_delta_NDVI,
#   mean_ndvi_raster,
#   delta_ndvi_raster,
#   most_recent_quarter_ndvi_file,
#   quarterly_delta_ndvi_raster,
#   stations_sf,
# )

# }#end fx

# ===================================
# library(stars)
# library(tidyverse)
# library(lubridate)
# library(arrow)
# library(SPEI)
# library(ggplot2)
# source("R/get_park_polygons.R")

# library(targets)
# #tar_load(monthly_mean_ndvi)
# #tar_load(most_recent_ndvi_date)
# #tar_load(parks)
# #tar_load(remnants)
# #tar_load(park_fire_history)

# source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")
# source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_upload.R")

# #' @param invasive_age_months The number of months for which to include inat data as points.  Older data are converted to hex bins.
# #' @param invasive_taxa Taxa names to filter the inat data by.  Works by str_detect, so Genera or partial names are fine. If NULL will use all taxa
# # get_current_envdata <- function(
# #                             monthly_mean_ndvi = monthly_mean_ndvi,
# #                             most_recent_ndvi_date= most_recent_ndvi_date,
# #                             time_window_days = 120,
# #                             n_stations = 3,
# #                             parks,
# #                             remnants,
# #                             park_fire_history,
# #                             sleep_time = 10,
# #                             max_attempts = 10,
# #                             tag = "current",
# #                             min_date = "1970-01-01",
# #                             invasive_age_months = 3,
# #                             invasive_taxa = c("Acacia", "Pinus", "Hakea", "Eucalyptus", "Leptospermum"),
# #                             verbose=TRUE,
# #                              ...
# # ){


#   get_temp_directory <- function(temp_directory = "data/temp"){
# #    if(dir.exists(file.path(temp_directory))){
# #      unlink(file.path(temp_directory),recursive = TRUE,force = TRUE) }
#     #Create temp directory (needs to come after get_park_polygons if using the same temp_directory, since the temp folder is deleted)
#     if(!dir.exists(file.path(temp_directory))){
#       dir.create(file.path(temp_directory),recursive = TRUE)    }
#       return(temp_directory)
#     }

#   # Get outputs from model

#     #model_results <- tar_load(model_results)

#     #model_prediction <- tar_load(model_prediction)

#     #spatial_outputs <- tar_load(spatial_outputs)


# #   # Get list of available env data files
# get_env_files <- function(repo="AdamWilsonLab/emma_envdata"){
#     env_files <- pb_list(repo)
#     return(env_files)
# }
# # # A proper get_env_files() that only returns file paths
# #   get_env_files <- function(repo="AdamWilsonLab/emma_envdata") {
# #     # Find all .tif files under the directory
# #     paths <- list.files(
# #       dir, pattern = "(?i)\\.tif$", full.names = TRUE, recursive = TRUE
# #     )

# #     # Ensure character vector
# #     paths <- as.character(paths)

# #     # Safety check: stop if no files
# #     if (length(paths) == 0L) {
# #       stop("No .tif files found in directory: ", dir)
# #     }

# #     # Safety check: stop if any missing files
# #     if (!all(file.exists(paths))) {
# #       missing <- paths[!file.exists(paths)]
# #       stop("Missing files: ", paste(missing, collapse = ", "))
# #     }

# #     # Return vector of file paths
# #     paths
# #   }

#   # Get list of available report files
# get_report_files <- function(repo = "AdamWilsonLab/emma_report",create_park_tag=T){
#     report_files <- pb_list(repo)

#     # create output release if needed
#     park_data_tag = "park_data"
#     if(create_park_tag & !park_data_tag %in% report_files$tag){

#       pb_release_create(repo,
#                         tag = park_data_tag)

#     }


#         return(report_files)
# }


#   #get most recent fire data
# get_years_since_fire.tif <- function(env_files, temp_directory, most_recent_fire_date,piggyback_push){
#     env_files %>%
#       filter(tag == "processed_most_recent_burn_dates") %>%
#       mutate(file_date = gsub(pattern = ".tif",replacement = "",x = file_name)) %>%
#       mutate(file_date = gsub(pattern = "_",replacement = "-",x = file_date)) %>%
#       slice(which.max(as_date(file_date))) -> most_recent_fire_file

#     robust_pb_download(file = most_recent_fire_file$file_name,
#                   dest = file.path(temp_directory),
#                   repo = "AdamWilsonLab/emma_envdata",
#                   tag = most_recent_fire_file$tag,
#                   max_attempts = max_attempts,
#                   sleep_time = 10)

#     most_recent_fire.tif <- terra::rast(file.path(temp_directory,
#                                                      most_recent_fire_file$file_name))

#     most_recent_fire.tif[most_recent_fire.tif == 0] <- NA #toss NAs

# convert from date of fire to years since fire

#     years_since_fire.tif <-
#       terra::app(x = most_recent_fire.tif,
#                  fun = function(x){
#                    return( time_length(Sys.Date() - as_date(x,origin = lubridate::origin),unit = "years"))
#                  })

#   if(piggyback_push)  robust_pb_upload(file = file.path(temp_directory,"years_since_fire.tif"),
#                      repo = "AdamWilsonLab/emma_report",
#                      tag = tag,
#                      max_attempts = 10,
#                      sleep_time = 10,
#                      temp_directory = temp_directory,
#                      overwrite = TRUE)

#     return(years_since_fire.tif)
# }

#   # crop years since fire raster to the remnants
# generate_fires_vector <- function(years_since_fire.tif){
#     remnants <- terra::rast("data/misc/remnants.tif")

# years_since_fire.tif %>%
#   terra::mask(remnants) -> years_since_fire.tif

#   # make a polygon version and convert to WGS84 (for plotting ease)

#     fires_wgs <- terra::as.polygons(x = years_since_fire.tif) %>%
#       st_as_sf() %>%
#       rename(Years = lyr.1) %>%
#       st_transform(crs = st_crs(4326))

#     return(fires_wgs)

# }

# #   # get most recent NDVI data
# # get_most_recent_ndvi_file <- function(env_files){
# #     env_files %>%
# #       filter(tag == "clean_ndvi_modis") %>%
# #       filter(grepl(pattern = ".tif",x = file_name)) %>%
# #       mutate(file_date = gsub(pattern = ".tif",replacement = "",x = file_name)) %>%
# #       mutate(file_date = gsub(pattern = "_",replacement = "-",x = file_date)) %>%
# #       slice(which.max(as_date(file_date))) -> most_recent_ndvi_file

# #   return(most_recent_ndvi_file)
# # }

# get_most_recent_ndvi_file <- function(env_files) {
#   stopifnot(is.data.frame(env_files), all(c("tag","file_name") %in% names(env_files)))

#   env_files %>%
#     filter(tag == "clean_ndvi_modis") %>%
#     filter(str_detect(file_name, "(?i)\\.tif$")) %>%
#     mutate(
#       date_str = str_match(file_name, "(\\d{4})[-_]?(\\d{2})[-_]?(\\d{2})")[,1],
#       file_date = suppressWarnings(ymd(gsub("[-_]", "", date_str)))
#     ) %>%
#     filter(!is.na(file_date)) %>%
#     arrange(desc(file_date), desc(file_name)) %>%
#     slice(1)
# }

# # get_most_recent_ndvi_file <- function(env_files) {
# #   stopifnot(is.data.frame(env_files), all(c("tag","file_name") %in% names(env_files)))

# #   out <- env_files %>%
# #     dplyr::filter(.data$tag == "clean_ndvi_modis") %>%
# #     dplyr::filter(stringr::str_detect(.data$file_name, "(?i)\\.tif$")) %>%
# #     dplyr::mutate(
# #       date_str  = stringr::str_match(.data$file_name, "(\\d{4})[-_]?([0-1]\\d)[-_]?([0-3]\\d)")[, 1],
# #       file_date = suppressWarnings(lubridate::ymd(date_str))
# #     ) %>%
# #     dplyr::filter(!is.na(.data$file_date)) %>%
# #     dplyr::arrange(dplyr::desc(.data$file_date), dplyr::desc(.data$file_name)) %>%
# #     dplyr::slice(1)

# #   if (nrow(out) != 1L) {
# #     stop("NDVI .tif을 찾지 못했습니다. tag == 'clean_ndvi_modis'를 확인하세요.")
# #   }
# #   if (!nzchar(out$file_name[[1]])) {
# #     stop("most_recent_ndvi_file$file_name 이 비어 있습니다.")
# #   }
# #   out
# # }

# get_most_recent_ndvi.tif <- function(most_recent_ndvi_file, temp_directory) {
#   # 1) 파일 다운로드
#   robust_pb_download(
#     file = most_recent_ndvi_file$file_name,
#     dest = file.path(temp_directory),
#     repo = "AdamWilsonLab/emma_envdata",
#     tag  = most_recent_ndvi_file$tag,
#     max_attempts = max_attempts,
#     sleep_time   = 10
#   )

#   # 2) 래스터 로드
#   r <- terra::rast(file.path(temp_directory, most_recent_ndvi_file$file_name))

#   # 3) 스케일/클리닝(주석에 있던 내용 복원)
#   r <- (r / 100) - 1
#   r[r >  1] <-  1
#   r[r < -1] <- -1
#   r <- terra::mask(r, mask = r, maskvalue = 0)

#   # 4) SpatRaster 반환 (중요!)
#   return(r)
# }


#   # # Load the NDVI raster

#   #   most_recent_ndvi.tif <- terra::rast(file.path(temp_directory,
#   #                                                    most_recent_ndvi_file$file_name))

#   #   #most_recent_ndvi.tif <- terra::rast(file.path(temp_directory,"ndvi.tif"))
#   # # Fix the NDVI values

#   #   most_recent_ndvi.tif <- (most_recent_ndvi.tif/100)-1

#   #   most_recent_ndvi.tif[most_recent_ndvi.tif > 1] <- 1
#   #   most_recent_ndvi.tif[most_recent_ndvi.tif < -1] <- -1

#   #   most_recent_ndvi.tif %>%
#   #     terra::mask(mask = most_recent_ndvi.tif,
#   #                 maskvalue = 0) -> most_recent_ndvi.tif

#   #   return(most_recent_ndvi.tif)
#   #   }

#   # Get NDVI date
# get_most_recent_ndvi_date <- function(most_recent_ndvi_file){
#     most_recent_ndvi_file %>%
#       mutate(file_name = gsub(pattern = ".tif",replacement="",x=file_name))%>%
#       pull(file_name)%>%
#       as_date() -> most_recent_ndvi_date

#     return(most_recent_ndvi_date)
# }

#   # Monthly mean NDVI
# # get_monthly_mean_ndvi_raster <- function(monthly_mean_ndvi, temp_directory){
# #     robust_pb_download(file = monthly_mean$filename,
# #                        dest = file.path(temp_directory),
# #                        repo = monthly_mean_ndvi$repo,
# #                        tag = monthly_mean_ndvi$tag,
# #                        overwrite = TRUE,
# #                        max_attempts = max_attempts,
# #                        sleep_time = 10)
# #
# #     monthly_mean_ndvi_raster <- terra::rast(file.path(temp_directory,monthly_mean_ndvi$filename))
# #
# #     return(monthly_mean_ndvi_raster)
# # }

#     # Create delta NDVI raster

# get_monthly_delta_ndvi.tif <- function(most_recent_ndvi.tif,monthly_mean_ndvi.tif) {
#   monthly_delta_ndvi.tif <- (most_recent_ndvi.tif - monthly_mean_ndvi.tif)

#     if(crs(most_recent_ndvi.tif,proj=TRUE) != crs(monthly_mean_ndvi.tif,proj=TRUE)){

#       stop("NDVI CRS mismatch")

#     }

#     if(terra::ext(most_recent_ndvi.tif) != terra::ext(monthly_mean_ndvi.tif)){

#       stop("NDVI extent mismatch")

#     }


#     if(crs(monthly_delta_ndvi.tif,proj=TRUE) != crs(most_recent_ndvi.tif,proj=TRUE)){

#       crs(monthly_delta_ndvi.tif) <- crs(most_recent_ndvi.tif)
#     }

#     # Double check projections

#     if(crs(most_recent_ndvi.tif, proj = TRUE) !=
#        crs(monthly_mean_ndvi.tif, proj = TRUE)){
#       stop("NDVI layers have different projections")
#     }

#     if(crs(most_recent_ndvi.tif, proj = TRUE) !=
#        crs(monthly_delta_ndvi.tif, proj = TRUE)){
#       stop("NDVI layers have different projections")
#     }

#     # Write monthly delta NDVI layer

#     return(monthly_delta_ndvi.tif)
#     # Upload delta NDVI in case anyone wants it

#    # robust_pb_upload(file = file.path(temp_directory,"monthly_delta_NDVI.tif"),
#    #                   repo = "AdamWilsonLab/emma_report",
#    #                   tag = "current",
#    #                   max_attempts = max_attempts,
#    #                   sleep_time = 10,
#    #                   temp_directory = temp_directory,
#    #                   overwrite = TRUE)
# #return(monthly_delta_ndvi.tif)

#         }


#   # MODIS NDWI
#             ## Commenting NDWI out for now, as gee hasn't updated to the new version yet.
#             ## uncomment below and adjust the corresponding bits of report_prototype.qmd if this changes

#       # robust_pb_download(file = "ndwi.tif",
#       #                    tag = tag,
#       #                    dest = file.path(temp_directory),
#       #                    repo = "AdamWilsonLab/emma_envdata",
#       #                    max_attempts = max_attempts,
#       #                    sleep_time = sleep_time)
#       #
#       # ndwi_rast <- raster::raster(terra::rast(file.path(temp_directory,"ndwi.tif"))) %>%
#       #   round(digits = 2) #round to save memory

#     # MODIS NDWI date

#       # env_files %>%
#       #   filter(file_name == "ndwi.tif")%>%
#       #   pull(timestamp) %>%
#       #   as_date() -> most_recent_ndwi_date

#   # Other drought layers?


# # return(
# #   most_recent_fire_raster,
# #   years_since_fire_raster,
# #   fires_wgs,
# #   most_recent_ndvi_raster,
# #   most_recent_ndvi_date,
# #   monthly_mean_ndvi_raster,
# #   monthly_delta_NDVI,
# #   mean_ndvi_raster,
# #   delta_ndvi_raster,
# #   most_recent_quarter_ndvi_file,
# #   quarterly_delta_ndvi_raster,
# #   stations_sf,
# # )

# #}#end fx
