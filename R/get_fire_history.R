# source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")
# source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_upload.R")


get_fire_history <- function(temp_directory = "data/temp/fire_history",
                             max_attempts = 10,
                             sleep_time = 10,
                             protected_areas = protected_areas) {
  # ensure directories are empty

  # if(dir.exists(file.path(temp_directory))){
  #
  #   unlink(file.path(temp_directory),recursive = TRUE,force = TRUE)
  #
  # }
  #
  # if(!dir.exists(file.path(temp_directory))){
  #
  #   dir.create(file.path(temp_directory), recursive = TRUE)
  #
  # }

  # Grab fire data

  env_files <- pb_list(repo = "AdamWilsonLab/emma_envdata")

  fire_files <-
    env_files %>%
    filter(grepl(pattern = "time_since_fire", x = file_name))

  robust_pb_download(
    file = fire_files$file_name,
    dest = temp_directory,
    repo = "AdamWilsonLab/emma_envdata",
    tag = "current",
    overwrite = TRUE,
    max_attempts = max_attempts,
    sleep_time = sleep_time
  )

  # Grab template

  robust_pb_download(
    file = "template.tif",
    dest = temp_directory,
    repo = "AdamWilsonLab/emma_envdata",
    tag = "processed_static",
    overwrite = TRUE,
    max_attempts = max_attempts,
    sleep_time = sleep_time
  )

  template <- rast(file.path(temp_directory, "template.tif"))

  # Get pixels that are within parks

  all_parks <- st_as_sf(protected_areas)


  park_cells <-
    template %>%
    terra::extract(
      y = vect(all_parks),
      touches = TRUE
    )

  # Extract relevant data

  fire_dataset <- open_dataset(sources = file.path(temp_directory, fire_files$file_name)) %>%
    rename(days_since_fire = value) %>%
    dplyr::select(-variable) %>%
    filter(cellID %in% park_cells$template) %>%
    collect()

  # Write data (if needed)

  # fire_dataset %>%
  # write_parquet(sink = file.path(temp_directory,"park_cell_vegetation_ages.gz.parquet"),
  #               compression = "gzip")


  # Cleanup

  # unlink(file.path(temp_directory),recursive = TRUE,force = TRUE)

  # Return data

  return(fire_dataset)
}
