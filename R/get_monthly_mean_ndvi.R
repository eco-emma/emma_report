library(terra)
# current_month <- month(Sys.Date())

#' This function calculates the mean ndvi for the current month from the NDVI releases
get_monthly_mean_ndvi.tif <- function(env_files,
                                      temp_directory = "data/temp/",
                                      current_month = current_month) {
  # Filter to NDVI records from the relevant month

  env_files %>%
    filter(grepl(
      pattern = "parquet-ndvi",
      x = file_name
    )) -> env_files

  # Convert numeric to date

  env_files %>%
    mutate(unix_date = gsub(
      pattern = "-dynamic_parquet-ndvi-",
      replacement = "",
      x = file_name
    )) %>%
    mutate(unix_date = gsub(
      pattern = ".gz.parquet",
      replacement = "",
      x = unix_date
    )) -> env_files

  env_files %>%
    mutate(month = month(as_date(as.numeric(unix_date)))) -> env_files


  # Filter to relevant month

  env_files %>%
    filter(month == current_month) -> env_files

  # Download files

  robust_pb_download(
    file = env_files$file_name,
    tag = "current",
    repo = "AdamWilsonLab/emma_envdata",
    dest = file.path(temp_directory),
    max_attempts = 10,
    sleep_time = 10
  )

  # Open Dataset and calculate mean

  open_dataset(file.path(temp_directory, env_files$file_name)) %>%
    group_by(cellID) %>%
    summarize(value = mean(value)) %>%
    mutate(value = ifelse(value > 2000, NA, value)) |>
    collect() -> mean_values

  # Get template

  robust_pb_download(
    file = "template.tif",
    tag = "processed_static",
    repo = "AdamWilsonLab/emma_envdata",
    dest = file.path(temp_directory),
    max_attempts = 10,
    sleep_time = 10
  )

  template <- rast(file.path(temp_directory, "template.tif"))

  # Make mean raster from template

  values(template) <- NA

  values(template)[mean_values$cellID] <- mean_values$value
  # template <- classify(template, cbind(21474836,NA ))

  scoff(template) <- cbind(0.01, -1)
  # terra::NAflag(template) <- 21474836
  # scale and offset
  #      template <- (template/100)-1
  template %>%
    terra::mask(
      mask = template,
      maskvalue = 0, datatype = "INT4S"
    ) -> template
  plot(template)

  # write file

  return(template) # %>%
  # writeRaster(filename = file.path(temp_directory,paste(current_month,"_monthly_mean_ndvi.tif",sep = "")),
  #             overwrite=TRUE)

  # Upload

  # pb_upload(file = file.path(temp_directory,paste(current_month,"_monthly_mean_ndvi.tif",sep = "")),
  #           repo = "AdamWilsonLab/emma_report",
  #           tag = "current")


  # cleanup

  # unlink(file.path(temp_directory),recursive = TRUE,force = TRUE)

  # return metadata

  # output <- data.frame(filename =paste(current_month,"_monthly_mean_ndvi.tif",sep = ""),
  #            repo = "AdamWilsonLab/emma_report",
  #            tag = "current")
  #

  #  return(output)
}
