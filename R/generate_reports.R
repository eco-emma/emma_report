library(rmarkdown)
library(stars)
library(tidyverse)
library(lubridate)
library(arrow)
library(dygraphs)
library(rgee)
library(SPEI)
library(ggplot2)
#webshot::install_phantomjs()
source("R/get_park_polygons.R")

#library(targets)
#tar_load(monthly_mean_evi)
#tar_load(most_recent_evi_date)
#tar_load(parks)
#tar_load(remnants)
#tar_load(park_fire_history)
#source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")
#source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_upload.R")
#' @param invasive_age_months The number of months for which to include inat data as points.  Older data are converted to hex bins.
#' @param invasive_taxa Taxa names to filter the inat data by.  Works by str_detect, so Genera or partial names are fine. If NULL will use all taxa
generate_reports <- function(output_directory = "reports/",
                             temp_directory = "data/temp/reports",
                             temp_directory_evi = "data/temp/evi",
                             report_location = "report_prototype.qmd",
                             monthly_mean_evi = monthly_mean_evi,
                             most_recent_evi_date= most_recent_evi_date,
                             park_data_tag = "park_data",
                             time_window_days = 120,
                             n_stations = 3,
                             parks,
                             remnants,
                             park_fire_history,
                             sleep_time = 10,
                             max_attempts = 10,
                             tag = "current",
                             min_date = "1970-01-01",
                             invasive_age_months = 3,
                             invasive_taxa = c("Acacia", "Pinus", "Hakea", "Eucalyptus", "Leptospermum"),
                             verbose=TRUE,
                             ...
){


  #ensure directories are empty

  if(dir.exists(file.path(output_directory))){

    unlink(file.path(output_directory),recursive = TRUE,force = TRUE)

  }

  if(dir.exists(file.path(temp_directory))){

    unlink(file.path(temp_directory),recursive = TRUE,force = TRUE)

  }


  #create directories if needed

    if(!dir.exists(file.path(output_directory))){

      dir.create(file.path(output_directory), recursive = TRUE)

    }

  #Create temp directory (needs to come after get_park_polygons if using the same temp_directory, since the temp folder is deleted)

    if(!dir.exists(file.path(temp_directory))){

      dir.create(file.path(temp_directory),recursive = TRUE)

    }


  # Get outputs from model

    #model_results <- tar_load(model_results)

    #model_prediction <- tar_load(model_prediction)

    #spatial_outputs <- tar_load(spatial_outputs)


  # create output release if needed

    if(!park_data_tag %in% report_files$tag){

      pb_release_create(repo = REPORT_REPO,
                        tag = park_data_tag)

    }



  for (park_name in unique(parks$national_parks$CUR_NME)[1]){# remove [1] to process them all!

    gc()

    focal_park <- parks$national_parks %>%
      filter(CUR_NME == park_name)

    debug <- tryCatch(expr =
      render(input = report_location,
               output_file = gsub(pattern = " ",replacement = "_",
                                            x = paste0('report.', park_name, '.html')),
               output_dir = output_directory
        ),
      error = function(e){message("Error processing ", park_name);e}
      )

    if(inherits(debug,"error")){stop("Error processing ", park_name)}

  }# end for loop


    # Generate the Cape Nature reports via a for loop
      # park_name <- unique(parks$cape_nature$COMPLEX)[33]
      # park_name <- unique(parks$cape_nature$COMPLEX)[18]

    for (park_name in unique(parks$cape_nature$COMPLEX)[1]){. # remove [1] to process them all!

      gc()

      focal_park <- parks$cape_nature %>%
        filter(COMPLEX == park_name)

      debug <- tryCatch(expr =
                render(input = report_location,
                       output_file = gsub(pattern = " ",replacement = "_",
                                          x = paste0('report.', park_name, '.html')),
                       output_dir = output_directory),
              error = function(e){message("Error processing ", park_name); e}
      )

      #if(inherits(debug,"error")){stop()}

    }# end for loop


    #clean up temp directory
      unlink(file.path(temp_directory), recursive = TRUE, force = TRUE)
      unlink(file.path(temp_directory_evi), recursive = TRUE, force = TRUE)



}#end fx

