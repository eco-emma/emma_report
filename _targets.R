library(targets)
library(tarchetypes)
# install.packages("geotargets", repos = c("https://ropensci.r-universe.dev", "https://cran.r-project.org"))
library(geotargets)
library(tidyverse)
library(sf)
library(arrow)
library(piggyback)
library(plotly)
library(leaflet)
library(gt)
library(dygraphs)
library(quarto)
library(units)
#remotes::install_github("ropensci/stantargets")
# if(!"basemapR" %in% rownames(installed.packages())){
#   devtools::install_github('Chrisjb/basemapR')
# }

#library(stantargets)
# Envdata download/upload helpers and release-tag contract live in R/envdata_contract.R
# (sourced below). Do not source deleted AdamWilsonLab robust_pb_*.R URLs.
# source all files in R folder (skip filter_training_data.R — historical exclusion)
r_files <- list.files("R", pattern = "[.]R$", full.names = TRUE)
r_files <- r_files[!grepl("filter_training_data[.]R$", basename(r_files))]
# Load contract first so ENVDATA_* constants exist for other R/ files
contract <- r_files[grepl("envdata_contract[.]R$", basename(r_files))]
other <- setdiff(r_files, contract)
lapply(c(contract, other), source)

options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")
geotargets_option_set(gdal_vector_driver="ESRI Shapefile")

tar_option_set(packages = c("piggyback","cmdstanr", "posterior", "bayesplot", "tidyverse",
                            "stringr","knitr","sf","stars","units","arrow","lubridate","stantargets",
                            "doParallel","raster","quarto","tarchetypes",
                            "targets","doParallel","plotly","leaflet", "gt", "jsonlite", "terra"),
               deployment="main")

# push intermediate products to github release?
piggyback_push = F

sleep_time = 10
max_attempts = 10
# Legacy report-output tag (emma_report releases). Envdata inputs use ENVDATA_RELEASE_TAGS.
tag = "current"
min_date = "1970-01-01"

#tar_load(c(envdata, stan_data, model_results, spatial_outputs,model_prediction,protected_areas))
#Sys.setenv(HOME="/home/rstudio")

# tar_destroy(ask = F)

temp_directory="data/temp/"

list(

  # Load the model data

    tar_target(name = model_results,
               command = get_model_data(file = "model_results.rds")
              ),

    tar_target(name = spatial_outputs,
                 command = get_model_data(file = "spatial_outputs.rds")
              ),

    tar_target(name = model_prediction,
                 command = get_model_data(file = "model_prediction.rds")
              ),
#    tar_target(temp_directory, # don't make a target because it runs every time.
#               command = get_temp_directory("data/temp/"),format="file"),

    tar_terra_vect(protected_areas,
               command = get_park_polygons(temp_directory = temp_directory,
                                           sacad_filename = "data/manual_downloads/protected_areas/SACAD_OR_2021_Q4.shp",
                                           sapad_filename = "data/manual_downloads/protected_areas/SAPAD_OR_2021_Q4.shp",
                                           cape_nature_filename = "data/manual_downloads/protected_areas/Provincial_Nature_Reserves/CapeNature_Reserves_gw.shp")
               ),

   tar_target(remnants,
              command = get_remnants_raster()
              ),


  # tar_target(env_files,
  #             command = get_env_files()
  #             ),

  # Refresh release asset listing daily (static_data, vi_*, fireage, stac-*, …)
  tar_age(
        name    = env_files,
        command = get_env_files(),
        age     = as.difftime(1, units = "days")
      ),
  #   tar_target(
  #   env_files,
  #   # command = get_env_files(), 
  #   command = get_env_files(dir = file.path(temp_directory, "pb_cache")),
  #   format  = "file",           
  #   cue     = tar_cue(mode = "thorough")
  # ),
#  tar_target(report_files,
#              command = get_report_files()
#   ),

   tar_terra_rast(years_since_fire.tif,
              command = get_years_since_fire.tif(env_files,temp_directory, most_recent_fire_date,piggyback_push)
   ),

  tar_target(fires_wgs,
             command = generate_fires_vector(years_since_fire.tif)
  ),


   tar_age(name = weather_data,
           command = update_climate_data_gsod(protected_areas,stations, temp_directory),
           age = as.difftime(7, units = "days") #weekly updates
           #age = as.difftime(1, units = "days") #daily updates
           # age = as.difftime(0, units = "hours") #will update whenever run
   ),

tar_age(stations,
        command=get_stations_data(temp_directory, protected_areas),
        age = as.difftime(7, units = "days") #weekly updates
        #age = as.difftime(1, units = "days") #daily updates
),


#   tar_age(name = most_recent_evi_file,
#           command = get_most_recent_evi_file(env_files),
#           age = as.difftime(7, units = "days"), #weekly updates
# #          age = as.difftime(1, units = "days") #daily updates
#   ),
  
    tar_age(
    name    = most_recent_evi_file,
    command = get_most_recent_evi_file(env_files),
    age     = as.difftime(7, units = "days"),
  ),
  
  tar_age(name = most_recent_evi_date,
           command = get_most_recent_evi_date(most_recent_evi_file),
           age = as.difftime(7, units = "days") #weekly updates
           #age = as.difftime(1, units = "days") #daily updates
           # age = as.difftime(0, units = "hours") #will update whenever run
           ),
  
  tar_terra_rast(name = most_recent_evi.tif,
          command = get_most_recent_evi.tif(most_recent_evi_file, temp_directory),
          filetype="COG"),

tar_target(
  evi_export,
  {
    # 안전장치: SpatRaster인지 확인
    stopifnot(inherits(most_recent_evi.tif, "SpatRaster"))

    out <- file.path("data/evi", "most_recent_evi.tif")
    dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)

    terra::writeRaster(
      most_recent_evi.tif,
      filename = out,
      overwrite = TRUE,
      filetype = "COG",
      gdal = c("COMPRESS=LZW")
    )

    stopifnot(file.exists(out), file.info(out)$size > 0)

    message("Files under data/evi:")
    print(list.files("data/evi", full.names = TRUE, recursive = TRUE))

    out
  },
  format = "file"
),


   tar_age(name = current_month,
           command = lubridate::month(most_recent_evi_date),
           age = as.difftime(7, units = "days") #weekly updates
           #age = as.difftime(1, units = "days") #daily updates
           ),

tar_terra_rast(name = monthly_mean_evi.tif,
               command = get_monthly_mean_evi.tif(env_files,temp_directory,
                                                      current_month = current_month),
               filetype="COG"),

tar_terra_rast(name = monthly_delta_evi.tif,
               command = get_monthly_delta_evi.tif(most_recent_evi.tif,monthly_mean_evi.tif),
               filetype="COG"),

tar_age(name = inat_data,
              command = get_inat_data(inat_data_location = "data/manual_downloads/inat_project/observations-405358.csv",
                                      temp_directory = temp_directory,
                                      max_attemps = 10,
                                      sleep_time=10,
                                      oldest_date = "2020-01-01",
                                      protected_areas = protected_areas,
                                      park_buffer = 10000,
                                      invasive_taxa = c("Acacia", "Pinus", "Hakea", "Eucalyptus", "Leptospermum"), #these are flagged in the table
                                      verbose=FALSE),
           # age = as.difftime(7, units = "days") #weekly updates
           # age = as.difftime(1, units = "days") #daily updates
            age = as.difftime(28, units = "days") #will update monthly
           ),

   tar_age(name = park_fire_history,
              command = get_fire_history(temp_directory = temp_directory,
                                         max_attempts = 10,
                                         sleep_time = 10,
                                         protected_areas = protected_areas),
           # age = as.difftime(7, units = "days") #weekly updates
           # age = as.difftime(1, units = "days") #daily updates
            age = as.difftime(28, units = "days") #will update monthly
           ),




   tar_target(name = report_location,
              command = "report_prototype.qmd", # park report template
              format = "file"),

   # the target below re-runs if the qmd changes
   tar_target(name = report_qmd_dir,
              command = generate_park_qmds(protected_areas,report_location),
              format="file"),


   # tar_target(name = model_summary,
   #            command = workflow(call = rmarkdown::render(input = "model_summary.qmd"),
   #                               ... = model_results,
   #                               ... = model_prediction,
   #                               ... = spatial_outputs)),


    tar_quarto(website, debug=T, quiet=F)

)
