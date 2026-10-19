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

# PR CI sets EMMA_CI_LIGHT=1 to stub Quarto website only.
# Parks / weather / iNat are download-only from emma_envdata releases (fail loud
# if assets missing). Push/schedule (EMMA_CI_LIGHT=0) run full Quarto in
# targets-website.yml (Phase E); data job still excludes the website target.
ci_light <- identical(Sys.getenv("EMMA_CI_LIGHT"), "1")
if (ci_light) {
  message("EMMA_CI_LIGHT=1: light pipeline mode for pull_request CI")
}

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

    # Parks SoT: emma_envdata@static_data/protected_areas.gpkg (no local dissolve / park_data upload)
    tar_terra_vect(protected_areas,
               command = get_park_polygons(temp_directory = temp_directory)
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


   # Weather SoT: emma_envdata@weather_data (download-only; missing asset fails)
   tar_age(name = weather_data,
           command = update_climate_data_gsod(temp_directory = temp_directory),
           age = as.difftime(1, units = "days")
   ),

tar_age(stations,
        command = get_stations_data(temp_directory),
        age = as.difftime(1, units = "days")
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

# Report-side long-term climatology: fuse-then-mean over all VI parquet dates
# (MODIS + VIIRS when vi_viirs_parquet is published).
tar_terra_rast(name = mean_evi.tif,
               command = get_mean_evi.tif(env_files, temp_directory),
               filetype = "COG"),

tar_terra_rast(name = delta_evi.tif,
               command = get_delta_evi.tif(most_recent_evi.tif, mean_evi.tif),
               filetype = "COG"),

# iNat SoT: emma_envdata@inat_data/inat_invasives.gpkg (download-only; no live API)
tar_age(name = inat_data,
              command = get_inat_data(temp_directory = temp_directory, verbose = TRUE),
            age = as.difftime(1, units = "days")
           ),

   tar_age(name = park_fire_history,
              command = get_fire_history(temp_directory = temp_directory,
                                         max_attempts = 10,
                                         sleep_time = 10,
                                         protected_areas = protected_areas,
                                         n_recent = 24L,
                                         n_years = 20L),
           # age = as.difftime(7, units = "days") #weekly updates
           # age = as.difftime(1, units = "days") #daily updates
            age = as.difftime(28, units = "days") #will update monthly
           ),




   # Quiet assessment KPIs (P0 coverage — 2D/3B/4B/10B/12A) for index/Data/parks.
   tar_target(
     name = coverage_metrics,
     command = compute_coverage_metrics(
       most_recent_evi.tif = most_recent_evi.tif,
       most_recent_evi_date = most_recent_evi_date,
       protected_areas = protected_areas,
       temp_directory = temp_directory
     )
   ),

   # Rank 7 freshness: STAC datetime + release updated_at (build time at render).
   tar_age(
     name = data_freshness,
     command = compute_data_freshness(),
     age = as.difftime(1, units = "days")
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


    # Full Quarto site (~30m / ~570MB) on push/schedule / local.
    # PR CI (EMMA_CI_LIGHT=1) stubs docs/ so data targets stay cheap to validate.
    if (ci_light) {
      tar_target(
        name = website,
        command = {
          dir.create("docs", showWarnings = FALSE, recursive = TRUE)
          marker <- "docs/.emma_ci_light_skip"
          writeLines("skipped Quarto website (EMMA_CI_LIGHT=1)", marker)
          marker
        },
        format = "file"
      )
    } else {
      tar_quarto(website, debug = TRUE, quiet = FALSE)
    }

)
