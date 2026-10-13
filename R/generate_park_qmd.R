library(rmarkdown)
library(stars)
library(tidyverse)
library(lubridate)
library(arrow)
library(dygraphs)
library(SPEI)
library(ggplot2)
source("R/get_park_polygons.R")


#tar_load(parks)

generate_park_qmds <- function(
    protected_areas,
    report_location,
    output_directory="reports"){

#create directory if needed
if(!dir.exists(file.path(output_directory))){
  dir.create(file.path(output_directory), recursive = TRUE)
}

# Proceed if rendering the whole project, exit otherwise
#if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
#  quit()
#}



## filter to just a few for testing - set to T for interactive debugging
if(F){
    protected_areas <- protected_areas |>
  st_as_sf() |>
  filter(name%in%c("Table Mountain National Park","West Coast National Park",
                   "Cederberg Nature Reserve Complex","Anysberg Nature Reserve","Addo-Elephant National Park"))
  # just set one park for testing
  park_name="Table Mountain National Park"
  park_name="West Coast National Park"

}

for (park_name in unique(protected_areas$name)){
  # read in the template qmd file
  templ <- readLines("report_prototype.qmd")
  # udpate it with the park name - this is used in the template to generate different reports for each park
    writeLines(
      gsub("focal_park_name: \"Table Mountain National Park\"",paste0("focal_park_name: \"",park_name,"\"",sep=""),templ),
      file.path(output_directory,paste0(gsub(" ","_",park_name), ".qmd"))
    )
    message("Done with ", park_name)
  }

return(output_directory)

}
