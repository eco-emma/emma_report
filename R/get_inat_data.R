library(sf)
library(tidyverse)

#' Empty invasives sf matching the pinned `inat_data` schema (WGS84)
inat_empty_sf <- function() {
  sf::st_sf(
    scientific_name = character(),
    observed_on = as.Date(character()),
    longitude = numeric(),
    latitude = numeric(),
    invasive = logical(),
    geometry = sf::st_sfc(crs = "WGS84")
  )
}

#' Download invasive iNaturalist records from emma_envdata `inat_data`
#'
#' Download-only from `inat_invasives.gpkg`. No live rinat API, no park-union,
#' no CSV-seed GHA fallback. Missing asset → fail loud. Zero rows → empty sf
#' (Quarto empty-iNat leaflet guards apply).
#'
#' @param temp_directory Download directory
#' @param ... Ignored legacy args (CSV path, taxa, park buffer, …)
#' @return sf of invasives (WGS84)
get_inat_data <- function(temp_directory = "data/temp/inat/",
                          verbose = TRUE,
                          ...) {
  dots <- list(...)
  if (length(dots) > 0L && isTRUE(verbose)) {
    message(
      "get_inat_data(): ignoring legacy live/CSV args; ",
      "downloading inat_invasives.gpkg from emma_envdata@inat_data"
    )
  }

  if (!dir.exists(temp_directory)) {
    dir.create(temp_directory, recursive = TRUE, showWarnings = FALSE)
  }

  path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$inat,
    file = "inat_invasives.gpkg",
    dest_dir = temp_directory,
    overwrite = TRUE
  )

  invasives <- sf::st_read(path, quiet = TRUE)
  need <- ENVDATA_REPORT_ASSETS$inat$required_cols

  if (nrow(invasives) == 0L) {
    if (isTRUE(verbose)) {
      message(
        "inat_invasives.gpkg is empty (0 rows); returning empty sf ",
        "(Quarto empty-iNat path)"
      )
    }
    return(inat_empty_sf())
  }

  missing_cols <- setdiff(need, names(invasives))
  if (length(missing_cols) > 0L) {
    stop(
      "inat_invasives.gpkg missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  geom_col <- attr(invasives, "sf_column")
  invasives <- invasives[, c(need, geom_col)]
  invasives <- sf::st_transform(invasives, "WGS84")

  if (isTRUE(verbose)) {
    message(
      "Downloaded ", nrow(invasives),
      " invasive records from emma_envdata@inat_data/inat_invasives.gpkg"
    )
  }
  invasives
}
