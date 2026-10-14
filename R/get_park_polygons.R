library(sf)
library(tidyverse)
library(dplyr)

#' Download processed park polygons from emma_envdata `static_data`
#'
#' Report SoT is `protected_areas.gpkg` on `eco-emma/emma_envdata@static_data`
#' (name, type, area_ha; EPSG:9221). Park shapefile processing and the legacy
#' `emma_report@park_data` upload path are retired (Phase C / R2).
#'
#' Missing asset → fail loud (no local shapefile fallback).
#' Zero-feature gpkg → succeed with empty vect (Quarto handles empty parks).
#'
#' @param temp_directory Temporary download directory
#' @param ... Ignored; retained so older call sites with shapefile paths still parse
#' @return terra SpatVector of parks
get_park_polygons <- function(temp_directory = "data/temp/parks",
                              ...) {
  dots <- list(...)
  if (length(dots) > 0L) {
    message(
      "get_park_polygons(): ignoring legacy shapefile / park_data args; ",
      "downloading protected_areas.gpkg from emma_envdata@static_data"
    )
  }

  if (!dir.exists(temp_directory)) {
    dir.create(temp_directory, recursive = TRUE, showWarnings = FALSE)
  }

  path <- download_envdata_asset(
    tag = ENVDATA_RELEASE_TAGS$static,
    file = "protected_areas.gpkg",
    dest_dir = temp_directory,
    overwrite = TRUE
  )

  protected_areas <- sf::st_read(path, quiet = TRUE)
  need <- ENVDATA_REPORT_ASSETS$parks$required_cols
  missing_cols <- setdiff(need, names(protected_areas))
  if (length(missing_cols) > 0L) {
    stop(
      "protected_areas.gpkg missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Keep required attrs + active geometry column (sf-safe).
  geom_col <- attr(protected_areas, "sf_column")
  protected_areas <- protected_areas[, c(need, geom_col)]

  # Cleanup download dir (keep returned object in memory / targets store)
  unlink(temp_directory, recursive = TRUE, force = TRUE)

  protected_areas_v <- terra::vect(protected_areas)
  terra::crs(protected_areas_v) <- sf::st_crs(protected_areas)$wkt
  message(
    "Downloaded ", nrow(protected_areas),
    " parks from emma_envdata@static_data/protected_areas.gpkg"
  )
  protected_areas_v
}
