#' Download remnant polygons from the emma_envdata manual-data release
#'
#' Legacy code scraped AdamWilsonLab/emma_envdata git tree paths. Remnant
#' shapefile inputs now live on the `manual-data` release.
get_remnants_polygons <- function(dest_dir = "data/manual_downloads/RLE_2021_Remnants") {

  zip_name <- "RLE_2021_Remnants.zip"
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  zip_path <- file.path(dest_dir, zip_name)
  if (!file.exists(zip_path)) {
    download_envdata_asset(
      tag = "manual-data",
      file = zip_name,
      dest_dir = dest_dir,
      overwrite = FALSE
    )
  }

  shp <- list.files(dest_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(shp) == 0L) {
    utils::unzip(zip_path, exdir = dest_dir)
    shp <- list.files(dest_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  }
  if (length(shp) == 0L) {
    stop("No remnant shapefile found after downloading ", zip_name)
  }
  sf::read_sf(shp[[1]])
}
