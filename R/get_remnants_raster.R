#' Download remnants layer from the new static_data domain.tif
#'
#' Legacy reports expected a standalone remnants.tif on processed_static.
#' New envdata publishes remnants as band 3 of static_data/domain.tif.
get_remnants_raster <- function(dest = "data/misc/remnants.tif") {
  write_remnants_from_domain(dest = dest)
  invisible(NULL)
}
