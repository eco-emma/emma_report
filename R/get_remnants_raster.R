#' @description Function to scrape our envdata repo and download the remnant data layers
get_remnants_raster <- function() {
  #  tf = file.path(tempdir(),"remnants.tif")
  robust_pb_download(
    file = "remnants.tif",
    #                     dest = tf,
    dest = "data/misc",
    repo = "AdamWilsonLab/emma_envdata",
    tag = "processed_static"
  )

  # remnants = rast(tf)
  return(invisible(NULL))
}


################################################################
