#' @param temp_directory Temporary directory used in workflow to be deleted
#'
clean_up <- function(temp_directory) {
  # Delete the temporary directory if it hasn't been done

  if (dir.exists(file.path(temp_directory))) {
    unlink(file.path(temp_directory), recursive = TRUE, force = TRUE)
  }

  # Clear out the RNOO cache


  # Delete earth engine data
}
