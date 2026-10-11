library(piggyback)

#' @param file an rds file (with extension) in the emma_model repo
#' @returns a loaded object of the file
get_model_data <- function(file,
                           temp_directory = "data/temp/reports",
                           sleep_time = 10,
                           max_attempts = 100) {
  robust_pb_download(
    file = file,
    dest = file.path(temp_directory),
    repo = "AdamWilsonLab/emma_model",
    tag = "current",
    max_attempts = max_attempts,
    sleep_time = sleep_time
  )

  return(readRDS(file.path(temp_directory, file)))
}
