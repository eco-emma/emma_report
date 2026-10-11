#' @param call is a call to a function
#' @param ... additional stuff that functions relies upon
#' @note the point of this function is just to make sure the workflow is correct.
workflow <- function(call = NULL, ...) {
  return(eval(call))
}
