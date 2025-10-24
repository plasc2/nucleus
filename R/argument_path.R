#' Receive Path Argument
#'
#' This function processes path arguments received from Python.
#'
#' @param argument Received Python argument
#' @return A reformatted character object describing path
#' @export
argument_path <- function(argument) {
  return(as.character(argument))
}