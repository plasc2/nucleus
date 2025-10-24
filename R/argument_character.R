#' Receive Character Argument
#'
#' This function processes character arguments received from Python.
#'
#' @param argument Received Python argument
#' @return A reformatted character object
#' @export
argument_character <- function(argument, allow_spaces = FALSE) {
  if ((sum(grepl(" ", argument)) > 0 | sum(grepl(",", argument)) == 0) & allow_spaces == FALSE) {
    return(unlist(strsplit(argument, split = " ")))
  } else if (sum(grepl(",", argument)) > 0) {
    return(unlist(strsplit(argument, split = ", ")))
  } else {
    return(as.character(argument))
  }
}