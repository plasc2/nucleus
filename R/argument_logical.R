#' Receive Logical Argument
#'
#' This function processes logical arguments received from Python.
#'
#' @param argument Received Python argument
#' @return A reformatted logical object
#' @export
argument_logical <- function(argument) {
  if (!is.logical(argument)) {
    if (toupper(argument) == "TRUE") {
      return(TRUE)
    } else if (toupper(argument) == "FALSE") {
      return(FALSE)
    } else if (toupper(argument) == "NULL") {
      return(NULL)
    }
  } else {
    return(argument)
  }
}