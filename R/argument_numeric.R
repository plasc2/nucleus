#' Receive Numeric Argument
#'
#' This function processes numeric arguments received from Python.
#'
#' @param argument Received Python argument
#' @return A reformatted numeric object
#' @export
argument_numeric <- function(argument) {
  if (sum(grepl(",", argument)) > 0) {
    t.arg <- gsub(" ", "", argument)
    return(as.numeric(strsplit(t.arg, split = ",")[[1]]))
  } else if (sum(grepl(":", argument)) == 1) {
    t.arg1 <- as.numeric(sub("\\:.*", "", argument))
    t.arg2 <- as.numeric(sub(".*:", "", argument))
    return(t.arg1:t.arg2)
  } else {
    return(as.numeric(argument))
  }
}