#' Geography Alias from Level
#'
#' This function takes the ordinal level of a geographic unit, and returns
#' its 'alias' as defined in Nucleus.
#'
#' @param query 'Level', the name of a geographic unit as an integer
#' @return A character alias
#' @export
#' 
#' @examples
#' \dontrun{
#' alias_from_level(1)
#' }
#' \dontrun{
#' alias_from_level(c(1, 3))
#' }
#' 
alias_from_level <- function(query) {
  t.alias <- unlist(strsplit(nucleus::k.geographies$ALIAS[query], split = ", "))
  t.alias <- gsub("'", "", t.alias)
  return(t.alias)
}