#' Geography Level from Alias
#'
#' This function takes the 'alias' or name of a geographic unit, and returns
#' its 'level' as defined in Nucleus.
#'
#' @param query 'Alias', the name of a geographic unit as a character
#' @return An integer level
#' @export
#' 
#' @examples
#' \dontrun{
#' level_from_alias("State")
#' }
#' \dontrun{
#' level_from_alias(c("State", "Tract"))
#' }
#' 
level_from_alias <- function(query) {
  t.level <- nucleus::k.geographies$LEVEL[grepl(paste0(toupper(paste0("'", query, "'")), collapse = "|"), toupper(nucleus::k.geographies$ALIAS))]
  return(t.level)
}