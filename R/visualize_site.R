#' Visualize Site
#'
#' This function produces a minimalistic visualization of a given site.
#'
#' @param site A string or integer GEOID describing a geographic area in the U.S.
#' @return A map given argument 'device'
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
site <- "Dallas city, TX"

visualize_site <- function(site, device = "jpg") {
  query <- standardize_query(site)
  if (length(query) != 1) {
    stop("Site must produce query of length 1. If passing a string, be sure to add identifiers like 'city', 'village', 'metropolitan area', etc.")
  }
  f.get <- get(nucleus::k.geographies$ID_TIGRIS[as.numeric(query["LEVEL"])])
  if (as.numeric(query["LEVEL"]) > level_from_alias("State") & !as.numeric(query["LEVEL"]) %in% level_from_alias(c("CSA", "CBSA", "MSA"))) {
    t.unit <- suppressMessages(f.get(state = query["STATE"], year = 2020))
    t.unit <- t.unit[t.unit$GEOID == query["GEOID"], ]
    if (nrow(t.unit) == 0) {
      stop("Failed to locate site.")
    }
  } else {
    
  }
  
}