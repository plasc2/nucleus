#' Pull Query Geometry
#'
#' This function pulls a query's geometry from the 'tigris' package.
#'
#' @param query A standard Nucleus query
#' @return An sf object
#' @export
pull_geometry <- function(query, year) {
  if (paste0(names(query), collapse = "") != paste0(c("NAME", "GEOGRAPHY", "LEVEL", "GEOID", "STATE"), collapse = "")) {
    stop("Invalid query passed.")
  }
  if (typeof(class) == "list" & length(query) > 1) {
    stop("Multiple queries passed.")
  }
  f.get <- utils::getFromNamespace(nucleus::k.geographies$ID_TIGRIS[as.numeric(query["LEVEL"])], "tigris")
  if (query["STATE"] != "No state container") {
    t.out <- f.get(year = year, state = query["STATE"])
  } else {
    t.out <- f.get(year = year)
  }
  t.out <- t.out[t.out$GEOID == query["GEOID"], ]
  return(t.out)
}