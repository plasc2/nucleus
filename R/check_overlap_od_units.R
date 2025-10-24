#' Check OD Table and Unit Geometries Overlap
#'
#' This function checks for overlaps between an OD table and unit geometries.
#'
#' @param od_table Base origin-destination table
#' @param od_ids OD 'start' and 'end' column names as a vector c("start_id", "end_id")
#' @param units Dataframe or sf object describing unit geometries
#' @param unit_ids Unique ID column name for unit geometries
#' @return A logical output describing whether there are valid overlaps between inputs
#' @export

check_overlap_od_units <- function(od_table, od_ids, units, unit_ids) {
  if (sum(od_ids %in% names(od_table)) != 2) {
    stop(paste0("OD ID column name(s) '", paste0(od_ids[!od_ids %in% names(od_table)], collapse = "' and '"), "' not found in OD table."))
  }
  if (!unit_ids %in% names(units)) {
    stop(paste0("Unit ID column name '", unit_ids, "' not found in unit data."))
  }
  unique_od_ids <- unique(unlist(od_table[od_ids[1]]), unlist(od_table[od_ids[2]]))
  if (sum(unlist(sf::st_drop_geometry(units[, unit_ids])) %in% unique_od_ids) == 0) {
    return(c(VALID = FALSE,
             UNITS_IN_OD = 0,
             OD_IN_UNITS = 0))
  } else {
    return(c(VALID = TRUE,
             UNITS_IN_OD = sum(unlist(sf::st_drop_geometry(units[, unit_ids])) %in% unique_od_ids) / nrow(units),
             OD_IN_UNITS = sum(unique_od_ids %in% unlist(sf::st_drop_geometry(units[, unit_ids]))) / length(unique_od_ids)))
  }
}