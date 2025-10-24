#' Unit Inter-Distances
#'
#' This function calculates geodesic distances between given units' centroids.
#'
#' @param units Unit geometries to calculate distances between
#' @param id_column Unique identifier column for unit geometries
#' @param na.rm A logical determining whether NA values should be omitted from the final output table
#' @param threshold Optional upper distance limit, distance values above (non-inclusive) the threshold will be omitted
#' @return A table of inter-unit distances in kilometers, with a 'START' and 'END' column identifying units
#' @export
inter_distances <- function(units, id_column = "GEOID", na.rm = FALSE, threshold = NULL) {
  if (sum(duplicated(unlist(sf::st_drop_geometry(units[id_column])))) > 0) {
    stop("Duplicate IDs detected.")
  }
  if (sf::st_is_longlat(units) == FALSE) {
    stop("Units must be provided in a geographic coordinate reference system.")
  }
  if (nrow(units) == 0 | ncol(units) == 0) {
    stop("Table has insufficient rows or columns.")
  }
  centroids <- suppressWarnings(sf::st_centroid(units))
  t.out <- data.frame(START = rep(unlist(sf::st_drop_geometry(units[id_column])), each = nrow(units)), END = rep(unlist(sf::st_drop_geometry(units[id_column])), times = nrow(units)))
  coords_s <- sf::st_coordinates(centroids[match(t.out$START, unlist(sf::st_drop_geometry(centroids[id_column]))), ])
  coords_e <- sf::st_coordinates(centroids[match(t.out$END, unlist(sf::st_drop_geometry(centroids[id_column]))), ])
  t.out$START_X <- coords_s[, 2]
  t.out$START_Y <- coords_s[, 1]
  t.out$END_X <- coords_e[, 2]
  t.out$END_Y <- coords_e[, 1]
  t.out$D_KM <- mapply(function(s, e) geosphere::distGeo(s,e) / 1000, split(coords_s, row(coords_s)), split(coords_e, row(coords_e)))
  if (na.rm == TRUE) {
    t.out <- t.out[!is.na(t.out$D_KM), ]
  }
  if (!is.null(threshold)) {
    if (threshold > 0) {
      t.out <- t.out[t.out$D_KM <= threshold, ]
    } else {
      stop("Threshold must be greater than 0.")
    }
  }
  t.out$START_X <- NULL
  t.out$START_Y <- NULL
  t.out$END_X <- NULL
  t.out$END_Y <- NULL
  return(t.out)
}
