#' Check Unit Geometries and Groups Overlap
#'
#' This function checks for overlaps between unit geometries and groups.
#'
#' @param units Dataframe sf object describing unit geometries
#' @param groups Dataframe sf object describing groups
#' @return A logical output describing whether there are valid overlaps between inputs
#' @export
check_overlap_units_groups <- function(units, groups, mode, threshold = 0.1) {
  if (!"sf" %in% class(units) | !"sf" %in% class(groups)) {
    stop("Arguments 'units' and 'groups' must be sf.")
  }
  if (!"sf" %in% class(units)) {
    stop("Argument 'units' must be sf.")
  }
  if (!"sf" %in% class(groups)) {
    stop("Argument 'groups' must be sf.")
  }
  if (!toupper(mode) %in% c("INTERSECTS", "CONTAINS", "CONTAINS_THRESHOLD", "THRESHOLD_CONTAINED", "TWOWAY_THRESHOLD")) {
    stop("Invalid 'mode' selection.")
  }
  if (toupper(mode) == "INTERSECTS") {
    units$GEOID_A <- units$GEOID
    groups$GEOID_B <- groups$GEOID
    t.intersection <- suppressWarnings(sf::st_intersection(groups, units))
    if (nrow(t.intersection) == 0) {
      return(c(VALID = FALSE),
             UNITS_IN_GROUPS = 0,
             GROUPS_IN_UNITS = 0)
    } else {
      return(c(VALID = TRUE,
               UNITS_IN_GROUPS = sum(units$GEOID_A %in% t.intersection$GEOID_A) / nrow(units),
               GROUPS_IN_UNITS = sum(groups$GEOID_B %in% t.intersection$GEOID_B) / nrow(groups)))
    }
  } else if (toupper(mode) == "CONTAINS") {
    units$GEOID_A <- units$GEOID
    groups$GEOID_B <- groups$GEOID
    t.contains <- units[unlist(sf::st_contains(groups, units)), ]
    if (nrow(t.contains) == 0) {
      return(c(VALID = FALSE),
             UNITS_IN_GROUPS = 0,
             GROUPS_IN_UNITS = 0)
    } else {
      return(c(VALID = TRUE,
               UNITS_IN_GROUPS = nrow(t.contains) / nrow(units),
               GROUPS_IN_UNITS = length(unique(unlist(sf::st_intersects(t.contains, groups)))) / nrow(groups)))
    }
  } else if (toupper(mode) == "CONTAINS_THRESHOLD") {
    units$GEOID_A <- units$GEOID
    groups$GEOID_B <- groups$GEOID
    t.intersection <- suppressWarnings(sf::st_intersection(units, groups))
    t.intersection <- t.intersection[sf::st_area(t.intersection) >= threshold * sf::st_area(units)[match(units$GEOID_A, t.intersection$GEOID_A)], ]
    if (nrow(t.intersection) == 0) {
      return(c(VALID = FALSE),
             UNITS_IN_GROUPS = 0,
             GROUPS_IN_UNITS = 0)
    } else {
      return(c(VALID = TRUE,
               UNITS_IN_GROUPS = length(unique(t.intersection$GEOID_A)) / nrow(units),
               GROUPS_IN_UNITS = length(unique(unlist(sf::st_intersects(t.intersection, groups)))) / nrow(groups)))
    }
  } else if (toupper(mode) == "THRESHOLD_CONTAINED") {
    units$GEOID_A <- units$GEOID
    groups$GEOID_B <- groups$GEOID
    t.intersection <- suppressWarnings(sf::st_intersection(units, groups))
    for (i.group in 1:nrow(groups)) {
      i.intersection <- t.intersection[sf::st_area(t.intersection) >= threshold * sf::st_area(groups[i.group, ]), ]
      if (!exists("t.intersections")) {
        t.intersections <- i.intersection
      } else {
        t.intersections <- rbind(t.intersections, i.intersection)
        t.intersections <- t.intersections[!duplicated(t.intersections$GEOID_A), ]
      }
    }
    if (nrow(t.intersections) == 0) {
      return(c(VALID = FALSE),
             UNITS_IN_GROUPS = 0,
             GROUPS_IN_UNITS = 0)
    } else {
      return(c(VALID = TRUE,
               UNITS_IN_GROUPS = length(unique(t.intersections$GEOID_A)) / nrow(units),
               GROUPS_IN_UNITS = length(unique(unlist(sf::st_intersects(t.intersections, groups)))) / nrow(groups)))
    }
  } else {
    units$GEOID_A <- units$GEOID
    groups$GEOID_B <- groups$GEOID
    t.intersection <- suppressWarnings(sf::st_intersection(units, groups))
    i.contains_majority <- which(sf::st_area(t.intersection) >= threshold * sf::st_area(units)[match(units$GEOID_A, t.intersection$GEOID_A)])
    for (i.group in 1:nrow(groups)) {
      i.intersection <- t.intersection[sf::st_area(t.intersection) >= threshold * sf::st_area(groups[i.group, ]), ]
      if (!exists("t.intersections")) {
        t.intersections <- i.intersection
      } else {
        t.intersections <- rbind(t.intersections, i.intersection)
        t.intersections <- t.intersections[!duplicated(t.intersections$GEOID_A), ]
      }
    }
    i.majorty_contained <- which(units$GEOID_A %in% t.intersections$GEOID_A)
    t.twoway_threshold <- units[unique(c(i.contains_majority, i.majorty_contained)), ]
    if (nrow(t.twoway_threshold) == 0) {
      return(c(VALID = FALSE),
             UNITS_IN_GROUPS = 0,
             GROUPS_IN_UNITS = 0)
    } else {
      return(c(VALID = TRUE,
               UNITS_IN_GROUPS = nrow(t.twoway_threshold) / nrow(units),
               GROUPS_IN_UNITS = length(unique(unlist(sf::st_intersects(t.twoway_threshold, groups)))) / nrow(groups)))
    }
  }
}
