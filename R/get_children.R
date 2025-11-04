#' Get children geometries
#'
#' This function pulls a geographic unit's children geometries of a sub-level
#'
#' @param argument Received Python argument
#' @return A reformatted character object
#' @export
get_children <- function(query, children, year, mode, threshold = 0.1) {
  if (!toupper(mode) %in% c("INTERSECTS", "CONTAINS", "CONTAINS_THRESHOLD", "THRESHOLD_CONTAINED", "TWOWAY_THRESHOLD")) {
    stop("Invalid selection for 'mode'.")
  }
  if (typeof(children) == "character") {
    children <- nucleus::level_from_alias(children)
  } else if (typeof(children) == "numeric") {
    if (!children %in% nucleus::k.geographies$LEVEL) {
      stop("Invalid children geography level.")
    }
  }
  if ("sf" %in% class(query)) {
    if (children > nucleus::level_from_alias("State")) {
      t.states <- tigris::states(cb = TRUE, year = 2020)
      t.states <- sf::st_filter(t.states, query, .predicate = sf::st_intersects)
      t.states <- t.states$STUSPS
      for (i.state in t.states) {
        i.counties <- tigris::counties(state = i.state, cb = TRUE, year = 2020)
        t.row <- nrow(i.counties)
        i.counties <- sf::st_filter(i.counties, query, .predicate = sf::st_intersects)
        if (nrow(i.counties) == t.row) {
          rm(i.counties, t.row)
          next
        }
        rm(t.row)
        if (nrow(i.counties) > 0) {
          i.counties <- i.counties$COUNTYFP
        } else {
          rm(i.counties)
          next
        }
        if (!exists("t.counties")) {
          t.counties <- list()
        }
        t.counties[[which(t.states == i.state)]] <- i.counties
        rm(i.counties, i.state)
      }
      f.get <- utils::getFromNamespace(nucleus::k.geographies$ID_TIGRIS[as.numeric(children)], "tigris")
      if (exists("t.states") & exists("t.counties")) {
        for (i.state in 1:length(t.states)) {
          i.children <- f.get(state = t.states[[i.state]], county = t.counties[i.state], year = year)
          if (!exists("t.children")) {
            t.children <- i.children
          } else {
            t.children <- rbind(t.children, i.children)
          }
          rm(i.children)
        }
      } else if (exists("t.states") & !exists("t.counties")) {
        for (i.state in 1:length(t.states)) {
          i.children <- f.get(state = t.states[[i.state]], year = year)
          if (!exists("t.children")) {
            t.children <- i.children
          } else {
            t.children <- rbind(t.children, i.children)
          }
          rm(i.children)
        }
      }
    } else {
      f.get <- utils::getFromNamespace(nucleus::k.geographies$ID_TIGRIS[as.numeric(children)], "tigris")
      t.children <- f.get(year = year)
      t.children <- sf::st_filter(t.children, t.unit, .predicate = sf::st_intersects)
    }
  } else {
    if (typeof(query) == "character" & length(query) == 1) {
      query <- nucleus::standardize_query(query)
      if (typeof(query) == "list" & length(query) > 1) {
        stop("Multiple matches received from 'standardize_query'.")
      }
    } else {
      if (paste0(names(query), collapse = "") == paste0(c("NAME", "GEOGRAPHY", "LEVEL", "GEOID", "STATE"), collapse = "")) {
        query <- query
      } else {
        stop("Invalid query.")
      }
    }
    if (query["STATE"] != "No state container") {
      f.unit <- utils::getFromNamespace(nucleus::k.geographies$ID_TIGRIS[as.numeric(query["LEVEL"])], "tigris")
      t.unit <- f.unit(state = query["STATE"], year = year)
      t.unit <- t.unit[t.unit$GEOID == query["GEOID"], ]
      if (nrow(t.unit) == 0) {
        stop("Failed to locate geometry.")
      }
      t.counties <- tigris::counties(state = query["STATE"], cb = TRUE, year = 2020)
      t.counties <- sf::st_filter(t.counties, t.unit, .predicate = sf::st_intersects)
      t.counties <- t.counties$COUNTYFP
      f.get <- utils::getFromNamespace(nucleus::k.geographies$ID_TIGRIS[as.numeric(children)], "tigris")
      t.children <- f.get(state = query["STATE"], county = t.counties, year = year)
    } else {
      t.unit <- nucleus::pull_geometry(query, year = year)
      
      if (children > nucleus::level_from_alias("State")) {
        t.states <- tigris::states(cb = TRUE, year = 2020)
        t.states <- sf::st_filter(t.states, t.unit, .predicate = sf::st_intersects)
        t.states <- t.states$STUSPS
        for (i.state in t.states) {
          i.counties <- tigris::counties(state = i.state, cb = TRUE, year = 2020)
          t.row <- nrow(i.counties)
          i.counties <- sf::st_filter(i.counties, t.unit, .predicate = sf::st_intersects)
          if (nrow(i.counties) == t.row) {
            rm(i.counties, t.row)
            next
          }
          rm(t.row)
          if (nrow(i.counties) > 0) {
            i.counties <- i.counties$COUNTYFP
          } else {
            rm(i.counties)
            next
          }
          if (!exists("t.counties")) {
            t.counties <- list()
          }
          t.counties[[which(t.states == i.state)]] <- i.counties
          rm(i.counties, i.state)
        }
        f.get <- utils::getFromNamespace(nucleus::k.geographies$ID_TIGRIS[as.numeric(children)], "tigris")
        if (exists("t.states") & exists("t.counties")) {
          for (i.state in 1:length(t.states)) {
            i.children <- f.get(state = t.states[i.state], county = t.counties[[i.state]], year = year)
            if (!exists("t.children")) {
              t.children <- i.children
            } else {
              t.children <- rbind(t.children, i.children)
            }
            rm(i.children)
          }
        } else if (exists("t.states") & !exists("t.counties")) {
          for (i.state in 1:length(t.states)) {
            i.children <- f.get(state = t.states[[i.state]], year = year)
            if (!exists("t.children")) {
              t.children <- i.children
            } else {
              t.children <- rbind(t.children, i.children)
            }
            rm(i.children)
          }
        } else {
          f.get <- utils::getFromNamespace(nucleus::k.geographies$ID_TIGRIS[as.numeric(children)], "tigris")
          t.children <- f.get(year = year)
          t.children <- sf::st_filter(t.children, t.unit, .predicate = sf::st_intersects)
        }
      }
    }
  }
  if (!exists("t.unit")) {
    t.unit <- query
  }
  if (toupper(mode) == "INTERSECTS") {
    t.children <- sf::st_filter(t.children, t.unit, .predicate = sf::st_intersects)
    return(t.children)
  } else if (toupper(mode) == "CONTAINS") {
    t.children <- t.children[unlist(sf::st_contains(t.unit, t.children)), ]
    return(t.children)
  } else if (toupper(mode) == "CONTAINS_MAJORITY") {
    # Units where %t of area in container
    t.children <- sf::st_filter(t.children, t.unit, .predicate = sf::st_intersects)
    t.unit$GEOID_A <- t.unit$GEOID
    t.children$GEOID_B <- t.children$GEOID
    t.intersect <- suppressWarnings(sf::st_intersection(t.children, t.unit))
    t.children <- t.children[sf::st_area(t.intersect)[match(t.children$GEOID_B, t.intersect$GEOID_B)] >= threshold * sf::st_area(t.children), ]
    t.children$GEOID_B <- NULL
    return(t.children)
  } else if (toupper(mode) == "MAJORITY_CONTAINED") {
    # Units where %t of container is filled by unit
    t.children <- sf::st_filter(t.children, t.unit, .predicate = sf::st_intersects)
    t.unit$GEOID_A <- t.unit$GEOID
    t.children$GEOID_B <- t.children$GEOID
    t.intersect <- suppressWarnings(sf::st_intersection(t.children, t.unit))
    t.children <- t.children[t.children$GEOID %in% t.intersect$GEOID[sf::st_area(t.intersect) >= threshold * sf::st_area(t.unit)], ]
    return(t.children)
  } else {
    # Both above methods
    t.children <- sf::st_filter(t.children, t.unit, .predicate = sf::st_intersects)
    t.unit$GEOID_A <- t.unit$GEOID
    t.children$GEOID_B <- t.children$GEOID
    t.intersect <- suppressWarnings(sf::st_intersection(t.children, t.unit))
    i.contains_majority <- which(sf::st_area(t.intersect)[match(t.children$GEOID_B, t.intersect$GEOID_B)] >= threshold * sf::st_area(t.children))
    i.majorty_contained <- which(t.children$GEOID %in% t.intersect$GEOID[sf::st_area(t.intersect) >= threshold * sf::st_area(t.unit)])
    t.children <- t.children[unique(c(i.contains_majority, i.majorty_contained)), ]
    return(t.children)
  }
}


