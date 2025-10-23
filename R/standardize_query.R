#' Standardize Query
#'
#' This function takes an input geographic unit query, and returns a standardized
#' version of that query in Nucleus' format, or NA if no match exists.
#'
#' @param query The character query to standardize
#' @return The standardized query with its level, state (if applicable), and GEOID
#' @export
#' 
#' @examples
#' \dontrun{
#' standardize_query("USA")
#' }
#' \dontrun{
#' standardize_query("Fort Worth, TX")
#' }
#' \dontrun{
#' standardize_query("Fort Worth City, TX")
#' }
#' 
standardize_query <- function(query) {
  if (length(query) != 1) {
    stop("Multiple inputs forbidden. Query must be of length 1.")
  }
  if (is.na(suppressWarnings(as.integer(query))) == FALSE) {
    if (nchar(query) < 2) {
      stop("Integer query must have length greater than or equal to 2.")
    } else if (nchar(query) == 2) {
      if (query %in% k.states$GEOID) {
        return(c(NAME = k.states$NAME[k.states$GEOID == query],
                 GEOGRAPHY = "State",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "State"],
                 GEOID = query,
                 STATE = "No state container"))
      } else {
        stop(paste0("No state with code ", query, "."))
      }
    } else if (nchar(query) == 3) {
      t.reference <- suppressMessages(tigris::combined_statistical_areas(year = 2020))
      if (query %in% t.reference$GEOID) {
        return(c(NAME = t.reference$NAMELSAD[t.reference$GEOID == query],
                 GEOGRAPHY = "Combined Statistical Area",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Combined Statistical Area"],
                 GEOID = query,
                 STATE = "No state container"))
      } else {
        stop(paste0("No combined statistical area with code ", query, "."))
      }
    } else if (nchar(query) == 5) {
      t.reference.1 <- suppressMessages(tigris::core_based_statistical_areas(year = 2020, cb = TRUE))
      if (substr(query, 0, 2) %in% k.states$GEOID) {
        t.reference.2 <- suppressMessages(tigris::counties(state = k.states$SHORT[which(k.states$GEOID == substr(query, 0, 2))], year = 2020, cb = TRUE))
      }
      if (exists("t.reference.2")) {
        if (query %in% t.reference.1$GEOID & query %in% t.reference.2$GEOID) {
          return(list(
            c(NAME = t.reference.1$NAMELSAD[t.reference.1$GEOID == query],
              GEOGRAPHY = "Core-Based Statistical Area",
              LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Core-Based Statistical Area"],
              GEOID = query,
              STATE = "No state container"),
            c(NAME = t.reference.2$NAMELSAD[which(t.reference.2$GEOID == query)],
              GEOGRAPHY = "County",
              LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "County"],
              GEOID = query,
              STATE = k.states$SHORT[which(k.states$GEOID == substr(query, 0, 2))])
          ))
        } else if (query %in% t.reference.1$GEOID & !query %in% t.reference.2$GEOID) {
          return(c(NAME = t.reference.1$NAMELSAD[t.reference.1$GEOID == query],
                   GEOGRAPHY = "Core-Based Statistical Area",
                   LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Core-Based Statistical Area"],
                   GEOID = query,
                   STATE = "No state container"))
        } else if (!query %in% t.reference.1$GEOID & query %in% t.reference.2$GEOID) {
          return(c(NAME = t.reference.2$NAMELSAD[t.reference.2$GEOID == query],
                   GEOGRAPHY = "County",
                   LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "County"],
                   GEOID = query,
                   STATE = k.states$SHORT[which(k.states$GEOID == substr(query, 0, 2))]))
        } else {
          stop(paste0("No core-based statistical area or county with code ", query, "."))
        }
      } else {
        if (query %in% t.reference.1$GEOID) {
          return(c(NAME = t.reference.1$NAMELSAD[t.reference.1$GEOID == query],
                   GEOGRAPHY = "Core-Based Statistical Area",
                   LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Core-Based Statistical Area"],
                   GEOID = query,
                   STATE = "No state container"))
        } else {
          stop(paste0("No core-based statistical area with code ", query, "."))
        }
      }
    } else if (nchar(query) == 10) {
      t.reference <- suppressMessages(tigris::metro_divisions(year = 2020))
      if (query %in% t.reference$GEOID) {
        return(c(NAME = t.reference$NAMELSAD[t.reference$GEOID == query],
                 GEOGRAPHY = "Metropolitan Division",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Metropolitan Division"],
                 GEOID = query,
                 STATE = "No state container"))
      } else {
        stop(paste0("No metropolitan division with code ", query, "."))
      }
    } else if (nchar(query) == 7) {
      if (substr(query, 0, 2) %in% k.states$GEOID) {
        t.reference.1 <- suppressMessages(tigris::zctas(state = k.states$SHORT[which(k.states$GEOID == substr(query, 0, 2))], year = 2010))
        t.reference.2 <- suppressMessages(tigris::school_districts(state = k.states$SHORT[which(k.states$GEOID == substr(query, 0, 2))], year = 2020, cb = TRUE))
        t.reference.3 <- suppressMessages(tigris::places(state = k.states$SHORT[which(k.states$GEOID == substr(query, 0, 2))], year = 2020, cb = TRUE))
      } else {
        stop("Unable to classify geography.")
      }
      if (query %in% t.reference.1$GEOID & query %in% t.reference.2$GEOID & query %in% t.reference.3$GEOID) {
        return(list(
          c(NAME = t.reference.1$ZCTA5CE10[t.reference.1$GEOID == query],
            GEOGRAPHY = "ZIP Code",
            LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "ZIP Code"],
            GEOID = query,
            STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)]),
          c(NAME = t.reference.2$NAMELSAD[t.reference.2$GEOID == query],
            GEOGRAPHY = "School District",
            LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "School District"],
            GEOID = query,
            STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)]),
          c(NAME = t.reference.3$NAMELSAD[which(t.reference.3$GEOID == query)],
            GEOGRAPHY = "Place",
            LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
            GEOID = query,
            STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)])
        ))
      } else if (query %in% t.reference.1$GEOID & !query %in% t.reference.2$GEOID & !query %in% t.reference.3$GEOID) {
        return(c(NAME = t.reference.1$ZCTA5CE10[which(t.reference.1$GEOID == query)],
                 GEOGRAPHY = "ZIP Code",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "ZIP Code"],
                 GEOID = query,
                 STATE = k.states$SHORT[which(k.states$GEOID == substr(query, 0, 2))]))
      } else if (!query %in% t.reference.1$GEOID & query %in% t.reference.2$GEOID & !query %in% t.reference.3$GEOID) {
        return(c(NAME = t.reference.2$NAMELSAD[t.reference.2$GEOID == query],
                 GEOGRAPHY = "School District",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "School District"],
                 GEOID = query,
                 STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)]))
      }  else if (!query %in% t.reference.1$GEOID & query %in% t.reference.2$GEOID & query %in% t.reference.3$GEOID) {
        return(list(
          c(NAME = t.reference.2$NAMELSAD[t.reference.2$GEOID == query],
            GEOGRAPHY = "School District",
            LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "School District"],
            GEOID = query,
            STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)]),
          c(NAME = t.reference.3$NAMELSAD[t.reference.3$GEOID == query],
            GEOGRAPHY = "Place",
            LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
            GEOID = query,
            STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)])
        ))
      }  else if (query %in% t.reference.1$GEOID & !query %in% t.reference.2$GEOID & query %in% t.reference.3$GEOID) {
        return(list(
          c(NAME = t.reference.1$ZCTA5CE10[t.reference.1$GEOID == query],
            GEOGRAPHY = "ZIP Code",
            LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "ZIP Code"],
            GEOID = query,
            STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)]),
          c(NAME = t.reference.3$NAMELSAD[t.reference.3$GEOID == query],
            GEOGRAPHY = "Place",
            LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
            GEOID = query,
            STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)])
        ))
      } else if (!query %in% t.reference.1$GEOID & !query %in% t.reference.2$GEOID & query %in% t.reference.3$GEOID) {
        return(c(NAME = t.reference.3$NAMELSAD[t.reference.3$GEOID == query],
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = query,
                 STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)]))
      } else {
        stop(paste0("No ZIP codes, school districts, or places with code ", query, "."))
      }
    } else if (nchar(query) == 11) {
      if (substr(query, 0, 2) %in% k.states$GEOID) {
        t.reference <- suppressMessages(tigris::tracts(state = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)], year = 2020, cb = TRUE))
        if (query %in% t.reference$GEOID) {
          return(c(NAME = t.reference$NAMELSAD[t.reference$GEOID == query],
                   GEOGRAPHY = "Tract",
                   LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Tract"],
                   GEOID = query,
                   STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)]))
        } else {
          stop(paste0("No tract with code ", query, "."))
        }
      } else {
        stop("Unable to classify geography.")
      }
    } else if (nchar(query) == 12) {
      if (substr(query, 0, 2) %in% k.states$GEOID) {
        t.reference <- suppressMessages(tigris::block_groups(state = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)], year = 2020, cb = TRUE))
        if (query %in% t.reference$GEOID) {
          return(c(NAME = t.reference$NAMELSAD[t.reference$GEOID == query],
                   GEOGRAPHY = "Block Group",
                   LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Block Group"],
                   GEOID = query,
                   STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)]))
        } else {
          stop(paste0("No block group with code ", query, "."))
        }
      } else {
        stop("Unable to classify geography.")
      }
    } else if (nchar(query) == 15) {
      if (substr(query, 0, 2) %in% k.states$GEOID) {
        t.reference <- suppressMessages(tigris::blocks(state = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)], year = 2020))
        if (query %in% t.reference$GEOID) {
          return(c(NAME = t.reference$NAME20[t.reference$GEOID == query],
                   GEOGRAPHY = "Block",
                   LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Block"],
                   GEOID = query,
                   STATE = k.states$SHORT[k.states$GEOID == substr(query, 0, 2)]))
        } else {
          stop(paste0("No block with code ", query, "."))
        }
      } else {
        stop("Failed to classify geography.")
      }
    } else {
      stop("Failed to classify geography.")
    }
  } else {
    query <- toupper(trimws(query))
    
    if (sum(grepl(paste0("'", query, "'"), toupper(k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Nation"]))) > 0) {
      return(c(NAME = "Nation",
               GEOGRAPHY = "Nation",
               LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Nation"],
               GEOID = "US",
               STATE = "No state container"))
    } else if (query %in% toupper(do.call(paste0, expand.grid(c("South", "Midwest", "West", "Northeast"), c("", " Region"))))) {
      t.reference <- suppressMessages(tigris::regions(year = 2020))
      return(c(NAME = t.reference$NAMELSAD[c(which(toupper(t.reference$NAMELSAD) == query), which(toupper(t.reference$NAME) == query))],
               GEOGRAPHY = "Region",
               LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Region"],
               GEOID = t.reference$GEOID[c(which(toupper(t.reference$NAMELSAD) == query), which(toupper(t.reference$NAME) == query))],
               STATE = "No state container"))
    } else if (query %in% toupper(do.call(paste0, expand.grid(c("South Atlantic", "East North Central", "Mountain", "Pacific", "Middle Atlantic", "West South Central", "East South Central", "New England"), c("", " Division"))))) {
      t.reference <- suppressMessages(tigris::divisions(year = 2020))
      return(c(NAME = t.reference$NAMELSAD[c(which(toupper(t.reference$NAMELSAD) == query), which(toupper(t.reference$NAME) == query))],
               GEOGRAPHY = "Region",
               LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Region"],
               GEOID = t.reference$GEOID[c(which(toupper(t.reference$NAMELSAD) == query), which(toupper(t.reference$NAME) == query))],
               STATE = "No state container"))
    } else if (query %in% toupper(k.states$NAME) | query %in% toupper(k.states$SHORT)) {
      return(c(NAME = k.states$NAME[c(which(toupper(k.states$NAME) == query), which(toupper(k.states$SHORT) == query))],
               GEOGRAPHY = "State",
               LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "State"],
               GEOID = k.states$GEOID[c(which(toupper(k.states$NAME) == query), which(toupper(k.states$SHORT) == query))],
               STATE = "No state container"))
    } else if (paste0(word(query, -2:-1), collapse = " ") %in% toupper(k.states$NAME) | word(query, -1) %in% toupper(k.states$NAME) | word(query, -1) %in% toupper(k.states$SHORT)) {
      if (paste0(word(query, -2:-1), collapse = " ") %in% toupper(k.states$NAME)) {
        t.container <- k.states$SHORT[toupper(k.states$NAME) == paste0(word(query, -2:-1), collapse = " ")]
        query <- gsub(paste0(", ", paste0(word(query, -2:-1), collapse = " ")), "", query)
        query <- gsub(paste0(" ", paste0(word(query, -2:-1), collapse = " ")), "", query)
      } else if (word(query, -1) %in% toupper(k.states$NAME)) {
        t.container <- k.states$SHORT[toupper(k.states$NAME) == word(query, -1)]
        if (sum(grepl(",", query)) > 0) {
          query <- gsub(paste0(", ", word(query, -1)), "", query)
        } else {
          query <- gsub(paste0(" ", word(query, -1)), "", query)
        }
      } else {
        t.container <- word(query, -1)
        if (sum(grepl(",", query)) > 0) {
          query <- gsub(paste0(", ", word(query, -1)), "", query)
        } else {
          query <- gsub(paste0(" ", word(query, -1)), "", query)
        }
      }
      if (word(query, 1) %in% c("CITY", "TOWN", "VILLAGE", "BOROUGH") | word(query, -1) %in% c("CITY", "TOWN", "VILLAGE", "CDP", "BOROUGH")) {
        if (paste0(word(query, 1:2), collapse = " ") == "CITY OF") {
          query <- gsub("CITY OF ", "", query)
          query <- paste0(query, " CITY")
        } else if (paste0(word(query, 1:2), collapse = " ") == "TOWN OF") {
          query <- gsub("TOWN OF ", "", query)
          query <- paste0(query, " TOWN")
        } else if (paste0(word(query, 1:2), collapse = " ") == "VILLAGE OF") {
          query <- gsub("VILLAGE OF ", "", query)
          query <- paste0(query, " VILLAGE")
        } else if (paste0(word(query, 1:2), collapse = " ") == "BOROUGH OF") {
          query <- gsub("BOROUGH OF ", "", query)
          query <- paste0(query, " BOROUGH")
        }
        t.reference <- suppressMessages(tigris::places(state = t.container, year = 2020, cb = TRUE))
        if (query %in% toupper(t.reference$NAMELSAD)) {
          return(c(NAME = t.reference$NAMELSAD[toupper(t.reference$NAMELSAD) == query],
                   GEOGRAPHY = "Place",
                   LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                   GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == query],
                   STATE = t.container))
        } else {
          stop(paste0("No place with name ", query, "."))
        }
      } else if (word(query, -1) == "COUNTY") {
        t.reference <- suppressMessages(tigris::counties(state = t.container, year = 2020, cb = TRUE))
        return(c(NAME = t.reference$NAMELSAD[toupper(t.reference$NAMELSAD) == query],
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == query],
                 STATE = t.container))
      } else {
        t.reference <- suppressMessages(tigris::places(state = t.container, year = 2020, cb = TRUE))
        t.out <- list()
        i.out <- 1 # Debugging
        if (sum(grepl(query, toupper(t.reference$NAME))) > 0) {
          for (i.out in 1:sum(grepl(query, toupper(t.reference$NAME)))) {
            t.out[[i.out]] <- c(NAME = t.reference$NAMELSAD[grepl(query, toupper(t.reference$NAME))][i.out],
                                GEOGRAPHY = "Place",
                                LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                                GEOID = t.reference$GEOID[grepl(query, toupper(t.reference$NAME))][i.out],
                                STATE = t.container)
          }
          if (length(t.out) == 0) {
            stop("Failed to classify geography.")
          } else if (length(t.out) == 1) {
            return(unlist(t.out))
          } else {
            return(t.out)
          }
        } else {
          stop("Failed to classify geography.")
        }
      }
    } else if (word(query, -1) == "MSA" | paste0(word(query, -2:-1), collapse = " ") %in% c("METRO AREA", "METROPOLITAN AREA") | paste0(word(query, -3:-1), collapse = " ") == "METROPOLITAN STATISTICAL AREA") {
      if (word(query, -1) == "MSA") {
        query <- gsub(" MSA", "", query)
      } else if (paste0(word(query, -2:-1), collapse = " ") == "METRO AREA") {
        query <- gsub(" METRO AREA", "", query)
      } else if (paste0(word(query, -2:-1), collapse = " ") == "METROPOLITAN AREA") {
        query <- gsub(" METROPOLITAN AREA", "", query)
      } else if (paste0(word(query, -3:-1), collapse = " ") == "METROPOLITAN STATISTICAL AREA") {
        query <- gsub(" METROPOLITAN STATISTICAL AREA", "", query)
      }
      t.reference <- suppressMessages(tigris::metro_divisions(year = 2020))
      t.reference$NAMETEMP <- sub("\\,.*", "", t.reference$NAME)
      if (query %in% toupper(t.reference$NAMETEMP)) {
        return(c(NAME = t.reference$NAMELSAD[toupper(t.reference$NAMETEMP) == query],
                 GEOGRAPHY = "Metropolitan Division",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Metropolitan Division"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMETEMP) == query],
                 STATE = "No state container"))
      } else {
        stop("Failed to clasify geography. Did you remember to add '-' in the MSA name?")
      }
    } else if (word(query, -1) == "CSA" | paste0(word(query, -3:-1), collapse = " ") == "COMBINED STATISTICAL AREA") {
      if (word(query, -1) == "CSA") {
        query <- gsub(" CSA", "", query)
      } else if (paste0(word(query, -3:-1), collapse = " ") == "COMBINED STATISTICAL AREA") {
        query <- gsub(" COMBINED STATISTICAL AREA", "", query)
      }
      t.reference <- suppressMessages(tigris::combined_statistical_areas(cb = TRUE, year = 2020))
      t.reference$NAMETEMP <- sub("\\,.*", "", t.reference$NAME)
      if (query %in% toupper(t.reference$NAMETEMP)) {
        return(c(NAME = t.reference$NAMELSAD[toupper(t.reference$NAMETEMP) == query],
                 GEOGRAPHY = "Combined Statistical Area",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Combined Statistical Area"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMETEMP) == query],
                 STATE = "No state container"))
      } else {
        stop("Failed to clasify geography. Did you remember to add '-' in the CSA name?")
      }
    } else if (word(query, -1) == "CBSA" | paste0(word(query, -3:-1), collapse = " ") == "CORE-BASED STATISTICAL AREA" | paste0(word(query, -4:-1), collapse = " ") == "CORE BASED STATISTICAL AREA") {
      if (word(query, -1) == "CBSA") {
        query <- gsub(" CBSA", "", query)
      } else if (paste0(word(query, -3:-1), collapse = " ") == "CORE-BASED STATISTICAL AREA") {
        query <- gsub(" CORE-BASED STATISTICAL AREA", "", query)
      } else if (paste0(word(query, -4:-1), collapse = " ") == "CORE BASED STATISTICAL AREA") {
        query <- gsub(" CORE BASED STATISTICAL AREA", "", query)
      }
      t.reference <- suppressMessages(tigris::core_based_statistical_areas(cb = TRUE, year = 2020))
      t.reference$NAMETEMP <- sub("\\,.*", "", t.reference$NAME)
      if (query %in% toupper(t.reference$NAMETEMP)) {
        return(c(NAME = t.reference$NAMELSAD[toupper(t.reference$NAMETEMP) == query],
                 GEOGRAPHY = "Combined Statistical Area",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Combined Statistical Area"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMETEMP) == query],
                 STATE = "No state container"))
      } else {
        stop("Failed to clasify geography. Did you remember to add '-' in the CBSA name?")
      }
    } else {
      if (query == "NYC" | query == "NEW YORK CITY") {
        t.reference <- suppressMessages(tigris::places(state = "NY", cb = TRUE, year = 2020))
        return(c(NAME = "New York city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "NEW YORK CITY"],
                 STATE = "NY"))
      } else if (query == "LA" | query == "LOS ANGELES" | query == "LOS ANGELES CITY") {
        t.reference <- suppressMessages(tigris::places(state = "CA", cb = TRUE, year = 2020))
        return(c(NAME = "Los Angeles city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "LOS ANGELES CITY"],
                 STATE = "CA"))
      } else if (query == "CHICAGO" | query == "CHICAGO CITY") {
        t.reference <- suppressMessages(tigris::places(state = "IL", cb = TRUE, year = 2020))
        return(c(NAME = "Chicago city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "CHICAGO CITY"],
                 STATE = "IL"))
      } else if (query == "HOUSTON" | query == "HOUSTON CITY") {
        t.reference <- suppressMessages(tigris::places(state = "TX", cb = TRUE, year = 2020))
        return(c(NAME = "Houston city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "HOUSTON CITY"],
                 STATE = "TX"))
      } else if (query == "PHOENIX" | query == "PHOENIX CITY") {
        t.reference <- suppressMessages(tigris::places(state = "AZ", cb = TRUE, year = 2020))
        return(c(NAME = "Phoenix city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "PHOENIX CITY"],
                 STATE = "AZ"))
      } else if (query == "PHILADELPHIA" | query == "PHILADELPHIA CITY") {
        t.reference <- suppressMessages(tigris::places(state = "PA", cb = TRUE, year = 2020))
        return(c(NAME = "Philadelphia city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "PHILADELPHIA CITY"],
                 STATE = "PA"))
      } else if (query == "SAN ANTONIO" | query == "SAN ANTONIO CITY") {
        t.reference <- suppressMessages(tigris::places(state = "TX", cb = TRUE, year = 2020))
        return(c(NAME = "San Antonio city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "SAN ANTONIO CITY"],
                 STATE = "TX"))
      } else if (query == "SAN DIEGO" | query == "SAN DIEGO CITY") {
        t.reference <- suppressMessages(tigris::places(state = "CA", cb = TRUE, year = 2020))
        return(c(NAME = "San Diego city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "SAN DIEGO CITY"],
                 STATE = "CA"))
      } else if (query == "DALLAS" | query == "DALLAS CITY") {
        t.reference <- suppressMessages(tigris::places(state = "TX", cb = TRUE, year = 2020))
        return(c(NAME = "Dallas city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "DALLAS CITY"],
                 STATE = "TX"))
      } else if (query == "JACKSONVILLE" | query == "JACKSONVILLE CITY") {
        t.reference <- suppressMessages(tigris::places(state = "FL", cb = TRUE, year = 2020))
        return(c(NAME = "Jacksonville city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "JACKSONVILLE CITY"],
                 STATE = "FL"))
      } else if (query == "FORT WORTH" | query == "FORT WORTH CITY") {
        t.reference <- suppressMessages(tigris::places(state = "TX", cb = TRUE, year = 2020))
        return(c(NAME = "Fort Worth city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "FORT WORTH CITY"],
                 STATE = "TX"))
      } else if (query == "SAN JOSE" | query == "SAN JOSE CITY") {
        t.reference <- suppressMessages(tigris::places(state = "CA", cb = TRUE, year = 2020))
        return(c(NAME = "San Jose city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "SAN JOSE CITY"],
                 STATE = "CA"))
      } else if (query == "AUSTIN" | query == "AUSTIN CITY") {
        t.reference <- suppressMessages(tigris::places(state = "TX", cb = TRUE, year = 2020))
        return(c(NAME = "Austin city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "AUSTIN CITY"],
                 STATE = "TX"))
      } else if (query == "CHARLOTTE" | query == "CHARLOTTE CITY") {
        t.reference <- suppressMessages(tigris::places(state = "NC", cb = TRUE, year = 2020))
        return(c(NAME = "Charlotte city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "CHARLOTTE CITY"],
                 STATE = "NC"))
      } else if (query == "COLUMBUS" | query == "COLUMBUS CITY") {
        t.reference <- suppressMessages(tigris::places(state = "OH", cb = TRUE, year = 2020))
        return(c(NAME = "Columbus city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "COLUMBUS CITY"],
                 STATE = "OH"))
      } else if (query == "INDIANAPOLIS" | query == "INDIANAPOLIS CITY") {
        t.reference <- suppressMessages(tigris::places(state = "IN", cb = TRUE, year = 2020))
        return(c(NAME = "Indianapolis city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "INDIANAPOLIS CITY"],
                 STATE = "IN"))
      } else if (query == "SAN FRANCISCO" | query == "SAN FRANCISCO CITY") {
        t.reference <- suppressMessages(tigris::places(state = "CA", cb = TRUE, year = 2020))
        return(c(NAME = "San Francisco city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "SAN FRANCISCO CITY"],
                 STATE = "CA"))
      } else if (query == "SEATTLE" | query == "SEATTLE CITY") {
        t.reference <- suppressMessages(tigris::places(state = "WA", cb = TRUE, year = 2020))
        return(c(NAME = "Seattle city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "SEATTLE CITY"],
                 STATE = "WA"))
      } else if (query == "DENVER" | query == "DENVER CITY") {
        t.reference <- suppressMessages(tigris::places(state = "CO", cb = TRUE, year = 2020))
        return(c(NAME = "Denver city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "DENVER CITY"],
                 STATE = "CO"))
      } else if (query == "OKLAHOMA CITY") {
        t.reference <- suppressMessages(tigris::places(state = "OK", cb = TRUE, year = 2020))
        return(c(NAME = "Oklahoma city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "OKLAHOMA CITY"],
                 STATE = "OK"))
      } else if (query == "NASHVILLE" | query == "NASHVILLE CITY") {
        t.reference <- suppressMessages(tigris::places(state = "TN", cb = TRUE, year = 2020))
        return(c(NAME = "Nashville-Davidson metropolitan government (balance)",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[t.reference$NAMELSAD == "Nashville-Davidson metropolitan government (balance)"],
                 STATE = "TN"))
      } else if (query == "WASHINGTON D.C." | query == "D.C." | query == "DC" | query == "WASHINGTON DC") {
        t.reference <- suppressMessages(tigris::places(state = "DC", cb = TRUE, year = 2020))
        return(c(NAME = "Washington city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "WASHINGTON CITY"],
                 STATE = "DC"))
      } else if (query == "EL PASO" | query == "EL PASO CITY") {
        t.reference <- suppressMessages(tigris::places(state = "TX", cb = TRUE, year = 2020))
        return(c(NAME = "El Paso city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "EL PASO CITY"],
                 STATE = "TX"))
      } else if (query == "LAS VEGAS" | query == "LAS VEGAS CITY" | query == "VEGAS") {
        t.reference <- suppressMessages(tigris::places(state = "NV", cb = TRUE, year = 2020))
        return(c(NAME = "Las Vegas city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "LAS VEGAS CITY"],
                 STATE = "NV"))
      } else if (query == "BOSTON" | query == "BOSTON CITY") {
        t.reference <- suppressMessages(tigris::places(state = "MA", cb = TRUE, year = 2020))
        return(c(NAME = "Boston city",
                 GEOGRAPHY = "Place",
                 LEVEL = k.geographies$LEVEL[k.geographies$GEOGRAPHY == "Place"],
                 GEOID = t.reference$GEOID[toupper(t.reference$NAMELSAD) == "BOSTON CITY"],
                 STATE = "MA"))
      } else {
        stop("Failed to clasify geography.")
      }
    }
  }
}
