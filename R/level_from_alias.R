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
#' Lookup table
k.geographies <- data.frame(GEOGRAPHY = c("Nation", "Region", "Division", "State", "Combined Statistical Area", "Core-Based Statistical Area", "Metropolitan Division", "County", "ZIP Code", "School District", "Place", "City", "Tract", "Block Group", "Block"))
k.geographies$LEVEL <- 1:nrow(k.geographies)
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Nation"] <- c(paste0("'", c("Nation", "U.S.", "US", "USA", "U.S.A", "National", "America"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Region"] <- c(paste0("'", c("Region", "Regions"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Division"] <- c(paste0("'", c("Division", "Divisions"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Combined Statistical Area"] <- c(paste0("'", c("Combined Statistical Area", "CSA", "Combined Statistical Areas", "CSAs"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Core-Based Statistical Area"] <- c(paste0("'", c("Core-Based Statistical Area", "CBSA", "Core-Based Statistical Areas", "Core Based Statistical Area", "Core Based Statistical Areas", "CBSAs"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Metropolitan Division"] <- c(paste0("'", c("Metropolitan Division", "MD", "MSA", "MSAs", "MDs", "Metros", "Metropolitan Areas", "Metropolitan Area", "Metropolitan Statistical Area", "Metropolitan Statistical Areas", "Metropolitan Divisions", "Metro"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "State"] <- c(paste0("'", c("State", "States"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "County"] <- c(paste0("'", c("County", "Counties"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "ZIP Code"] <- c(paste0("'", c("ZIP Code", "ZIP Codes", "ZIP Code Tabulation Areas", "ZCTA", "ZCTAs"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "School District"] <- c(paste0("'", c("School District", "School Districts", "ISDs", "ISD"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Place"] <- c(paste0("'", c("Place", "Places"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "City"] <- c(paste0("'", c("City", "Cities"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Tract"] <- c(paste0("'", c("Tract", "Tracts", "Census Tract", "Census Tracts"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Block Group"] <- c(paste0("'", c("Block Group", "Block Groups", "Census Block Groups", "Census Block Group", "BG"), "'", collapse = ", "))
k.geographies$ALIAS[k.geographies$GEOGRAPHY == "Block"] <- c(paste0("'", c("Block", "Blocks", "Census Block", "Census Blocks"), "'", collapse = ", "))
k.geographies$ID_LODES[k.geographies$GEOGRAPHY == "State"] <- "state"
k.geographies$ID_LODES[k.geographies$GEOGRAPHY == "County"] <- "county"
k.geographies$ID_LODES[k.geographies$GEOGRAPHY == "Tract"] <- "tract"
k.geographies$ID_LODES[k.geographies$GEOGRAPHY == "Block Group"] <- "bg"
k.geographies$ID_LODES[k.geographies$GEOGRAPHY == "Block"] <- "block"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "Nation"] <- "nation"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "Division"] <- "divisions"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "Region"] <- "regions"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "Combined Statistical Area"] <- "combined_statistical_areas"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "Core-Based Statistical Area"] <- "core_based_statistical_areas"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "Metropolitan Division"] <- "metro_divisions"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "State"] <- "states"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "County"] <- "counties"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "ZIP Code"] <- "zctas"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "School District"] <- "school_districts"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "Place"] <- "places"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "City"] <- "places"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "Tract"] <- "tracts"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "Block Group"] <- "block_groups"
k.geographies$ID_TIGRIS[k.geographies$GEOGRAPHY == "Block"] <- "blocks"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "Nation"] <- "nation"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "Division"] <- "division"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "Region"] <- "region"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "Combined Statistical Area"] <- "combined statistical area"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "Core-Based Statistical Area"] <- "cbsa"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "Metropolitan Division"] <- "metropolitan statistical area/micropolitan statistical area"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "State"] <- "state"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "County"] <- "county"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "ZIP Code"] <- "zcta"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "School District"] <- "school district (unified)"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "Place"] <- "place"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "City"] <- "place"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "Tract"] <- "tract"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "Block Group"] <- "block group"
k.geographies$ID_GETACS[k.geographies$GEOGRAPHY == "Block"] <- "block"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "Division"] <- "NAMELSAD"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "Region"] <- "NAMELSAD"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "Combined Statistical Area"] <- "NAMELSAD"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "Core-Based Statistical Area"] <- "NAMELSAD"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "Metropolitan Division"] <- "NAMELSAD"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "State"] <- "NAME"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "County"] <- "NAME"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "ZIP Code"] <- "GEOID"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "School District"] <- "NAME"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "Place"] <- "NAMELSAD"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "City"] <- "NAMELSAD"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "Tract"] <- "NAMELSAD"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "Block Group"] <- "NAMELSAD"
k.geographies$NAMECOLUMN[k.geographies$GEOGRAPHY == "Block"] <- "NAMELSAD"

#' Function
f.levelfromalias <- function(query) {
  t.level <- k.geographies$LEVEL[grepl(paste0(toupper(paste0("'", query, "'")), collapse = "|"), toupper(k.geographies$ALIAS))]
  return(t.level)
}