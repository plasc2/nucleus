#' Visualize Site
#'
#' This function produces a minimalistic visualization of a given site.
#'
#' @param site A string or integer GEOID describing a geographic area in the U.S.
#' @param device File output type as either jpg, png, or pdf.
#' @param output_directory A folder to save the final plot.
#' @return A map given argument 'device'
#' @export
#' 
#' 
visualize_site <- function(site, device = "jpg", output_directory) {
  query <- standardize_query(site)
  if (length(query) != 1 & typeof(query) != "character") {
    stop("Site must produce query of length 1. If passing a string, be sure to add identifiers like 'city', 'village', 'metropolitan area', etc.")
  }
  f.get <- getFromNamespace(k.geographies$ID_TIGRIS[as.numeric(query["LEVEL"])], "tigris")
  if (as.numeric(query["LEVEL"]) > level_from_alias("State") & !as.numeric(query["LEVEL"]) %in% level_from_alias(c("CSA", "CBSA", "MSA"))) {
    if (!as.numeric(query["LEVEL"]) %in% level_from_alias(c("ZIP Code", "Block"))) {
      t.unit <- suppressMessages(f.get(state = query["STATE"], year = 2020, cb = TRUE))
    } else {
      t.unit <- suppressMessages(f.get(state = query["STATE"], year = 2020))
    }
    t.unit <- t.unit[t.unit$GEOID == query["GEOID"], ]
    if (nrow(t.unit) == 0) {
      stop("Failed to locate site.")
    }
    if (as.numeric(query["LEVEL"]) > level_from_alias("County")) {
      t.bbox <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(t.unit, 10000)))
      t.counties <- suppressMessages(sf::st_filter(tigris::counties(state = query["STATE"], cb = TRUE, year = 2020), t.bbox, .predicate = sf::st_intersects))
      t.counties <- t.counties$COUNTYFP
      for (i.county in t.counties) {
        i.roads.detail <- suppressMessages(tigris::roads(state = query["STATE"], county = i.county))
        # i.roads.detail <- i.roads.detail[!is.na(i.roads.detail$RTTYP),]
        i.roads.detail <- i.roads.detail[!i.roads.detail$RTTYP %in% c("M", "S"), ]
        i.roads.detail <- sf::st_make_valid(i.roads.detail)
        i.roads.detail <- sf::st_simplify(i.roads.detail)
        if (!exists("t.roads.detail")) {
          t.roads.detail <- i.roads.detail
        } else {
          t.roads.detail <- rbind(t.roads.detail, i.roads.detail)
        }
        rm(i.roads.detail, i.county)
      }
      t.roads.detail <- sf::st_filter(t.roads.detail, t.bbox, .predicate = sf::st_intersects)
    } else {
      t.bbox <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(t.unit, 20000)))
    }
    t.roads <- suppressMessages(tigris::primary_secondary_roads(query["STATE"]))
    t.roads <- sf::st_filter(t.roads, t.bbox, .predicate = sf::st_intersects)
    if (!exists("t.counties")) {
      t.counties <- suppressMessages(sf::st_filter(tigris::counties(state = query["STATE"], cb = TRUE, year = 2020), t.bbox, .predicate = sf::st_intersects))
      t.counties <- t.counties$COUNTYFP
    }
    for (i.county in t.counties) {
      i.water <- suppressMessages(tigris::area_water(state = query["STATE"], county = i.county))
      i.water <- i.water[i.water$AWATER >= 2000, ]
      i.water <- sf::st_make_valid(i.water)
      i.water <- sf::st_simplify(i.water)
      if (!exists("t.water")) {
        t.water <- i.water
      } else {
        t.water <- rbind(t.water, i.water)
      }
      rm(i.water, i.county)
    }
  } else {
    t.unit <- suppressMessages(f.get(year = 2020))
    t.unit <- t.unit[t.unit$GEOID == query["GEOID"], ]
    if (nrow(t.unit) == 0) {
      stop("Failed to locate site.")
    }
    t.bbox <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(t.unit, 50000)))
    t.roads <- suppressMessages(tigris::primary_roads())
    t.roads <- sf::st_filter(t.roads, t.bbox, .predicate = sf::st_intersects)
  }
  if (exists("t.roads.detail")) {
    t.out <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = t.bbox, fill = "#1E1E1E", color = NA) +
      ggplot2::geom_sf(data = t.water, fill = "#333333", color = NA) +
      ggplot2::geom_sf(data = t.roads.detail, fill = NA, color = ggplot2::alpha("white", 0.7), linewidth = 0.1) +
      ggplot2::geom_sf(data = t.roads, fill = NA, color = ggplot2::alpha("white", 0.80), linewidth = 0.15) +
      ggplot2::geom_sf(data = t.unit, fill = NA, color = ggplot2::alpha("#1E1E1E", 0.5), linewidth = 3) +
      ggplot2::geom_sf(data = t.unit, fill = NA, color = "white", linewidth = 0.75) +
      ggplot2::coord_sf(xlim = c(sf::st_bbox(t.bbox)["xmin"], sf::st_bbox(t.bbox)["xmax"]), ylim = c(sf::st_bbox(t.bbox)["ymin"], sf::st_bbox(t.bbox)["ymax"]), expand = FALSE, clip = "on") +
      ggplot2::theme(axis.text.x=ggplot2::element_blank(),
            axis.ticks.x=ggplot2::element_blank(),
            axis.title.x=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_blank(),
            axis.ticks.y=ggplot2::element_blank(),
            axis.title.y=ggplot2::element_blank(),
            legend.position="none",
            plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
            panel.spacing = ggplot2::unit(0, "pt"))
  } else {
    if (exists("t.water")) {
      t.out <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = t.bbox, fill = "#1E1E1E", color = NA) +
        ggplot2::geom_sf(data = t.water, fill = "#212121", color = NA) +
        ggplot2::geom_sf(data = t.roads, fill = NA, color = ggplot2::alpha("white", 0.80), linewidth = 0.15) +
        ggplot2::geom_sf(data = t.unit, fill = NA, color = ggplot2::alpha("#1E1E1E", 0.5), linewidth = 3) +
        ggplot2::geom_sf(data = t.unit, fill = NA, color = "white", linewidth = 0.75) +
        ggplot2::coord_sf(xlim = c(sf::st_bbox(t.bbox)["xmin"], sf::st_bbox(t.bbox)["xmax"]), ylim = c(sf::st_bbox(t.bbox)["ymin"], sf::st_bbox(t.bbox)["ymax"]), expand = FALSE, clip = "on") +
        ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                       axis.ticks.x=ggplot2::element_blank(),
                       axis.title.x=ggplot2::element_blank(),
                       axis.text.y=ggplot2::element_blank(),
                       axis.ticks.y=ggplot2::element_blank(),
                       axis.title.y=ggplot2::element_blank(),
                       legend.position="none",
                       plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
                       panel.spacing = ggplot2::unit(0, "pt"))
    } else {
      t.out <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = t.bbox, fill = "#1E1E1E", color = NA) +
        ggplot2::geom_sf(data = t.roads, fill = NA, color = ggplot2::alpha("white", 0.80), linewidth = 0.15) +
        ggplot2::geom_sf(data = t.unit, fill = NA, color = ggplot2::alpha("#1E1E1E", 0.5), linewidth = 3) +
        ggplot2::geom_sf(data = t.unit, fill = NA, color = "white", linewidth = 0.75) +
        ggplot2::coord_sf(xlim = c(sf::st_bbox(t.bbox)["xmin"], sf::st_bbox(t.bbox)["xmax"]), ylim = c(sf::st_bbox(t.bbox)["ymin"], sf::st_bbox(t.bbox)["ymax"]), expand = FALSE, clip = "on") +
        ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                       axis.ticks.x=ggplot2::element_blank(),
                       axis.title.x=ggplot2::element_blank(),
                       axis.text.y=ggplot2::element_blank(),
                       axis.ticks.y=ggplot2::element_blank(),
                       axis.title.y=ggplot2::element_blank(),
                       legend.position="none",
                       plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
                       panel.spacing = ggplot2::unit(0, "pt"))
    }
  }
  ggplot2::ggsave(file.path(output_directory, paste0("plot.", device)), plot = t.out, device = device, width = 2000, height = 2000, units = "px")
}