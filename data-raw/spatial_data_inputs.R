library(readxl)
library(tidyverse)
library(sf)
library(spdplyr)
library(ggplot2)
library(magrittr)
library(sf)
library(rmapshaper)
sf_use_s2(FALSE)

subbasin_shps <- sf::read_sf(here::here(
  "data-raw/source_data",
  "Subwatershed_Metrics_Complete_Export.shp"
))%>% st_zm() %>% st_transform("EPSG:4326")# %>% column_to_rownames("SWSID") %>%
  #sf::st_as_sf()

base_crs <- subbasin_shps %>% st_crs()

## simplify shapes


subbasin_simplified <- ms_simplify(subbasin_shps, keep = 0.10,
                                keep_shapes = FALSE) %>% st_set_crs(base_crs)


system.time({
  leaflet(subbasin_simplified) %>% addPolygons()
})
subbasin_shps<- subbasin_simplified

usethis::use_data(subbasin_shps,overwrite = TRUE)

# subbasin metrics --------------------------------------------------------
subbasin_metrics <- read_excel("data-raw/source_data/20220722_KC_SPS_Complete_Metrics_Join_Only.xls") %>%
  mutate_at(
    c(
      "Presence_of_Shellfish",
      "Drains_to_P_Sensitive_Lake",
      "Presence_of_Coho_Bearing_Streams",
      "Is_Headwater_Basin",
      "Contains_Swimming_Beaches"
    ),
    as.logical
  ) %>% column_to_rownames("SWSID")

usethis::use_data(subbasin_metrics, overwrite = TRUE)


# Jurisdiction Boundaries -------------------------------------------------

library(arcpullr)

king_co_server <-
  "https://gisdata.kingcounty.gov/arcgis/rest/services"
cities_url <- "OpenDataPortal/admin___base/MapServer/446"
kingco_url <- paste(king_co_server, cities_url, sep = "/")

cities_shp <- get_spatial_layer(kingco_url) %>% st_transform("EPSG:4326") %>%
  st_zm() %>%
  dplyr::select(c(CITYNAME))%>% st_set_crs(base_crs) #%>% st_cast('MULTIPOLYGON')

city_names <- cities_shp %>%
  sf::st_drop_geometry() %>%
  distinct()


usethis::use_data(cities_shp, overwrite = TRUE)
usethis::use_data(city_names, overwrite = TRUE)


# joined_shps <- sf::st_join(cities_shp,subbasin_shps) %>%
#   #select(c(SWSID , CITYNAME)) %>%
#   tibble::remove_rownames() %>%
#   sf::st_drop_geometry()
#
# city_lookup <- aggregate(joined_shps$SWSID,list(joined_shps$CITYNAME),unlist,simplify=FALSE) %>%
#   rename(CITYNAME = Group.1, SWSID = x
#          )


usethis::use_data(cities_shp,overwrite = TRUE )
# usethis::use_data(city_lookup,overwrite = TRUE )
