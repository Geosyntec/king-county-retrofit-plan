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
)) %>%
  st_zm() %>%
  st_transform("EPSG:4326") # %>% column_to_rownames("SWSID") %>%


base_crs <- subbasin_shps %>% st_crs()

## simplify shapes


subbasin_simplified <- ms_simplify(subbasin_shps,
  keep = 0.10,
  keep_shapes = FALSE
) %>% st_set_crs(base_crs)

# test loading time.
# system.time({
#   leaflet(subbasin_simplified) %>% addPolygons()
# })
subbasin_shps <- subbasin_simplified %>% select(c(SWSID, geometry))

usethis::use_data(subbasin_shps, overwrite = TRUE)

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
  ) %>%
  column_to_rownames("SWSID")

usethis::use_data(subbasin_metrics, overwrite = TRUE)


# Jurisdiction Boundaries -------------------------------------------------
#
# library(arcpullr)
#
# king_co_server <-
#   "https://gisdata.kingcounty.gov/arcgis/rest/services"
# cities_url <- "OpenDataPortal/admin___base/MapServer/446"
# kingco_url <- paste(king_co_server, cities_url, sep = "/")
#
# cities_shp <- get_spatial_layer(kingco_url) %>% st_transform("EPSG:4326") %>%
#   st_zm() %>%
#   dplyr::select(c(CITYNAME))%>% st_set_crs(base_crs) #%>% st_cast('MULTIPOLYGON')

cities_shp <-
  sf::read_sf(here::here(
    "data-raw/source_data",
    "King_Co_Incorporated_Areas_Dissolved2.shp"
  )) %>%  mutate(CITYNAME = if_else(CITYNAME == "King County",  "King County (Unincorporated)", CITYNAME))


city_names <- cities_shp %>%
  sf::st_drop_geometry() %>%
  distinct() %>% arrange(CITYNAME) %>% pull(CITYNAME) %>% recode("King County" = "King County (Unincorporated)")

usethis::use_data(cities_shp, overwrite = TRUE)
usethis::use_data(city_names, overwrite = TRUE)
