library(readxl)
library(tidyverse)
library(sf)

subbasin_shps <- sf::read_sf(here::here(
  "data-raw/source_data",
  "Subwatershed_Metrics_Complete_Export.shp"
))%>% st_zm() %>% st_transform("EPSG:3857")

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
  ) %>% column_to_rownames(SWSID)

usethis::use_data(subbasin_metrics, overwrite = TRUE)


# Jurisdiction Boundaries -------------------------------------------------

library(arcpullr)

king_co_server <-
  "https://gisdata.kingcounty.gov/arcgis/rest/services"
cities_url <- "OpenDataPortal/admin___base/MapServer/446"
kingco_url <- paste(king_co_server, cities_url, sep = "/")
cities_shp <- get_spatial_layer(kingco_url) %>% st_transform("EPSG:3857") %>%
  st_zm() %>%
  dplyr::select("CITYNAME")

city_names <- cities_shp %>%
  sf::st_drop_geometry() %>%
  distinct()


usethis::use_data(cities_shp, overwrite = TRUE)
usethis::use_data(city_names, overwrite = TRUE)



joined_shps <- sf::st_join(cities_shp,subbasin_shps) %>% select(c(SWSID , CITYNAME)) %>% tibble::remove_rownames() %>% sf::st_drop_geometry()

city_lookup <- aggregate(joined_shps$SWSID,list(joined_shps$CITYNAME),unlist,simplify=FALSE) %>%
  rename(CITYNAME = Group.1, SWSID = x
         )



usethis::use_data(city_lookup,overwrite = TRUE )
