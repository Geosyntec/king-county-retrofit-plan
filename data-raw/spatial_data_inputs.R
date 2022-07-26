library(readxl)
library(tidyverse)

#subbasin metric table
subbasin_metrics <- read_excel(
  "data-raw/source_data/20220722_KC_SPS_Complete_Metrics_Join_Only.xls") %>%
  mutate_at(c("Presence_of_Shellfish",
              "Drains_to_P_Sensitive_Lake",
              "Presence_of_Coho_Bearing_Streams",
              "Is_Headwater_Basin",
              "Contains_Swimming_Beaches"
  ),as.logical)

usethis::use_data(subbasin_metrics,overwrite = TRUE)


# Jurisdiction Boundaries -------------------------------------------------

library(arcpullr)

king_co_server <-"https://gisdata.kingcounty.gov/arcgis/rest/services"
cities_url <- "OpenDataPortal/admin___base/MapServer/446"
kingco_url <- paste(king_co_server,cities_url,sep ="/")
cities_shp <- get_spatial_layer(kingco_url)

city_names <- cities_shp %>% sf::st_drop_geometry() %>%
  dplyr::select("CITYNAME") %>% distinct()


usethis::use_data(cities_shp,overwrite = TRUE)
usethis::use_data(city_names, overwrite = TRUE)


