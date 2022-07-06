## code to prepare `DATASET` dataset goes here
library(dplyr)
watershed_shapefile <- sf::read_sf(here::here('data-raw/source_data','kingCosheds.shp'))
watershed_dataframe <- watershed_shapefile %>% sf::st_drop_geometry() %>% as.data.frame()

usethis::use_data(watershed_shapefile, overwrite = TRUE)
usethis::use_data(watershed_dataframe, overwrite = TRUE)
