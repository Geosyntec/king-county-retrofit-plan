library(shiny)
library(rgdal)
library(mapview)

df <- readOGR(dsn=here("data","nc.shp"))
head(df) %>% as.data.frame
