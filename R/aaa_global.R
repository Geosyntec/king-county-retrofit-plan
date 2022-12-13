require(shiny)
require(rhandsontable)
require(shinydashboard)
require(shinyWidgets)
require(shinydashboardPlus)
require(sf)
require(dplyr)
require(spdplyr)
require(DT)
require(PROMETHEE)
require(tibble)
require(echarts4r)
require(reactable)
require(tidyr)
require(shinyjs)
require(fresh)
require(shinyvalidate)
require(apexcharter)

load(here::here("data", "wrias.rda"))
load(here::here("data", "subbasin_data.rda"))
load(here::here("data", "subbasin_shps.rda"))
load(here::here("data", "city_names.rda"))
load(here::here("data", "city_lookup.rda"))
load(here::here("data", "cities_shp.rda"))
load(here::here("data", "metrics.rda"))
source(here::here("R", "promethee.R"))
source(here::here("R", "normalize_pt.R"))
source(here::here("R", "fct_helpers.R"))

storymap_url <- "https://storymaps.arcgis.com/stories/da6688d548a44aea8171222b6d3ce5b7"
goals <- metrics %>%
  select(Goal, Goal_Description, Subgoal, Subgoal_Description, Name) %>%
  unique()
project_crs <- st_crs(subbasin_shps)

palfactor <-
  colorFactor(
    palette = c("blue", "red", "green"),
    levels = c("Public", "Private", "For-Profit")
  )
wria_pal <- colorFactor("GnBu", domain = NULL, reverse = TRUE)
basemap <- leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap", group = "Base") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "Grey") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addLayersControl(
    position = "bottomright", options = layersControlOptions(collapsed = FALSE),
    baseGroups = c("Base", "Satellite", "Grey"),
    overlayGroups = c("City Limits", "All Subbasins", "WRIA Outlines")
  ) %>%
  addPolygons(
    data = subbasin_shps, weight = 0.4, fillOpacity = 0.1,
    color = "#593D3B", fillColor = "#d2d6de",
    group = "All Subbasins", options = list(zIndex = 100)
  ) %>%
  addPolygons(
    data = cities_shp, group = "City Limits", dashArray = 5,
    color = "#002673", weight = 2, fill = FALSE,
    fillOpacity = 0.1,
    label = ~CITYNAME,
    highlightOptions = highlightOptions(
      weight = 6, color = "yellow", fill = TRUE, dashArray = 0)
  ) %>%
  addPolygons(
    data = wrias, group = "WRIA Outlines",
    options = list(zIndex = 200), color = "#2C8C99",
    weight = 0.8,
    label = ~WQBE_basin,
    highlightOptions = highlightOptions(weight = 3),
    # fillColor = ~wria_pal(WQBE_basin),
    opacity = 1, fillOpacity = 0
  ) %>%
  hideGroup("City Limits")
basemap

king_co_palette <- c("#FFE39F", "#B2CB9A", "#6FB084", "#3B925D", "#1D7324")

# https://coolors.co/palette/194663-fbb360-3fa6da-ec9a3c-111418

kingco_theme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#194663",
    yellow = "#FBB360", # F0B726",
    aqua = "#3FA6DA",
    # black = "#111418",
    orange = "#EC9A3C"
  ),
  fresh::adminlte_sidebar(
    #  width = "400px",
    dark_bg = "#E1E2E1",
    dark_hover_bg = "#184663",
    dark_color = "#2E3440"
  )
)

# set default otptions for datables
options(DT.options = list(
  scrollX = TRUE, digits = 2,
  dom = "tlprBi",
  buttons = c("copy", "csv", "excel", "pdf", "print")
))
