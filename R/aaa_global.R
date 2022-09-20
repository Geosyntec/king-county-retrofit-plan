require(shiny)
require(rhandsontable)
require(shinydashboard)
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


load(here::here("data", "subbasin_data.rda"))
load(here::here("data", "subbasin_shps.rda"))
load(here::here("data", "city_names.rda"))
load(here::here("data", "city_lookup.rda"))
load(here::here("data", "cities_shp.rda"))
load(here::here("data", "metrics.rda"))
source(here::here("R", "promethee.R"))
source(here::here("R", "normalize_pt.R"))
source(here::here("R", "fct_helpers.R"))

goals <- metrics %>% select(Goal, Goal_Description, Subgoal, Subgoal_Description, Name) %>% unique()
project_crs <- st_crs(subbasin_shps)

basemap <- leaflet() %>%
  addProviderTiles("CartoDB.Voyager", group = "Base") %>%
  # addProviderTiles("CartoDB.DarkMatter", group = "Base") %>%
  addProviderTiles("Esri.WorldGrayCanvas", group = "Grey") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite")


king_co_palette <- c("#FFE39F", "#B2CB9A", "#6FB084", "#3B925D", "#1D7324")

kingco_theme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#194663",
    yellow = "#FBB360", #F0B726",
    aqua = "#3FA6DA",
    #black = "#111418",
    orange = "#EC9A3C"
  ),
  fresh::adminlte_sidebar(
    #  width = "400px",
    dark_bg = "#E1E2E1",
    dark_hover_bg = "#184663",
    dark_color = "#2E3440"
  )
)

#set default otptions for datables
options(DT.options = list(
  scrollX = TRUE,digits=2,
  dom = "tlprBi",
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
))
