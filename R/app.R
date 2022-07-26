library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(dplyr)

load(here::here('data',"subbasin_metrics.rda"))

shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      box(title = 'Data Filters',solidHeader = TRUE, width = 6,
      #wqbe basins
      shinyWidgets::pickerInput(inputId = 'select-1',multiple =
                              TRUE,
                            label = "WRIA Watershed",
        choices = subbasin_metrics["WQBE_basin"] %>% unique() #%>% sort()
      ),
      #headwaters
      checkboxInput(inputId = 'checkbox-1',label = "Headwaters Only"
                    )
    #  drains to swimming beach
    )
    ),
    controlbar = dashboardControlbar(collapsed = FALSE,
                                     htmltools::strong('UI Color'),
                                     skinSelector()),
    title = "Skin Selector"
  ),
  server = function(input, output) {
    rv = reactiveValues()
    rv$data.df <- NULL



    }
)


