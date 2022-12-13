debugUI <- function(id) {
  ns <- NS(id)
  tagList(
    # tableOutput(ns('OUT1')),
    shinydashboardPlus::box(
      width = 12,
      leafletOutput(ns("map"))
    ),
    shinydashboardPlus::box(
      width = 12, title = "shps Table", collapsible = TRUE,
      DTOutput(ns("filtered_shps"))
    ),
    shinydashboardPlus::box(
      width = 12, title = "Filtered Table", collapsible = TRUE,
      DTOutput(ns("filtered_data_table"))
    ),
  )
}

debugServer <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {
      output$map <- renderLeaflet(leaflet(rv$filtered_shps) %>% addPolygons())
      output$base_data_table <- renderDT(DT::datatable(rv$filtered_shps,
        extensions = "FixedColumns",
        options = list(
          dom = "t",
          scrollX = TRUE
        )
      ))
      output$filtered_data_table <- renderDT(DT::datatable(rv$filtered_data,
        extensions = "FixedColumns",
        options = list(
          dom = "t",
          scrollX = TRUE
        )
      ))
    }
  )
}
library(shiny)

ui <- fluidPage(
  # theme = bs_theme(bg = "black", fg = "white"),
  debugUI("test")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    base_data = subbasin_data %>% na.omit(),
    filtered_data = subbasin_data %>% na.omit() %>% head(),
    filtered_shps = subbasin_shps
  )
  debugServer("test", rv)
}
shinyApp(ui, server) # %>% run_with_themer()
