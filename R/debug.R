
source(here::here('R','aaa_global.R'))

debugUI <- function(id) {
  ns <- NS(id)
  tagList(
  verbatimTextOutput(ns('table'))
  )
}

debugServer <- function(id,rv) {
  moduleServer(
    id,
    function(input, output, session) {
    filtered <- reactive(rv$base_data)

      output$table <- renderPrint(filtered$data)

    }
  )
}

library(shiny)

ui <- fluidPage(
  debugUI("test")
)

server <- function(input, output, session) {
  rv <- reactiveValues(base_data = subbasin_data) #, basin_ids = subbasin_metrics %>% rownames())

  debugServer("test",rv)
}

shinyApp(ui, server)
