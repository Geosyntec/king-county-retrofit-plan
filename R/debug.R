debugUI <- function(id) {
  ns <- NS(id)
  tagList(
  dataTableOutput(ns('table'))
  )
}

debugServer <- function(id,rv) {
  moduleServer(
    id,
    function(input, output, session) {


      output$table <- renderDataTable(rv$data)

    }
  )
}

library(shiny)

ui <- fluidPage(
  debugUI("test")
)

server <- function(input, output, session) {
  rv <- reactiveValues(data = subbasin_data) #, basin_ids = subbasin_metrics %>% rownames())

  debugServer("test",rv)
}

shinyApp(ui, server)
