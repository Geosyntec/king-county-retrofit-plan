criteria_page_UI2 <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns('table'))
  )
}

criteria_page_server2 <- function(id,filtered) {
  moduleServer(
    id,
    function(input, output, session) {


      output$table <- renderPrint(filtered())

    }
  )
}
