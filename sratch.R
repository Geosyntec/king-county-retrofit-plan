nameUI <- function(id) {
  ns <- NS(id)
  tagList(
  DTOutput(ns("table1"))
  )
}

nameServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$table1 <- renderDataTable(data)
    }
  )
}

ui <- fluidPage(
  shinydashboardPlus::dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = shinydashboard::dashboardBody(nameUI("test"))


  )
)

server <- function(input, output, session) {
  data <- reactive(subbasin_data)
  nameServer("test", subbasin_data)
}

shinyApp(ui, server)
