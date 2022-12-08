library(shiny)
reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
          id = ns("form"),
          shinydashboardPlus::box(solidHeader = TRUE,status = 'primary', width = NULL,title = "Watershed Report",
  box(title = 'Inputs',width = NULL,status = 'primary',
shiny::wellPanel(
  ("Selected WRIAs:"),
  code(textOutput(ns('wrias'))),

  ("Selected Jurisdictions:"),
  code(textOutput(ns('jurisdictions'))),

  ("Subbasin Attributes:"),
  code(textOutput(ns('attributes'))),
),
  hr(),
  p('Filters')),
  br(),
  p('Results'),
  hr()
  ))))
}

reportServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
#Inputs
      output$wrias = renderText('wrias')
      output$jurisdictions = renderText('jurisdictions')
      output$attributes =
        renderText('attributes')
    }
  )
}



ui <- fluidPage(
  shinydashboardPlus::dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = shinydashboard::dashboardBody(
  reportUI('report_test'))
))

server <- function(input, output, session) {
  reportServer('report_test')
}

shinyApp(ui, server)
