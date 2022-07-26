library(shiny)
library(leaflet)
library(reactable)
library(crosstalk)
library(shinydashboardPlus)
library(DT)
library(shinyWidgets)

load(here::here("data","subbasin_metrics.rda"))
load(here::here("data","city_names.rda"))

#specify the metrics to use for filtering
  metrics_to_filter <- c(
    "character" = list("WQBE_basin","Jurisdiction"),
    "range" = c("Imperviousness"),
    "logical" = c("Presence_of_Shellfish", "Drains_to_P_Sensitive_Lake",
                  "Is_Headwater_Basin", "Contains_Swimming_Beaches")

  )

  char_cols <- metrics_to_filter['character']

filter_page_UI <- function(id) {
  ns <- NS(id)

  tagList(

# Column 1 ----------------------------------------------------------------
column(width = 3,
# Box to hold data filters
 shinydashboardPlus::box(
    title = "Filters",
    closable = FALSE,
    width = 12,
    solidHeader = TRUE,
    collapsible = FALSE,

    # Filter items  -----------------------------------------------------------
    #WRIA Shared Data
    filter_select(id = ns('sd1'),label = "WRIA",

                  )
    #WRIA
    pickerInput(
      inputId = ns("wriapicker"),
      label = "WRIA",
      choices = distinct(subbasin_metrics,WQBE_basin),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE),
    ),

    #Jurisdiction
    pickerInput(
      inputId = ns("jurisdictionpicker"),
      label = "Jurisdiction",
      choices = city_names,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE),
    ),

    # Imperviousness
    sliderInput(inputId = ns('impervfilter'),label = "Imperviousness",
                min = 0, max = 100, value = c(0,100)
                ),
  #Logicals

  checkboxGroupButtons(
    inputId = ns("Id060"),
    label = "Label",
    choices = c("Option 1",
                "Option 2", "Option 3", "Option 4"),
  #   checkIcon = list(
  #     yes = tags$i(class = "fa fa-check-square",
  #                  style = "color: steelblue"),
  #     no = tags$i(class = "fa fa-square-o",
  #                 style = "color: steelblue"))
  # ),
),
  awesomeCheckbox(
    inputId = "shellfish-checkbox",
    label = "Contains Shellfish Beaches Only",
    value = FALSE
  ),
  awesomeCheckbox(
    inputId = "P-checkbox",
    label = "Drains to Phosphorus Sensitive Lake Only",
    value = FALSE
  ),
  awesomeCheckbox(
    inputId = "headwaters-checkbox",
    label = "Headwaters Only",
    value = FALSE
  ),
  awesomeCheckbox(
    inputId = "swimming-checkbox",
    label = "Contains Swimming Beaches Only",
    value = FALSE
  )

  )
),


# Column 2 ----------------------------------------------------------------


column(width = 9,
       shinydashboardPlus::box(
         title = "Map",
         closable = TRUE,
         width = 12,
         solidHeader = TRUE,
         collapsible = TRUE,
         "Map"),

  shinydashboardPlus::box(
  title = "Table",
  closable = FALSE,
  width = 12,
  solidHeader = TRUE,
  collapsible = TRUE,
  "Table Here",
  DTOutput(outputId = ns("table_1"))

),
shinydashboardPlus::box(
  title = "debug",
  closable = FALSE,
  width = 12,
  solidHeader = TRUE,
  collapsible = TRUE,
  textOutput(ns("message")),
  DTOutput(outputId = ns("hot"))


  )
)




)
}

filter_page <- function(id) {
moduleServer(id,
  function(input, output, session) {
  # shared Data
    sd <- SharedData$new(subbasin_metrics
    )

  # #filter reactive vals
  #   observeEvent(input$wriapicker, {
  #     output$message =   renderText(input$wriapicker)
  #    filtered <- table_reactive$data.df %>%
  #       dplyr::filter(WQBE_basin %in% input$wriapicker)
  #   })
  #
  #   # table server
  #   output$hot = renderDataTable(table_reactive$data.df)
  #
  #   output$table_1 = renderDataTable(subbasin_metrics)
  #
  # }
)
}


library(shiny)

ui <- fluidPage(
  shinydashboardPlus::dashboardPage(header = dashboardHeader(),sidebar = dashboardSidebar(),
                body = dashboardBody(

  filter_page_UI("test")
                )


  )
)

server <- function(input, output, session) {
  filter_page('test')
}

shinyApp(ui, server)
