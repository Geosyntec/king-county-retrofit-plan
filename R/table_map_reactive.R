library(htmltools)

# Help text  --------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(crosstalk)
library(sf)
library(DT)
library(dplyr)

load("~/Documents/repos/king-county-retrofit-plan/data/test_data.rda")

filterable_cols <- c(
  "Shellfish Count",
  "Swimming Tributary Percentage",
  "Urban Growth Boundary Percentage",
  "PercentImpervious",
  "Is Drainage to P Sensitive Lake"
)

df <- test_data %>% select(all_of(filterable_cols))

filter_locations_UI <- function(
    id, accept_button = actionButton(inputId = "blnk", label = NULL)) {
  ns <- NS(id)

  tagList(

    fluidRow(

      column(
        width = 12,
      #  wellPanel(style = "background: white",
       # h1("Watershed Selection"),
        #p("Choose which watersheds to include your watershed inventory")),
        # Data Filters -----------------------------------------------------
        column(width = 3,
               shinydashboardPlus::box(width=12,title = "Filter Data",
                                       collapsible = FALSE,
                                       closable = FALSE,

                                       solidHeader = TRUE,
                                       status = "primary",



               shinyWidgets::pickerInput(
                 inputId = "Id083",
                 label = "Jurisdiction",
                 choices = c('A',"B","C"),
                 multiple = TRUE
               ),
               shinyWidgets::pickerInput(
                 inputId = "Id083",
                 label = "WRIA Watershed",
                 choices = c('A',"B","C"),
                 multiple = TRUE
               ),
              shiny::sliderInput(inputId = "Id01",
                                 label = 'Percent Impervious',
                                 value=c(0,100),min = 0,max = 100,
                                 step = 5,post = "%"),
              shiny::sliderInput(inputId = "Id01",
                                 label = 'Percent Shellfish Areas',
                                 value=c(0,100),min = 0,max = 100,
                                 step = 5,post = "%"),
              shiny::sliderInput(inputId = "Id01",
                                 label = 'Percent within Urban Growth Boundary',
                                 value=c(0,100),min = 0,max = 100,
                                 step = 5,post = "%"),
              ##Check boxes
               shinyWidgets::awesomeCheckbox(
                 inputId = "Id043",
                 label = "Headwaters Only",
                 value = FALSE
               ),
               shinyWidgets::awesomeCheckbox(
                 inputId = "Id044",
                 label = "Drains to Phosphorus Sensitive Lakes Only",
                 value = FALSE
               ),
               shinyWidgets::awesomeCheckbox(
                 inputId = "Id045",
                 label = "Contains Swimming Beaches Only",
                 value = FALSE
               )

               )),
        column(width = 9,
        # fluid row for boxes -----------------------------------------------------
        fluidRow(

          # Map Box -----------------------------------------------------------------


          shinydashboardPlus::box(
            collapsible = TRUE,
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            title = "Map",
            sidebar = boxSidebar(
              id = "mycardsidebar",
              icon = shiny::icon("info-circle"),
              p("NPDES Phase II Permit Section: S5.C.1.d.i

Purpose: Assess existing information related to local receiving waters and contributing area conditions to identify which receiving waters are most likely to benefit from stormwater management planning.

Important: Use this step to understand the possible benefit of stormwater management planning. This analysis is critical to the development of the watershed inventory deliverable dues March 31, 2022

")
            ),
            # sidebar_icon = "info-circle",
            column(
              width = 12,
              helpText("Watershed boundaries shown below are from Ecology's
                   Watershed Characterization Asessement Units dataset."),
              leafletOutput(
                ns("Map"),
                height = 600
              )
            ),
            footer = shinyWidgets::materialSwitch(
              inputId = ns("ws_switch"),
              label = "Clip to city bounds",
              inline = TRUE
            )
          ),




),

          # Table Box ---------------------------------------------------------------
fluidRow(
          # column(width = 6,

          shinydashboardPlus::box(
            collapsible = TRUE,
            closable = FALSE,
            width = 12,
            footer_padding = TRUE,
            solidHeader = TRUE,
            status = "primary",
            title = "Watershed Selection Table", sidebar_title = "Permit Guidance",
            # enable_sidebar = TRUE,
            sidebar = shinydashboardPlus::boxSidebar(
              id = "mycardsidebar2",
              icon = shiny::icon("info-circle"),
              p("NPDES Phase II Permit Section: S5.C.1.d.i

Purpose: Assess existing information related to local receiving waters and contributing area conditions to identify which receiving waters are most likely to benefit from stormwater management planning.

Important: Use this step to understand the possible benefit of stormwater management planning. This analysis is critical to the development of the watershed inventory deliverable dues March 31, 2022

")
            ),
            column(
              width = 12,
              helpText("Watershed names have been prepopulated with unique IDs.
                 Where possible, names were derived from spatial databases.
                 Double-click on a Watershed Name to rename"),
              # shinyWidgets::panel(
              h5("Select watersheds to include in inventory from the table below"),
              # mod_dt_edit_ui(ns("editable")),
              DT::DTOutput(ns("sheds_table")),
              shiny::actionButton(ns("Reset"), label = "Reset"),
              accept_button,
              br()

              #verbatimTextOutput(ns("render_selected_line"))
            ) # )
          )
        ),
fluidRow(

  shinydashboardPlus::flipBox(id = 'flipbox',
                              front = h2('Debug Info...'),
                              back = verbatimTextOutput('debug')

                              )
)
      )
    )
  )
)
}

filter_locations_server <- function(id, rv, ws_id = "P2_SWSI") {
  moduleServer(
    id,
    function(input, output, session) {


      sheds_df <- reactive(rv$data
         )#%>%
        #select(c("watershed.name","City Name", "Watershed.area", "Acres.within.city.limits")))

      output$sheds_table <- renderDT(
        sheds_df(),
        #extensions = c('Select', 'Buttons'),
        options = list(pageLength = 20,
          #select = list(style = 'os', items = 'row'),
          #buttons = c('selectAll', 'selectNone', 'selectRows'),
          dom = 'tp',
          rowId = 0
        )
      )

      output$debug = renderText(
        paste(
        "Basins included",
        sheds_df %>% rownames())
        )






})}




#
ui <- (
  shinydashboardPlus::dashboardPage(
    header = shinydashboardPlus::dashboardHeader(),
    sidebar = shinydashboard::dashboardSidebar(),
    body = shinydashboard::dashboardBody(

      filter_locations_UI("testing")
    )
  )
)
# )

server <- function(input, output, session) {
  rv <- reactiveValues()

  #rv$city <- "SeaTac"
  filter_locations_server("testing", rv) # returns a reactive value df

}

shinyApp(ui, server)
