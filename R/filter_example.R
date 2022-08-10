library(shiny)
library(leaflet)
library(reactable)
library(crosstalk)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(shinyWidgets)
library(dplyr)
library(htmltools)
library(spdplyr)
library(shinyBS)

load(here::here("data", "subbasin_metrics.rda"))
load(here::here("data", "subbasin_shps.rda"))
load(here::here("data", "city_names.rda"))
load(here::here("data", "city_lookup.rda"))
load(here::here("data", "cities_shp.rda"))
source(here::here("R", "fct_helpers.R"))

subbasin_shps<- subbasin_shps %>% column_to_rownames('SWSID') %>% sf::st_as_sf()

wria_choices <- distinct(subbasin_metrics, WQBE_basin) %>% paste()
filter_page_UI <- function(id) {
  ns <- NS(id)

  tagList(# Column 1 ----------------------------------------------------------------
          fluidRow(
          column(
            width = 5,
            #box(width=12,
                uiOutput(ns("num_selected")),
            # Box to hold data filters
            shinydashboardPlus::box(height="60rem",
              title = "Filters",
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              collapsible = FALSE,

              # Filter items  -----------------------------------------------------------
              #WRIA Shared Data

              #WRIA
              pickerInput(
                selected = wria_choices,
                inputId = ns("wriapicker"),
                label = "WRIA",
                choices = distinct(subbasin_metrics, WQBE_basin),
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
              ) %>% tipify( title="This is an example of a tooltip", placement = "right", trigger = "hover",
                           options = NULL),

              #Jurisdiction
              pickerInput(
                inputId = ns("jurisdictionpicker"),
                label = "Jurisdiction",
                choices = city_names,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
              ),

              # Imperviousness
              # sliderInput(inputId = ns('impervfilter'),label = "Imperviousness",
              #             min = 0, max = 1, value = c(0,1),step = 0.05
              #             ),


              #Logicals
              #column(width = 12,
              wellPanel(
                htmltools::strong("Limit selection to:"),
                fluidRow(
                  column(
                    width = 6,
                    awesomeCheckbox(
                      inputId = ns("check1"),
                      label = "Swimming Beaches",
                      value = FALSE
                    ),
                    awesomeCheckbox(
                      inputId = ns("check2"),
                      label = "Phosphorus Sensitive Lakes",
                      value = FALSE
                    )
                  ),
                  column(
                    width = 6,
                    awesomeCheckbox(
                      inputId = ns("check3"),
                      label = "Headwaters",
                      value = FALSE
                    ),
                    awesomeCheckbox(
                      inputId = ns("check4"),
                      label = "Shellfish Beaches",
                      value = FALSE
                    )
                  )
                )
              )

              # checkboxGroupButtons(
              #   size = 'xs',direction = 'vertical',width= "90%",
              #
              #   inputId = ns("Id060"),
              #   label = "Limit selection to:",
              #   choiceNames =   c(
              #               "Swimming Beaches",
              #               "Phosphorus Sensitive Lakes",
              #               "Headwaters",
              #              "Shellfish Beaches"
              #               ),
              #   choiceValues = c(
              #     "Contains_Swimming_Beaches",
              #       "Drains_to_P_Sensitive_Lake",
              #       "Is_Headwater_Basin",
              #     "Presence_of_Shellfish"
              #   ),
              #   checkIcon = list(
              #     yes = tags$i(class = "fa fa-check-square",
              #                  style = "color: steelblue"),
              #     no = tags$i(class = "fa fa-square-o",
              #                 style = "color: steelblue"))
              # )
            )
          ),


          # Column 2 ----------------------------------------------------------------


          column(
            width = 7,
            # shinydashboardPlus::box(
            #   title = "Map",
            #   closable = TRUE,
            #   width = 12,
            #   solidHeader = TRUE,
            #   collapsible = TRUE,
            #   "Map"),

            tabBox(height = "60%",
              title = "",
              # closable = FALSE,
              width = 12,
              #solidHeader = TRUE,
              #collapsible = TRUE,
              tabPanel("Map", leafletOutput(ns("map"),height="60rem")),
              tabPanel('Debug',  verbatimTextOutput(ns("message")))

            ))),
            (column(width = 12,
              box(width = 12, "Table",
                       DTOutput(outputId = ns("hot"))),
            ))
          )


}

filter_page <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 wria_vals <- reactive(input$wriapicker)

                 city_vals <- reactive(input$jurisdictionpicker)
                 swimming <- reactive((if (input$check1) {
                   return(c(TRUE))
                 }
                 else{
                   return(c(TRUE, FALSE))
                 }))
                 P_lakes <- reactive((if (input$check2) {
                   return(c(TRUE))
                 }
                 else{
                   return(c(TRUE, FALSE))
                 }))
                 headwaters <- reactive((if (input$check3) {
                   return(c(TRUE))
                 }
                 else{
                   return(c(TRUE, FALSE))
                 }))
                 shellfish <- reactive((if (input$check4) {
                   return(c(TRUE))
                 }
                 else{
                   return(c(TRUE, FALSE))
                 }))

                 spatial_filter_ids <- reactive({
                   #list of selected cities
                   cities_shp %>% filter(CITYNAME %in% input$jurisdictionpicker) %>%
                     get_intersecting_polygons(subbasin_shps)

                 })



                 data.df <- reactive({
                   #if(!is.null(c(wria_vals(),city_vals(),imperv_vals(),checks()))) { #if sliders not null
                   #then filter
                   subbasin_metrics %>%
                     column_to_rownames("SWSID") %>%
                     #wria filter
                     dplyr::filter(WQBE_basin %in% wria_vals()) %>%

                     #jurisdiction filter
                     # dplyr::filter()
                     # }else{#%>%
                     #imperviousness filter
                     # dplyr::filter(between(Imperviousness,imperv_vals()[1],imperv_vals()[2])) %>%

                     #headwaters filter
                     filter(Contains_Swimming_Beaches %in% swimming()) %>%
                     filter(Is_Headwater_Basin %in% headwaters()) %>%
                     filter(Presence_of_Shellfish %in% shellfish()) %>%
                     filter(Drains_to_P_Sensitive_Lake %in% P_lakes())

                   #%>%
                   # dplyr::filter(SWSID %in% spatial_filter_ids() )
                   #else return the whole table
                   #subbasin_metrics}
                 }) %>%
                   bindEvent(c(
                     wria_vals(),
                     headwaters(),
                     shellfish(),
                     P_lakes(),
                     swimming()
                   ),
                   ignoreInit = FALSE)


                 # #filter reactive vals
                 #   observeEvent(input$wriapicker, {
                 #     output$message =   renderText(input$wriapicker)
                 #    filtered <- table_reactive$data.df %>%
                 #       dplyr::filter(WQBE_basin %in% input$wriapicker)
                 #   })
                 #
                 #   # table server

                 display_table <- reactive({
                   data.df() %>%
                     dplyr::select(
                       #only show a set of cols,
                       c(
                         #SWSID,
                         WQBE_basin,
                         Imperviousness,
                         Presence_of_Shellfish,
                         Drains_to_P_Sensitive_Lake,
                         Is_Headwater_Basin,
                         Contains_Swimming_Beaches
                       )
                     )
                 })
                 output$hot = renderDataTable(
                   DT::datatable(
                     display_table(),
                     rownames = TRUE,
                   #  style = "bootstrap5",
                     options = list(dom = 'tp', scrollX = TRUE),
                   #  extensions = 'Responsive'
                   ) %>% DT::formatPercentage("Imperviousness", 0)
                 )
                 #
                 #   output$table_1 = renderDataTable(subbasin_metrics)
                 #
                 # }

                 #count rows
                 output$num_selected <-
                   shiny::renderUI(
                     shinydashboardPlus::descriptionBlock(
                       text = "Subbasins Selected",
                       number = data.df() %>% nrow(),

                     )
                   )

                 #   shinydashboard::renderValueBox(valueBox(
                 #   subtitle = 'Subbasins Selected',
                 #   value = data.df() %>% nrow(),
                 #   color = "blue"
                 # ))

                 table_info = reactive(

                  # input$hot_rows_selected, {
                   {
                     if(length(input$hot_rows_selected)){
                   row.names(data.df())[c(input$hot_rows_selected)]
                     }else{
                       row.names(data.df())
                     }
                   })


                 output$message =   renderText(c(
                   # "headwaters",
                   # headwaters(),
                   # "swimming",
                   # swimming(),
                   # "P lakes",
                   # P_lakes(),
                   "rows selected",
                   table_info()
                 ))


                 output$map <-renderLeaflet({



                   # basins_selected = input$hot_rows_selected
                   leaflet() %>%
                       addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
                       addProviderTiles("Esri.WorldGrayCanvas", group = "Grey") %>%
                       addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
                       addLayersControl(position = "bottomright",options = layersControlOptions(collapsed = FALSE),
                                        baseGroups = c( "Grey", "Satellite","Dark")) %>%
                         setView(
                     lng = (-122.2),
                     lat = (47.6),
                     zoom = 7
                   )})

                 shps_selected <- reactive({

                   subbasin_shps[rownames(subbasin_shps) %in% table_info(), ]
                 })


# observer for map  -------------------------------------------------------

                 observe({


                   leafletProxy("map", data = shps_selected()) %>%
                     clearShapes() %>%
                     #add polygons

                   addPolygons(
                               opacity = 1,
                               color = "green",
                               weight = 0.5,
                               #dashArray = 1,
                               fillOpacity = 0.6,
                               fillColor = "green"

                   )



                 })
                 # End mod server ----------------------------------------------------------


               })
}


library(shiny)

ui <- fluidPage(
  shinydashboardPlus::dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = shinydashboard::dashboardBody(filter_page_UI("test"))


  )
)

server <- function(input, output, session) {
  filter_page('test')
}

shinyApp(ui, server)
