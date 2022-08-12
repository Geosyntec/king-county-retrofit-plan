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

subbasin_shps<- subbasin_shps #%>% #tibble::column_to_rownames('SWSID') %>%
  #sf::st_as_sf()

wria_choices <- subbasin_metrics$WQBE_basin %>% unique()
  distinct(subbasin_metrics, WQBE_basin) #%>% paste()
filter_page_UI <- function(id) {
  ns <- NS(id)

  tagList(# Column 1 ----------------------------------------------------------------
          fluidRow(
          column(
            width = 5,
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
              fluidRow(
              column(width = 6,
              pickerInput(selected = wria_choices,
                inputId = ns("wriapicker"),
                label = "WRIA",
                choices = wria_choices,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
              ) %>% tipify( title="This is an example of a tooltip", placement = "right", trigger = "hover",
                           options = NULL)),

              #Jurisdiction
              column(width = 6,
                pickerInput(
                inputId = ns("jurisdictionpicker"),
                label = "Jurisdiction",
                choices = city_names,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
              ))),

                htmltools::strong("Limit selection to:"),
              wellPanel(
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
              tabPanel("Map",
                       shinycssloaders::withSpinner(

                       leafletOutput(
                         ns("map"),
                         height="60rem")
                       )
                       ),
              tabPanel('Debug',  verbatimTextOutput(ns("message")))

            ))),
            (column(width = 12,
              box(width = 12, "Table",
                  shinycssloaders::withSpinner(
                       DTOutput(outputId = ns("hot")))),
            ))
          )


}

# Server ------------------------------------------------------------------

filter_page <- function(id, metrics) {
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

                 city_bounds <- reactive({

                   #list of selected cities
                   cities_shp %>% filter(CITYNAME %in% city_vals())
                 })


                 spatial_filter_ids <- reactive({
                   if(!is.null(city_vals())){
                   #list of selected cities
                   get_intersecting_ids(city_bounds(),subbasin_shps)
                   }else{
                     row.names(metrics)
                   }
                 })


#add jursidction if selected
                 # observe({
                 #
                 #
                 #   leafletProxy("map", data = city_bounds()) %>%
                 #     clearShapes() %>%
                 #   addPolygons()
                 #
                 # })

                filtered_ids <- reactive({
                  user_ids <- metrics %>%
                    dplyr::filter(WQBE_basin %in% wria_vals()) %>%
                    filter(Contains_Swimming_Beaches %in% swimming()) %>%
                    filter(Is_Headwater_Basin %in% headwaters()) %>%
                    filter(Presence_of_Shellfish %in% shellfish()) %>%
                    filter(Drains_to_P_Sensitive_Lake %in% P_lakes()) %>%
                    rownames()

                  if(length(user_ids >0)){
                    return(user_ids[user_ids %in% spatial_filter_ids()])
                  }else{
                    return(user_ids)
                  }


                }) %>%
                  bindEvent(c(
                    wria_vals(),
                    headwaters(),
                    shellfish(),
                    P_lakes(),
                    swimming(),
                    city_bounds()
                  ),
                  ignoreInit = FALSE)



                 data.df <- reactive({

                   metrics[row.names(metrics) %in% filtered_ids(), ]
                 })


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
                       number = nrow(data.df()),

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




                 output$map <-renderLeaflet({



                   # basins_selected = input$hot_rows_selected
                   leaflet() %>%
                       addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
                       addProviderTiles("Esri.WorldGrayCanvas", group = "Grey") %>%
                       addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
                       addLayersControl(position = "bottomright",options = layersControlOptions(collapsed = FALSE),
                                        baseGroups = c( "Grey", "Satellite","Dark")) %>%
                     addPolygons(data = subbasin_shps, group = 'selected_sheds') %>%

                         setView(
                     lng = (-122.2),
                     lat = (47.6),
                     zoom = 7
                   )
                   })

                 shps_selected <- reactive({

                   subbasin_shps %>% filter(SWSID %in% filtered_ids())
                 })


# map observers  -------------------------------------------------------
#

                 observeEvent(shps_selected(),{ leafletProxy("map") %>%
                     clearGroup('selected_sheds') %>%
                     addPolygons(data = shps_selected(), group = "selected_sheds",

                                 opacity = 0.6,
                                 color = "green",
                                 weight = 0.5,
                                 #dashArray = 1,
                                 fillOpacity = 0.1,
                                 fillColor = "green")
                   })

                 #add city if selected
                 observe({
                   if(length(city_vals())!=0){



                   leafletProxy("map") %>%
                     clearGroup('City') %>%
                       addPolygons(data = city_bounds(), group = "City",
                                   color='#927EAB',
                                   opacity = 1,
                                   weight = 3,

                                   fillOpacity = 0.25
                                   )

                   } else{

                   leafletProxy("map") %>%
                     clearGroup('City')
                 }})# %>% bindEvent(input$jurisdictionpicker)


                 # observe({
                 #   if(length(city_vals())==0){
                 #   print(length(city_vals()))
                 #   leafletProxy("map") %>%
                 #     clearGroup('City')
                 # }})%>% bindEvent(input$jurisdictionpicker)

                 #observe table select
                 # observe({
                 #   leafletProxy("map", data = shps_selected()) %>%
                 #     clearGroup('subbasins') %>%
                 #     #add polygons
                 #
                 #   addPolygons(layerId =  'subbasins',
                 #               opacity = 0.6,
                 #               color = "green",
                 #               weight = 0.5,
                 #               #dashArray = 1,
                 #               fillOpacity = 0.1,
                 #               fillColor = "green"
                 #
                 #   )
                 # })


                   #observe map click
                   observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
                     p <- input$map_shape_click
                     pt.df <- data.frame(x= p['lat'],y=p['lng'])
                     print(pt.df)
                   }) #%>% bindEvent(input$map)
# debug -------------------------------------------------------------------


                   output$message =   renderText(c(
                     # "headwaters",
                     # headwaters(),
                     # "swimming",
                     # swimming(),
                     # "P lakes",
                     # P_lakes(),
                     "citybounds:",
                     city_vals() %>% unlist(),
                     "merge:",
                     length(city_vals())

                     #spatial_filter_ids() %>% unlist(),
                     #"cities:"
                     #  city_bounds() %>% unlist()
                     #"rows selected",
                     #table_info()
                   ))


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
  filter_page('test',subbasin_metrics)
}

shinyApp(ui, server)
