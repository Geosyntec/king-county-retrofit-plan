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

load("~/Documents/repos/king-county-retrofit-plan/data/city_lookup.rda")
load("~/Documents/repos/king-county-retrofit-plan/data/subbasin_shps.rda")
load("~/Documents/repos/king-county-retrofit-plan/data/subbasin_metrics.rda")

sheds_df <- subbasin_metrics #%>% head(100)
subset_sheds <- subbasin_shps #%>% head(100)

ogd_basemap <- function() {leaflet() %>%
    addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
    #addProviderTiles("Esri.WorldGrayCanvas", group = "Gray Canvas") %>%
    addProviderTiles("Esri.WorldGrayCanvas", group = "Grey") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    #addTiles(streamTiles) %>%
    #addPolylines(data = ogd_streams,weight = 1,opacity = 1,color="#4374E0",group = "Streams") %>%
  addLayersControl(position = "bottomright",options = layersControlOptions(collapsed = FALSE),
                     baseGroups = c( "Grey", "Satellite","Dark"))
    #) #%>% hideGroup("Streams")
}


watershed_selection_UI <- function(id, accept_button = actionButton(inputId = "blnk", label = NULL)) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        wellPanel(style = "background: white",
                  h1("Watershed Selection"),
                  p("Choose which watersheds to include your watershed inventory")),


        # fluid row for boxes -----------------------------------------------------
        fluidRow(

          # Map Box -----------------------------------------------------------------


          shinydashboardPlus::box(
            collapsible = TRUE,
            closable = FALSE,
            width = 6,
            solidHeader = FALSE,
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
)
# footer = shinyWidgets::materialSwitch(
#   inputId = ns("ws_switch"),
#   label = "Clip to city bounds",
#   inline = TRUE
# )
          ),




# Table Box ---------------------------------------------------------------

# column(width = 6,

shinydashboardPlus::box(
  collapsible = TRUE,
  closable = FALSE,
  width = 6,
  footer_padding = TRUE,
  solidHeader = FALSE,
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
        )
      )
    )
  )
}

watershed_selection_server <- function(id, rv, ws_id = "SWSID",
                                       user_sheds = subbasin_shps,
                                       clipped_user_sheds =  subbasin_shps) {
  moduleServer(
    id,
    function(input, output, session) {
      output$city_nm <- renderPrint(rv$city)







      # Clip to bounds ----------------------------------------------------------



      # returns an sf object
      subset_sheds <- reactive({
        # if (input$ws_switch) {
        #   return(clipped_user_sheds)  #%>%
        #            #dplyr::filter(`City Name` == rv$city)) %>% unique()
        # } else {
          user_sheds# %>%
            #dplyr::filter(`City Name` == rv$city) %>%
            #unique()
        })
      #})

      #wser()

      sheds_df <- reactive(subset_sheds() %>%
                             sf::st_drop_geometry()
                             #as.data.frame() #%>%
                             #select(c("SWSID","City Name", "Watershed.area", "Acres.within.city.limits")))
)
      output$sheds_table <- renderDT(
        sheds_df(),
        server = TRUE,
        #extensions = c('Select', 'Buttons'),
        options = list(pageLength = 20,
                       #select = list(style = 'os', items = 'row'),
                       #buttons = c('selectAll', 'selectNone', 'selectRows'),
                       dom = 'tp',
                       rowId = 0
        )
      )


      selected_shedz <- reactive({
        if (length(input$sheds_table_rows_selected) > 0) {
          (subset_sheds()[c(unique(input$sheds_table_rows_selected)), ])
        } else {
          subset_sheds()
        }
      })

      observe({

        rv$shps <- selected_shedz()
        rv$data <- selected_shedz() %>% as.data.frame() %>% select(-geometry)
      })

      output$message <- renderText(length(input$my_datatable_rows_selected))

      output$Map <- renderLeaflet({
      #  sheds_bds <- sf::st_bbox(user_bounds())
        ogd_basemap() %>%
          #add watershed polygons
          addPolygons(group = "watersheds",
                      data = subset_sheds(),
                      opacity = 1,
                      color = "black",
                      weight = 0.5,
                      dashArray = 1,
                      fillOpacity = 0

          ) #%>% fitBounds(sheds_bds[[1]], sheds_bds[[2]], sheds_bds[[3]], sheds_bds[[4]]) #%>%

        #addProviderTiles(providers$CartoDB.Positron)

      })



      # })

      observe({

        #centers <- data.frame(st_centroid(selected_shedz(), of_largest_polygon = FALSE)
        #                      %>% st_coordinates())
        #centers$SWSID <- selected_shedz()[["SWSID"]]
        leafletProxy("Map", session) %>%
          clearGroup("watersheds") %>%
          #streams
          #addPolylines(data = ogd_streams,color = "#00B5C8",weight = 0.5,opacity = 1) %>%
          #watersheds
          addPolygons(group = "watersheds",
                      data = subset_sheds(),
                      opacity = 1,
                      color = "#03603d",
                      weight = 0.5,
                      fillOpacity = .3,
                      fillColor = "transparent"

          ) %>%
          #watershed fill
          addPolygons(weight = 0,group = "watersheds",
                      color = "green",
                      fillColor = "green",
                      #fillOpacity =.6,
                      data = selected_shedz()

          ) #%>%
          #city limits
          # addPolygons(data = user_bounds(),group = "city boundary",
          #             opacity = 1,
          #             fillColor = "transparent",
          #             weight = 1.5,color = "black",
          #             dashArray = "1,6,8,6") %>%
          #watershed labels
         # addLabelOnlyMarkers(group = "watersheds",
          #                    data = centers,
           #                   lng = ~X, lat = ~Y, label = ~SWSID,
            #                  labelOptions = labelOptions(noHide=TRUE,opacity = 0.6,direction = "top",
             #                                             style = list(
              #                                              "color" = "black",
                                                            #"box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    #                                        "font-size" = "12px",
                                                            #"border-color" = "rgba(0,0,0,0.5)",
                     #                                       "text-shadow"=" -1px 1px 0 #000,
                      #                     1px 1px 0 #000,
                       #                    1px -1px 0 #000;
                        #                   -1px -1px 0 #000;")
                         #     )
                              # labelOptions = labelOptions(
                              #   noHide = TRUE, direction = "top", textOnly = TRUE,
                              #   style = list("color:white;")
          #)




      }
      )

      #return reactive values here.


      #return(rv2)
    }
  )

}




#
ui <- (
  shinydashboardPlus::dashboardPage(
    header = shinydashboardPlus::dashboardHeader(),
    sidebar = shinydashboard::dashboardSidebar(),
    body = shinydashboard::dashboardBody(

      watershed_selection_UI("testing")
    )
  )
)
# )

server <- function(input, output, session) {
  rv <- reactiveValues()

  rv$city <- "SeaTac"
  watershed_selection_server("testing", rv) # returns a reactive value df

}

shinyApp(ui, server)
