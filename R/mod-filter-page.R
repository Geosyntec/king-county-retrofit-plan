
require(shinyBS)
require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinydashboardPlus)
require(DT)
require(dplyr)
require(spdplyr)
# load(here::here("data", "subbasin_data.rda"))
# load(here::here("data", "subbasin_shps.rda"))
# load(here::here("data", "city_names.rda"))
# load(here::here("data", "city_lookup.rda"))
# load(here::here("data", "cities_shp.rda"))
# source(here::here("R", "fct_helpers.R"))



wria_choices <- subbasin_data$WQBE_basin %>% unique()
# distinct(subbasin_data, WQBE_basin) #%>% paste()

filter_page_UI <- function(id) {
  ns <- NS(id)

  tagList(

    # Column 1 ----------------------------------------------------------------
    fluidRow(
      column(
        width = 4,
        # Box to hold data filters
        shinydashboardPlus::box(
          height = "60rem", status = "primary", headerBorder = TRUE,
          title = "Filters",
          closable = FALSE,
          width = 12,
          collapsible = FALSE,

          # Filter items  -----------------------------------------------------------
          # WRIA Shared Data

          # WRIA
          fluidRow(
            column(
              width = 6,
              pickerInput(
                selected = wria_choices,
                inputId = ns("wriapicker"),
                label = "WRIA",
                choices = wria_choices,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
              ) %>% tipify(
                title = "This is an example of a tooltip", placement = "right", trigger = "hover",
                options = NULL
              )
            ),

            # Jurisdiction
            column(
              width = 6,
              pickerInput(
                inputId = ns("jurisdictionpicker"),
                label = "Jurisdiction",
                choices = city_names,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
              )
            )
          ),
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
        ),
        shinydashboard::box(
          width = 12,
          # fixedRow
          fluidRow(
            column(
              width = 5,
              # column(width = 6,
              # wellPanel(
              uiOutput(ns("num_selected"))
            )
            # )

            #    )
            ,
            (column(
              width = 7, shiny::actionButton(ns("reset"), label = "Reset Filters", icon = icon("undo"), width = "90%"),
              shiny::actionButton(ns("save"), label = "Save Subbasins", icon = icon("check"), width = "90%")
            )

            )
          )
        ), # end box
        #                               column(width = 6,
        #
        #           column(width = 6,
        #
        # )
      ),


      # Column 2 ----------------------------------------------------------------


      column(
        width = 8,
        # shinydashboardPlus::box(
        #   title = "Map",
        #   closable = TRUE,
        #   width = 12,
        #   solidHeader = TRUE,
        #   collapsible = TRUE,
        #   "Map"),

        tabBox( # height = "60%",
          title = "",
          # closable = FALSE,
          width = 12,
          # solidHeader = TRUE,
          # collapsible = TRUE,
          tabPanel(
            "Map",
            shinycssloaders::withSpinner(
              leafletOutput(
                ns("map"),
                height = "60rem"
              )
            )
          ),
          tabPanel("Debug", verbatimTextOutput(ns("message")))
        )
      )
    ),
    (column(
      width = 12,
      box(
        width = 12, title = "Data Table", status = "primary", headerBorder = TRUE,
        shinycssloaders::withSpinner(
          DTOutput(outputId = ns("hot"))
        )
      ),
    ))
  )
}

# Server ------------------------------------------------------------------

filter_page_server <- function(id, watershed.data) {
  moduleServer(id, function(input, output, session) {
    # assign reactive values to module variables



    wria_vals <- reactive(input$wriapicker)

    city_vals <- reactive(input$jurisdictionpicker)

    swimming <- reactive((if (input$check1) {
      return(c(TRUE))
    } else {
      return(c(TRUE, FALSE))
    }))
    P_lakes <- reactive((if (input$check2) {
      return(c(TRUE))
    } else {
      return(c(TRUE, FALSE))
    }))
    headwaters <- reactive((if (input$check3) {
      return(c(TRUE))
    } else {
      return(c(TRUE, FALSE))
    }))
    shellfish <- reactive((if (input$check4) {
      return(c(TRUE))
    } else {
      return(c(TRUE, FALSE))
    }))

    city_bounds <- reactive({

      # list of selected cities
      cities_shp %>% dplyr::filter(CITYNAME %in% city_vals())
    })


    spatial_filter_ids <- reactive({
      if (!is.null(city_vals())) {
        # list of selected cities
        get_intersecting_ids(city_bounds(), subbasin_shps)
      } else {
        row.names(watershed.data)
      }
    })


    # add jursidction if selected
    observe({


      leafletProxy("map", data = city_bounds()) %>%
        clearShapes() %>%
      addPolygons()

    })

    filtered_ids <- reactive({
      user_ids <- watershed.data %>%
        dplyr::filter(WQBE_basin %in% wria_vals()) %>%
        dplyr::filter(Contains_Swimming_Beaches %in% swimming()) %>%
        dplyr::filter(Is_Headwater_Basin %in% headwaters()) %>%
        dplyr::filter(Presence_of_Shellfish %in% shellfish()) %>%
        dplyr::filter(Drains_to_P_Sensitive_Lake %in% P_lakes()) %>%
        rownames()
      # if(is.null(user_ids)){
      #   c("1000010", "1000020", "1000030", "1000040", "1000050", "1000060")
      #
      # }
      if (length(user_ids > 0)) {
        return(user_ids[user_ids %in% spatial_filter_ids()])
      } else {
        return(user_ids)
      }
    })
    #observe(print(spatial_filter_ids()))
    # %>%
    #   bindEvent(c(
    #     wria_vals(),
    #     headwaters(),
    #     shellfish(),
    #     P_lakes(),
    #     swimming(),
    #     city_bounds()
    #   ),
    #   ignoreInit = FALSE)



    data.df <- reactive({
      watershed.data[row.names(watershed.data) %in% filtered_ids(), ]
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
          # only show a set of cols,
          c(
            # SWSID,
            WQBE_basin,
            Imperviousness,
            Presence_of_Shellfish,
            Drains_to_P_Sensitive_Lake,
            Is_Headwater_Basin,
            Contains_Swimming_Beaches
          )
        )
    })
    output$hot = renderDT(
      display_table(),rownames = TRUE,
      #  style = "bootstrap5"options = list(dom = 'tp', scrollX = TRUE),
      #  extensions = 'Responsive'
       #DT::formatPercentage("Imperviousness", 0)
    )


      #output$table_1 = renderDataTable(subbasin_metrics())

    #}

    # count rows
    output$num_selected <- shiny::renderUI(
      card(
        .num = nrow(data.df()), .description =
          HTML("Subbasins <br> Selected")
      )
    )



    #   shinydashboard::renderValueBox(valueBox(
    #   subtitle = 'Subbasins Selected',
    #   value = data.df() %>% nrow(),
    #   color = "blue"
    # ))

    table_info <- reactive(

      # input$hot_rows_selected, {
      {
        if (length(input$hot_rows_selected)) {
          row.names(data.df())[c(input$hot_rows_selected)]
        } else {
          row.names(data.df())
        }
      }
    )




    output$map <- renderLeaflet({



      # basins_selected = input$hot_rows_selected
      leaflet() %>%
        addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
        addProviderTiles("Esri.WorldGrayCanvas", group = "Grey") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addLayersControl(
          position = "bottomright", options = layersControlOptions(collapsed = FALSE),
          baseGroups = c("Grey", "Satellite", "Dark")
        ) %>%
        addPolygons(data = subbasin_shps, group = "selected_sheds") # %>%

      #       setView(
      #   lng = (-122.2),
      #   lat = (47.6),
      #   zoom = 7
      # )
    })

    shps_selected <- reactive({
       req(filtered_ids())
      subbasin_shps[which(subbasin_shps$SWSID %in% filtered_ids()),]
    })

    # map observers  -------------------------------------------------------
    #

    observeEvent(shps_selected(), {
      leafletProxy("map") %>%
        clearGroup("selected_sheds") %>%
        addPolygons(
          data = shps_selected(), group = "selected_sheds",
          opacity = 0.6,
          color = "green",
          weight = 0.5,
          # dashArray = 1,
          fillOpacity = 0.1,
          fillColor = "green"
        )
    })

    # add city if selected
    observe({
      if (length(city_vals()) != 0) {
        leafletProxy("map") %>%
          clearGroup("City") %>%
          addPolygons(
            data = city_bounds(), group = "City",
            color = "#927EAB",
            opacity = 1,
            weight = 3,
            fillOpacity = 0.25
          )
      } else {
        leafletProxy("map") %>%
          clearGroup("City")
      }
    }) # %>% bindEvent(input$jurisdictionpicker)


    # observe({
    #   if(length(city_vals())==0){
    #   print(length(city_vals()))
    #   leafletProxy("map") %>%
    #     clearGroup('City')
    # }})%>% bindEvent(input$jurisdictionpicker)

    # observe table select
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


    # observe map click
    observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
      p <- input$map_shape_click
      pt.df <- data.frame(x = p["lat"], y = p["lng"])
      #print(pt.df)
    }) # %>% bindEvent(input$map)
    # debug -------------------------------------------------------------------


    output$message <- renderText(c(
      # "headwaters",
      # headwaters(),
      # "swimming",
      # swimming(),
      # "P lakes",
      # P_lakes(),
      "filtered ids:",
      filtered_ids(),
      city_vals() %>% unlist(),
      "merge:",
      length(city_vals())))

      # spatial_filter_ids() %>% unlist(),
      # "cities:"
      #  city_bounds() %>% unlist()
      # "rows selected",
      # table_info()
    df.toreturn <- reactive(data.df()) %>% bindEvent(input$save) #module returns filtered ids
    return(df.toreturn)
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
  filter_page_server("test", subbasin_data)
}

shinyApp(ui, server)
