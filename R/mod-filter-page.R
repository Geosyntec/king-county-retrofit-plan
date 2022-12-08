
require(shinyBS)
require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinydashboardPlus)
require(DT)
require(dplyr)
require(spdplyr)
require(shinyjs)
require(reactable)
#
wria_choices <- subbasin_data$WQBE_basin %>% unique()

filter_page_UI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    # Column 1 ----------------------------------------------------------------
    fluidRow(
      column(
        width = 4,
        # Box to hold data filters
        div(
          id = ns("form"),
          shinydashboardPlus::box(
            height = "60rem", status = "primary", headerBorder = TRUE,
            title = "Pre-Screening Criteria",
            closable = FALSE,
            width = NULL,
            collapsible = FALSE,
            helpText("Below are pre-screening criteria you may select to limit
                     the number of subbasins in your analysis to focus only the areas or attributes of interest."),
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
            htmltools::strong("Subbasin Attributes:"),
            wellPanel(
              # fluidRow(
              # column(
              #  width = 6,
              awesomeCheckbox(
                inputId = ns("check1"),
                label = "Swimming Beaches",
                value = FALSE
              ),
              awesomeCheckbox(
                inputId = ns("check2"),
                label = "Phosphorus Sensitive Lakes",
                value = FALSE
                #  )
              ),
              # column(
              # width = 6,
              awesomeCheckbox(
                inputId = ns("check3"),
                label = "Headwaters",
                value = FALSE
              ),
              awesomeCheckbox(
                inputId = ns("check4"),
                label = "Shellfish Beaches",
                value = FALSE
                # )
              )
            )
            # )
          )
        ),
        shinydashboard::box(
          width = NULL,
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
              width = 7,
              shiny::actionButton(ns("reset"),
                label = "Reset Filters",
                class = "btn btn-secondary",
                style = "color:black",
                icon = icon("undo"), width = "90%"
              ),
              shiny::actionButton(ns("save"),
                label = "Save and Proceed",
                class = "btn btn-primary",
                style = "color:white", icon = icon("check"), width = "90%"
              )
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
        shinydashboardPlus::box(
          title = "Map",
          closable = FALSE,
          width = NULL,
          solidHeader = TRUE,
          collapsible = TRUE,
          shinycssloaders::withSpinner(
            leafletOutput(
              ns("map"),
              height = "60rem"
            )
          )

          # tabBox( # height = "60%",
          #  title = "",
          # closable = FALSE,
          # width = 12,
          # solidHeader = TRUE,
          # collapsible = TRUE,
          # tabPanel(
          # "Map",
        ) # ,#,
        #
        # )
        # )
      ),

      # fluidRow(shinydashboard::box("Debug", width = 12, textOutput(ns("react")))),
      fluidRow(
        column(
          width = 12,
          tabBox(
            title = NULL, width = NULL,
            tabPanel(
              title = "Data Table",
              shinycssloaders::withSpinner(
                DTOutput(outputId = ns("hot"))
              )
            ),
            tabPanel(
              width = NULL, title = "Data Check",
              verbatimTextOutput(ns("react"))
            )
          )
        )
      )
    )
  )
}

# Server ------------------------------------------------------------------

filter_page_server <- function(id, rv) {
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
      # cities_shp[cities_shp["CITYNAME"] %in% city_vals(), ]
      cities_shp[cities_shp[, "CITYNAME", drop = TRUE] %in% city_vals(), ] %>% st_set_crs(project_crs)
      # list of selected cities
      # cities_shp %>% dplyr::filter(CITYNAME %in% city_vals())
    })


    spatial_filter_ids <- reactive({
      if (!is.null(city_vals())) {
        # list of selected cities
        get_intersecting_ids(city_bounds(), subbasin_shps)
      } else {
        row.names(rv$base_data)
      }
    })


    # Reset observer ----------------------------------------------------------



    # observer for reset button
    observe({
      # city_vals() < NULL
      rv$filtered_data <- NULL
      reset("form")
    }) %>% bindEvent(input$reset)


    # # add jursidction if selected
    # observe({
    #
    #
    #   leafletProxy("map", data = city_bounds()) %>%
    #     clearShapes() %>%
    #   addPolygons()
    #
    # })

    table_filtered_ids <- reactive({
      req(rv$base_data)
      user_ids <- rv$base_data %>%
        dplyr::filter(WQBE_basin %in% wria_vals()) %>%
        dplyr::filter(Contains_Swimming_Beaches %in% swimming()) %>%
        dplyr::filter(Is_Headwater_Basin %in% headwaters()) %>%
        dplyr::filter(Presence_of_Shellfish %in% shellfish()) %>%
        dplyr::filter(Drains_to_P_Sensitive_Lake %in% P_lakes()) %>%
        rownames()
    })


    # if(is.null(user_ids)){
    #   c("1000010", "1000020", "1000030", "1000040", "1000050", "1000060")
    #
    # }
    # this joins spatial and table filters
    filtered_ids <- reactive({
      if (length(table_filtered_ids() > 0)) {
        return(table_filtered_ids()[table_filtered_ids() %in% spatial_filter_ids()])
      } else {
        return(table_filtered_ids())
      }
    })
    # observe(print(spatial_filter_ids()))
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


    # rv <- reactiveValues(filtered = NULL)

    data.df <- reactive({
      rv$base_data[row.names(rv$base_data) %in% filtered_ids(), ]
    })



    # observe(print(data.df.rv$filtered %>% reactiveValuesToList() %>% unlist()))
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
            WRIA = WQBE_basin,
            #  Imperviousness,
            `Shellfish Beaches` =  Presence_of_Shellfish,
            `Phosphorus Sensitive Lakes` =  Drains_to_P_Sensitive_Lake,
            Headwaters = Is_Headwater_Basin,
            `Swimming Beaches` = Contains_Swimming_Beaches
          )
        )
    })

    output$hot <- renderDT(
      datatable(#colnames = display_table()  %>% colnames() %>% get_pretty_names(),
      display_table(),
      ),
      rownames = TRUE, server = FALSE
    )



    # count rows
    output$num_selected <- shiny::renderUI(
      card(
        .num = nrow(data.df()), .description =
          HTML("Subbasins <br> Selected")
      )
    )



    output$map <- renderLeaflet({
basemap #%>%
        # addLayersControl(
        #   position = "bottomright", options = layersControlOptions(collapsed = FALSE),
        #   baseGroups = c("Base", "Satellite", "Grey"),
        #   overlayGroups = c("City Limits", "All Subbasins")
        # ) %>%
        # addPolygons(
        #   data = subbasin_shps, weight = 1, opacity = 0.6,
        #   color = "#9E9E9E", fillColor = "#d2d6de",
        #   group = "All Subbasins", options = list(zIndex = 100)
        # ) %>%
        # addPolygons(data = cities_shp, group = "City Limits") %>%
        # hideGroup("City Limits")
    })

    shps_filtered <- reactive({
      subbasin_shps[which(subbasin_shps$SWSID %in% filtered_ids()), ]
    })

    # map observers  -------------------------------------------------------
    #
    # proxy1
    observeEvent(filtered_ids(), {
      leafletProxy("map") %>%
        clearGroup("Selected Subbasins") %>%
        addPolygons(
          data = shps_filtered(), weight = 2.5, color = "#28a745",
          fillColor = "#01ff70",
          group = "Selected Subbasins", options = list(zIndex = 101)
        ) %>% clearControls() %>%
        addLayersControl(
          position = "bottomright", options = layersControlOptions(collapsed = FALSE),
          baseGroups = c("Base", "Satellite", "Grey"),
          overlayGroups = c("Selected Subbasins", "City Limits", "All Subbasins")
        )
    })

    observe({
      leafletProxy("map") %>%
        clearGroup("city_bounds") %>%
        addPolygons(data = city_bounds(), dashArray = c("2, 2"), fillOpacity = 0.1,
                    weight = 1.5, color = "black", group = "city_bounds", options = list(zIndex = 200))
    }) %>% bindEvent(city_bounds(), ignoreInit = TRUE, ignoreNULL = TRUE)

# Click events ------------------------------------------------------------

    # observeEvent(input$map_shape_click, {
    #
    #   #capture the info of the clicked polygon
    #   click <- input$map_shape_click
    #
    #   #subset your table with the id of the clicked polygon
    #   #selected <- filtered_ids[mydata$myID == click$id,]
    #
    #   #if click id isn't null render the table
    #   if(!is.null(click$id)){
    #    print(click$id)
    #     }
    #   })








    df.toreturn <- reactive(data.df()) %>% bindEvent(input$save) # module returns filtered ids
    shps.toreturn <- reactive(shps_filtered()) %>% bindEvent(input$save)
    # observe({
    #   rv$filtered_data <-  data.df()
    #   rv$filtered_shps <- shps_filtered()
    # }) %>% bindEvent(input$save)

    return(list(df.toreturn(), shps.toreturn()))
    # End mod server ----------------------------------------------------------
  })
}


library(shiny)

ui <- fluidPage(
  shinydashboardPlus::dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = shinydashboard::dashboardBody(filter_page_UI("test_filter"))
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(base_data = subbasin_data, filtered_data = NULL,
                       filtered_shps = NULL, top_basins = NULL, top_shps = NULL)

  # rv <- reactiveValues(base_data = subbasin_data,filtered_data = NULL)
 outvals <- reactive(filter_page_server("test_filter", rv))
}

shinyApp(ui, server)
