
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
    useShinyjs(),

    # Column 1 ----------------------------------------------------------------
    fluidRow(
      column(
        width = 4,
        # Box to hold data filters
        div(id = ns("form"),
        shinydashboardPlus::box(
          height = "60rem", status = "primary", headerBorder = TRUE,
          title = "Filters",
          closable = FALSE,
          width = NULL,
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
            #fluidRow(
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
              #column(
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
                #)
              )
            )
          #)
        )),
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
                    label = "Save Subbasins",
                    class = "btn btn-primary",
                    style = "color:white", icon = icon("check"), width = "90%")
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
          #tabPanel(
           # "Map",

          )#,#,
         # box("Debug", verbatimTextOutput(ns("message")))
        #)
      #)
    ),
    fluidRow(
      column(
      width = 12,
      tabBox(title = NULL,width = NULL,
             tabPanel(title = "Data Table",

        shinycssloaders::withSpinner(
          reactableOutput(outputId = ns("hot")))),
          tabPanel(
            width = NULL, title = "Data Check",
            verbatimTextOutput(ns('react'))
            )
        )
      )
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
        row.names(watershed.data)
      }
    })


# Reset observer ----------------------------------------------------------



    #observer for reset button
    observe({
      #city_vals() < NULL
      data.df() = NULL
      reset("form")
    }
    ) %>% bindEvent(input$reset)


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
      user_ids <- watershed.data %>%
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


    rv <- reactiveValues(filtered = NULL)

    data.df <- reactive({
      watershed.data[row.names(watershed.data) %in% filtered_ids(), ]
    })

    observe({rv$filtered =  watershed.data[row.names(watershed.data) %in% filtered_ids(), ]})

    #observe(print(data.df.rv$filtered %>% reactiveValuesToList() %>% unlist()))
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
          #  Imperviousness,
            Presence_of_Shellfish,
            Drains_to_P_Sensitive_Lake,
            Is_Headwater_Basin,
            Contains_Swimming_Beaches
          )
        )
    })
    output$hot <-
      renderReactable({

      make_tf_icons <- function(value){
        ifelse(value, tagList(shiny::icon('check-circle',style = "color:#00A65A")),
               tagList(shiny::icon('minus-circle',class = "regular", style = "color:#dd4b39"),value))}

        reactable(
          display_table(),
          defaultColDef = colDef(align = 'center'),
          columns = list(
            Presence_of_Shellfish = colDef(name = 'Presence of Shellfish', cell = function(value) {make_tf_icons(value)}),
            Drains_to_P_Sensitive_Lake = colDef(name = 'Drains to Phosphorus Sensitive Lake', cell = function(value) {make_tf_icons(value)}),
            Is_Headwater_Basin = colDef(name = 'Headwater Basin', cell = function(value) {make_tf_icons(value)}),
            Contains_Swimming_Beaches = colDef(name = 'Contains Swimming Beaches', cell = function(value) {make_tf_icons(value)})
            #   ifelse(value, tagList(shiny::icon('check-circle',style = "color:#00A65A")),
            #          tagList(shiny::icon('minus-circle',class = "regular", style = "color:#EB8D80")))
            #   }



          )
        )
})



    #   renderDT(
    #   display_table(),
    #   rownames = TRUE,
    #   colnames <- c('WRIA','Presence of Shellfish', 'Drains to Phosphorus Sensitive Lake',
    #                 'Headwater Basin','Contains Swimming Beaches'
    #   )
    #   #  style = "bootstrap5"options = list(dom = 'tp', scrollX = TRUE),
    #   #  extensions = 'Responsive'
    #   # DT::formatPercentage("Imperviousness", 0)
    # )


    # output$table_1 = renderDataTable(subbasin_metrics())

    # }

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
      leaflet(subbasin_shps) %>%
        addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
        addProviderTiles("Esri.WorldGrayCanvas", group = "Grey") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addLayersControl(
          position = "bottomright", options = layersControlOptions(collapsed = FALSE),
          baseGroups = c("Grey", "Satellite", "Dark"),
          overlayGroups = c("City Limits", "base")
        ) %>%
        addPolygons(
          data = subbasin_shps, weight = 1, opacity = 0.6,
          color = "#9E9E9E", fillColor = "#d2d6de",
          group = "base", options = list(zIndex = 100)
        ) %>%
        addPolygons(data = cities_shp, group = "City Limits") %>%
        hideGroup("City Limits")
    })

    shps_filtered <- reactive({
      # req(filtered_ids())
      subbasin_shps[which(subbasin_shps$SWSID %in% filtered_ids()), ]
    })

    # map observers  -------------------------------------------------------
    #
    # proxy1
    observeEvent(display_table(), {
      leafletProxy("map") %>%
        clearGroup("selected_sheds") %>%
        # clearGroup("base") %>%
        # addPolygons(data = subbasin_shps, fillOpacity = 0.4,weight = 1,color = "#6c757d",
        #             fillColor = "grey",group = "base",  options = list(zIndex = 100)) %>%
        addPolygons(
          data = shps_filtered(), weight = 2.5, color = "#28a745",
          fillColor = "#01ff70",
          group = "selected_sheds", options = list(zIndex = 101)
        ) # %>%
    })

    observe({
      leafletProxy("map") %>%
        clearGroup("city_bounds") %>%
        addPolygons(data = city_bounds(), dashArray = c("2, 2"), fillOpacity = 0.1, weight = 1.5, color = "black", group = "city_bounds", options = list(zIndex = 200))
    }) %>% bindEvent(city_bounds(), ignoreInit = TRUE, ignoreNULL = TRUE)

    # proxy2
    # observeEvent(display_table(), {
    #   if(length(city_vals()) != 0) {
    #     leafletProxy("map") %>%
    #       showGroup("selected_sheds") %>%
    #       showGroup("base") %>%
    #     clearGroup("City") %>%
    #       addPolygons(
    #         data = city_bounds(), group = "City",
    #         color = "#927EAB",
    #         opacity = 1,
    #         weight = 3,
    #         fillOpacity = 0.25
    #       )
    #   } else {
    #     leafletProxy("map") %>%
    #     clearGroup("City")
    #
    # }})

    # proxy 3
    # add city if selected
    # observe({
    #   if (length(city_vals()) != 0) {
    #     leafletProxy("map") %>%
    #       clearGroup("City") %>%
    #       addPolygons(
    #         data = city_bounds(), group = "City",
    #         color = "#927EAB",
    #         opacity = 1,
    #         weight = 3,
    #         fillOpacity = 0.25
    #       )
    #   } else {
    #     leafletProxy("map") %>%
    #       clearGroup("City")
    #   }
    # }) %>% bindEvent(input$jurisdictionpicker)


    # observe({
    #   if(length(city_vals())==0){
    #   print(length(city_vals()))
    #   leafletProxy("map") %>%
    #     clearGroup('City')
    # }})%>% bindEvent(input$jurisdictionpicker)

    # observe table select
    # observe({
    #   leafletProxy("map", data = shps_filtered()) %>%
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


    # # observe map click
    # observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    #   p <- input$map_shape_click
    #   pt.df <- data.frame(x = p["lat"], y = p["lng"])
    #   #print(pt.df)
    # }) # %>% bindEvent(input$map)
    # debug -------------------------------------------------------------------

    output$react <- renderText(rv$filtered %>% unlist())
    output$message <- renderText(c(
      # "headwaters",
      # headwaters(),
      # "swimming",
      # swimming(),
      # "P lakes",
      # P_lakes(),
      "na values:",
      sapply(data.df(), function(y) sum(length(which(is.na(y)))))
      # "filtered ids:",
      # filtered_ids(),
      # city_vals() %>% unlist(),
      # "merge:",
      # length(city_vals())
    ))

    # spatial_filter_ids() %>% unlist(),
    # "cities:"
    #  city_bounds() %>% unlist()
    # "rows selected",
    # table_info()
    df.toreturn <- reactive(data.df()) %>% bindEvent(input$save) # module returns filtered ids
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
