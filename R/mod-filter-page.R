
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
            )
            # tabPanel(
            #   width = NULL, title = "Data Check",
            #   verbatimTextOutput(ns("message"))
            # )
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


# get user inputs -------------------------------------


    wria_vals <- reactive(input$wriapicker)

    city_vals <- reactive(
      if(!is.null(
      input$jurisdictionpicker)){
        input$jurisdictionpicker
      } else {
        city_names
      }
    )



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
      cities_shp[cities_shp[, "CITYNAME", drop = TRUE] %in% city_vals(), ] %>% st_set_crs(project_crs)
    })

#returns filterd ids based on city selection
spatial_filter_ids <- reactive({
      if (!is.null(city_vals())) {
        ids <- city_shed_dict %>% filter(CITYNAME %in% city_vals()) %>% as.data.frame()
      } else {
        ids <- city_vals() %>% select(SWSID) %>% as.data.frame()
      }
      return(ids[,'SWSID'])
    })

#observe(print(spatial_filter_ids()))



    table_filtered_ids <- reactive({
      req(rv$base_data)
      user_ids <- rv$base_data %>%
        dplyr::filter(WQBE_basin %in% wria_vals()) %>%
        dplyr::filter(Contains_Swimming_Beaches %in% swimming()) %>%
        dplyr::filter(Is_Headwater_Basin %in% headwaters()) %>%
        dplyr::filter(Presence_of_Shellfish %in% shellfish()) %>%
        dplyr::filter(Drains_to_P_Sensitive_Lake %in% P_lakes()) %>%
        #dplyr::filter(rownames(rv$base_data) %in% spatial_filter_ids()) %>%
        rownames()
      return(
        user_ids[which(user_ids %in% spatial_filter_ids())]
                 )
    })

observe(print(
  table_filtered_ids()
  ))



# join filters ---------------------------------------


    # # this joins spatial and table filters
    # filtered_ids <- reactive({
    #   if (length(table_filtered_ids() > 0)) {
    #     return(table_filtered_ids()[table_filtered_ids() %in% spatial_filter_ids()])
    #   } else {
    #     return(table_filtered_ids())
    #   }
    # })



    data.df <- reactive({
      rv$filtered_data
    })



# Update reactive values ------------------------------
observe({
  #rv$filtered_ids <- filtered_ids()
  newdata <- rv$base_data %>% filter(rownames(rv$base_data) %in% table_filtered_ids())
  rv$filtered_data <- newdata
  newshps <- rv$base_shps %>% filter(SWSID %in% table_filtered_ids())
  rv$filtered_shps <- newshps
})

    observe(print(rv$filtered_data))



    display_table <- reactive({
      rv$filtered_data %>%
        dplyr::select(
          # only show a set of cols,
          c(
            # SWSID,
            WRIA = WQBE_basin,
            #  Imperviousness,
            `Shellfish Beaches` = Presence_of_Shellfish,
            `Phosphorus Sensitive Lakes` = Drains_to_P_Sensitive_Lake,
            Headwaters = Is_Headwater_Basin,
            `Swimming Beaches` = Contains_Swimming_Beaches
          )
        )
    })

# tables ----------------------------------------------


    output$hot <- renderDT(
        display_table(),
      ,
      rownames = TRUE
    )



    # count rows
    output$num_selected <- shiny::renderUI(
      card(
        .num = nrow(rv$filtered_data), .description =
          HTML("Subbasins <br> Selected")
      )
    )





    # shps_filtered <- reactive({
    #   if(length(filtered_ids() > 0)){
    #   subbasin_shps[which(subbasin_shps$SWSID %in% filtered_ids()), ]
    #   } else {
    #       subbasin_shps
    #     }
    # })


    ## button observers ----------------------------------------------------------
    observe({
      rv$filtered_data <- NULL
      rv
      reset("form")
    }) %>% bindEvent(input$reset)
# Map  ------------------------------------------------


    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Base") %>%
        addProviderTiles("Esri.WorldGrayCanvas", group = "Grey") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addLayersControl(
          position = "bottomright", options = layersControlOptions(collapsed = FALSE),
          baseGroups = c("Base", "Satellite", "Grey"),
          overlayGroups = c("City Limits", "All Subbasins", "WRIA Outlines")
        ) %>%
        addPolygons(
          data = subbasin_shps, weight = 0.4, fillOpacity = 0.1,
          color = "#593D3B", fillColor = "#d2d6de", layerId = ~SWSID,
          group = "All Subbasins", options = list(zIndex = 10)
        ) %>%
        addPolygons(
          data = cities_shp, group = "City Limits", dashArray = 5,
          color = "#002673", weight = 2, fill = FALSE,
          fillOpacity = 0.1,
          label = ~CITYNAME,
          highlightOptions = highlightOptions(
            weight = 6, color = "yellow", fill = TRUE, dashArray = 0)
        ) %>%
        addPolygons(
          data = wrias, group = "WRIA Outlines",
          options = #list(
            #zIndex = 30
            pathOptions(clickable = FALSE), color = "#2C8C99",
          weight = 0.8,
          label = ~WQBE_basin,
          highlightOptions = highlightOptions(weight = 3),
          # fillColor = ~wria_pal(WQBE_basin),
          opacity = 1, fillOpacity = 0
        ) %>%
        hideGroup("City Limits")
    })

# observers -------------------------------------------


    ## map observers  -------------------------------------------------------
    map_update <- function(){
      leafletProxy("map") %>%
        clearGroup("Selected Subbasins") %>%
        #removeLayersControl() %>%
        addPolygons(
          data = rv$filtered_shps, weight = 2.5, color = "#28a745",
          fillColor = "#01ff70",opacity = 1.0,
          group = "Selected Subbasins",
          options = pathOptions(clickable = FALSE)
        )
      #%>%
        # addLayersControl(
        #   position = "bottomright", options = layersControlOptions(collapsed = FALSE),
        #   baseGroups = c("Base", "Satellite", "Grey"),
        #   overlayGroups = c("City Limits","Selected Subbasins", "All Subbasins", "WRIA Outlines")
        # )
    }
    #proxy1 - add selected subbasins
    observeEvent(table_filtered_ids(), {
      map_update()
    })
# selected city bounds
    observe({
      leafletProxy("map") %>%
        clearGroup("City Limits") %>%
        addPolygons(
          data = city_bounds(), dashArray = c("2, 2"), fillOpacity = 0.1,
          weight = 1.5, color = "black", group = "City Limits",  options = pathOptions(clickable = FALSE)
        )
    }) %>%
      bindEvent(city_bounds(), ignoreInit = TRUE, ignoreNULL = TRUE)

    ### map click events ------------------------------------------------------------

    observeEvent(input$map_shape_click, {
    #
    #   #capture the info of the clicked polygon
    click <- input$map_shape_click$id
    selected_ids <- reactive(rv$filtered_data %>% rownames())
    #update reactives
    if(click %in% selected_ids()){
      print('in')

      #remove it from the dataframe
      new_df <- rv$filtered_data %>% filter(!rownames(rv$filtered_data) %in% click)
      rv$filtered_data <- new_df
      new_shps <- rv$filtered_shps %>% filter(!SWSID %in% click)
      rv$filtered_shps <- new_shps
  #update map
     map_update()
    }else{
      print('out')
      #add it from the dataframe

      new_data <-  reactive({
        rv$base_data %>%
          filter(rownames(rv$base_data) %in% click ) %>%
        rbind(rv$filtered_data)
        })
      print(new_data())

      rv$filtered_data <- new_data()
      print('df')
      new_shps <- reactive(rv$base_shps  %>% filter(SWSID %in% rownames(new_data()) ))
      #   (rv$base_shps %>% select(SWSID)) %in% click)
      rv$filtered_shps <- new_shps()

      map_update()
    }
    })

    # #print(filtered_ids())
    # #
    # #   #subset your table with the id of the clicked polygon
    # #selected <- filtered_ids[mydata$myID == click$id,]
    #
    # #   #if click id isn"t null render the table
    #    # if(!is.null(click$id)){
    #    #  print(click$id)
    #    # }else{
    #    #     print("NULL")}
    #
    #
    # #is clicked value in? shps_filtered
    # # if(!is.null(click$id)) {
    # # if(click$id %in% filtered_ids()) {
    # #   print("in")
    # # } else {
    # #   print("out")
    # #   # new_data <- reactive(c(click$id, filtered_ids()))
    # #   # rv.local$selected_ids <- new_data()
    # #   # print(click$id)
    # #   # print(new_data())
    # # }
    # #   }
    # })







    df.toreturn <- reactive(rv$filtered_data) %>% bindEvent(input$save) # module returns filtered ids
    shps.toreturn <- reactive(rv$filtered_shps) %>% bindEvent(input$save)
    # observe({
    #   rv$filtered_data <-  data.df()
    #   rv$filtered_shps <- shps_filtered()
    # }) %>% bindEbasvent(input$save)

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
  rv <- reactiveValues(base_data = subbasin_data,base_shps = subbasin_shps,  filtered_data = NULL,filtered_shps=NULL, top_basins = NULL, top_shps = NULL)

  # rv <- reactiveValues(base_data = subbasin_data,filtered_data = NULL)
testinfo <- reactive(filter_page_server("test_filter", rv))
testinfo
}

shinyApp(ui, server)
