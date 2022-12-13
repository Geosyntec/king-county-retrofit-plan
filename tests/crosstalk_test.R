library(shiny)
library(crosstalk)
library(sf)

source("~/Documents/repos/king-county-retrofit-plan/R/aaa_global.R")

df <- subbasin_data %>% rownames_to_column("SWSID")
sf <- subbasin_shps

df.sf <- sf %>%
  merge(df) %>%
  head(100)
base.sf <- sf %>% merge(df)


abcUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map")), column(
      width = 12, checkboxInput("table_sel", label = "all", value = FALSE),
      DTOutput(ns("table")),
      verbatimTextOutput(ns("output_list"))
    )
  )
}

abcServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues("selected" = df.sf)
      # #get table clicks
      #       s <- reactive({
      #         if(!is.null(input$table_rows_selected)){
      #           df.sf[input$table_rows_selected,]
      #         }else{
      #           df.sf
      #         }
      #       })



      observe({
        table_click <- input$table_rows_selected
        print(table_click)
        idx <- df.sf[table_click, "SWSID"]
        bounds <- idx %>%
          st_bbox() %>%
          as.character()
        print(bounds)
        leafletProxy("map") %>%
          leaflet::removeShape("highlighted") %>%
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
          addPolygons(
            data = idx, color = "yellow", layerId = "highlighted",
            weight = 2, opacity = 0, fillOpacity = 0.8,
            fillColor = "yellow"
          )
      })



      # from_table <- reactive(s())

      #     DT::selectRows(dt_proxy, input$table_rows_all)
      #   } else {
      #     DT::selectRows(dt_proxy, NULL)
      #   }



      # get map clicks
      observe({
        click <- input$map_shape_click
        if (is.null(click)) {
          return()
        }
        # is shape in group
        if (click$id %in% (rv$selected %>% pull(SWSID))) {
          print("in")
        } else {
          print("out")
        }
        print(click$id)
      })

      output$map <- renderLeaflet({
        leaflet(rv$selected) %>%
          addTiles() %>%
          addPolygons(layerId = ~SWSID, group = "selected") %>%
          addPolygons(data = base.sf, color = "grey", group = "base")
      })

      output$table <- renderDT(
        {
          datatable(df,
            extensions = "Scroller", style = "bootstrap", selection = "single", class = "compact", width = "100%",
            options = list(deferRender = TRUE, scrollY = 300, scroller = TRUE)
          )
        },
        server = FALSE
      )

      # observe(print(sd$data(withSelection = TRUE)))
    }
  )
}

ui <- fluidPage(
  abcUI("test")
)


server <- function(input, output) {
  abcServer("test")
}

shinyApp(ui = ui, server = server)
