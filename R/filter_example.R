library(shiny)
library(leaflet)
library(reactable)
library(crosstalk)

load("~/Documents/repos/king-county-retrofit-plan/data/watershed_shapefile.rda")

shape_df <- watershed_shapefile %>% head(100)

shared_d <- SharedData$new(shape_df)

ui <- fluidPage(
  #  textOutput("texto"),

  crosstalk::filter_slider('imperv','Percent Impervious',shared_d,width=250,
                           column=~PrcntIm),
  bscols(
  reactableOutput("tbl"),
  leafletOutput(outputId = "map"))
)

server <- function(input, output) {



  output$map <- renderLeaflet({
    leaflet(shared_d) %>%
      addTiles()
  })
  output$tbl <- renderReactable({
    t<-  reactable(
      shared_d,
      onClick = "select",
      selection = "multiple",
      selectionId = "sel"
    )
  })

  d_new <- reactive({
    shared_d$data()#[getReactableState("tbl","selected"),]
  })

  observeEvent(as.character(getReactableState("tbl","selected")), {
    #  output$texto <- renderText(print(getReactableState("tbl","selected")))

    if (is.null(getReactableState("tbl","selected"))){
      leafletProxy("map", data = shared_d) %>%
        clearShapes() %>%
        addPolygons()
        #clearMarkers() %>%
        #addMarkers(lng = ~long, lat = ~lat, icon = icon_x)
    }
    else{
      leafletProxy("map", data = d_new()) %>%
        clearShapes() %>%
        #clearMarkers() %>%
        addPolygons()
        #addMarkers(lng = ~long, lat = ~lat, icon = icon_y)
    }
  })
}
shinyApp(ui = ui, server = server)
