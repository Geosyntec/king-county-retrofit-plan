library(shiny)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(here)
source(here::here('R','promethee_2.R'))

# ui object
ui <- fluidPage(
    titlePanel(p("Spatial app", style = "color:#3474A7")),
    sidebarLayout(
        sidebarPanel(
            # fileInput(
            #     inputId = "filedata",
            #     label = "Upload data. Choose csv file",
            #     accept = c(".csv")
            # ),
            fileInput(
                inputId = "filemap",
                label = "Upload map. Choose shapefile",
                multiple = TRUE,
                accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")
            ),
            selectInput(multiple = TRUE, selectize = TRUE,
                inputId = "variableselected",
                label = "Select variable",
                choices = c("cases", "population")
            )
            ),
        #     selectInput(
        #         inputId = "yearselected",
        #         label = "Select year",
        #         choices = 1968:1988
        #     ),
        #     p("Made with", a("Shiny",
        #                      href = "http://shiny.rstudio.com"
        #     ), "."),
        #     img(
        #         src = "imageShiny.png",
        #         width = "70px", height = "70px"
        #     )
        # ),

        mainPanel(
            leafletOutput(outputId = "map"),
           # dygraphOutput(outputId = "timetrend"),
            DTOutput(outputId = "table"),
           textOutput(outputId = "outColumns")
        )
    )
)

# server()
server <- function(input, output,session) {

#get choices for dropdown

#extract the data
    # read.dbf(file, as.is = FALSE)
    #
    # dbf_data <- reactive({
    #     req(input$filemap)
    #
    # })
    # outVar = reactive({
    #     req(input$filemap)
    #     choices = get(input$filemap)
    #     choices %>% select_if(is.numeric) %>% colnames()
    #
    # })
    # observe({updateSelectInput(session,"variableselected",choices = outVar())})
    #
    #
    map <- reactive({
        req(input$filemap)

        # shpdf is a data.frame with the name, size, type and
        # datapath of the uploaded files
        shpdf <- input$filemap

        # The files are uploaded with names
        # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
        # (path/names are in column datapath)
        # We need to rename the files with the actual names:
        # fe_2007_39_county.dbf, etc.
        # (these are in column name)

        # Name of the temporary directory where files are uploaded
        tempdirname <- dirname(shpdf$datapath[1])

        # Rename files
        for (i in 1:nrow(shpdf)) {
            file.rename(
                shpdf$datapath[i],
                paste0(tempdirname, "/", shpdf$name[i])
            )
        }

        # Now we read the shapefile with readOGR() of rgdal package
        # passing the name of the file with .shp extension.

        # We use the function grep() to search the pattern "*.shp$"
        # within each element of the character vector shpdf$name.
        # grep(pattern="*.shp$", shpdf$name)
        # ($ at the end denote files that finish with .shp,
        # not only that contain .shp)
        map <- readOGR(paste(tempdirname,
                             shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                             sep = "/"
        ))
        map
    })

    output$table <- renderDT(map()@data)



    outVar = reactive({
             req(input$filemap)
             map()@data %>% select_if(is.numeric) %>% colnames()

         })

    #observe({updatete(session,"variableselected",choices = outVar())})
    #output$outColumns <- renderText(outVar())

    observe({updateSelectInput(session,"variableselected",choices = outVar())})

 #    output$timetrend <- renderDygraph({
 #        data <- data()
 #        dataxts <- NULL
 #        counties <- unique(data$county)
 #        for (l in 1:length(counties)) {
 #            datacounty <- data[data$county == counties[l], ]
 #            dd <- xts(
 #                datacounty[, input$variableselected],
 #                as.Date(paste0(datacounty$year, "-01-01"))
 #            )
 #            dataxts <- cbind(dataxts, dd)
 #        }
 #        colnames(dataxts) <- counties
 #        dygraph(dataxts) %>%
 #            dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1
 #        d1$x$css <- "
 # .dygraph-legend > span {display:none;}
 # .dygraph-legend > span.highlight { display: inline; }
 # "
 #        d1
 #    })

    output$map <- renderLeaflet({
        if (#is.null(data()) |
            is.null(map())) {
            return(NULL)
        }

        map <- map()
        #data <- data()

        # # Add data to map
        # datafiltered <- data[which(data$year == input$yearselected), ]
        # ordercounties <- match(map@data$NAME, datafiltered$county)
        # map@data <- datafiltered[ordercounties, ]
        #
        # # Create variableplot
        # map$variableplot <- as.numeric(
        #     map@data[, input$variableselected])
        #
        # # Create leaflet
        # pal <- colorBin("YlOrRd", domain = map$variableplot, bins = 7)
        # labels <- sprintf("%s: %g", map$county, map$variableplot) %>%
        #     lapply(htmltools::HTML)

        l <- leaflet(map) %>%
            addTiles() %>%
            addPolygons()
            # addPolygons()
            #     fillColor = ~ pal(variableplot),
            #     color = "white",
            #     dashArray = "3",
            #     fillOpacity = 0.7,
            #     label = labels
            # ) %>%
            # leaflet::addLegend(
            #     pal = pal, values = ~variableplot,
            #     opacity = 0.7, title = NULL
            # )
    })
}

# shinyApp()
shinyApp(ui = ui, server = server)
