version_number <- "1.0-alpha"
date_text <- file.mtime('app.R')
jumbotron <-   HTML(
  paste0(
'<div class="jumbotron">
  <h1 class="display-4">King County Stormwater Retrofit Planner</h1>
  <p class="lead">This is a simple hero unit, a simple jumbotron-style component for calling extra attention to featured content or information.</p>
  <hr class="my-4">
  <p>It uses utility classes for typography and spacing to space content out within the larger container.</p>
  <p class="lead">',actionButton('start', label = "Get Started", class = "btn btn-primary btn-lg"),
  '</p>
  </div>'))


lpUI <- function(id) {
  ns <- NS(id)
  tagList(
    # #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    # absolutePanel(
    #   fixed = TRUE, top = 10, right = 10,
    #   tags$img(height = "100%", src = "bkrnd1.jpg")
    # ),
    jumbotron,
    # fixedRow(column(
    #   width = 8, offset = 2,
    #   shinydashboardPlus::box(
    #     closable = FALSE, status = "primary",
    #     width = 12, collapsible = FALSE,
    #     #title
    #     title = "King County Stormwater Retrofit Planner",
    #     footer =
    #   #  (boxLabel("version a.01", status = "warning",style = "font-size:1.3rem;font-weight:100;")),
    #     span(paste("Last updated:", date_text),style = "font-size:1rem;")
    #
  #)))

    )
}


# tagList(shinyWidgets::setBackgroundImage(src = 'bkrnd1.jpg', shinydashboard = FALSE))





lpServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}

ui <- fluidPage(
  lpUI("main")
)

server <- function(input, output, session) {
  lpServer("main")
}

shinyApp(ui, server)
