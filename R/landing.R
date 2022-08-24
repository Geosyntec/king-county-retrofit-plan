version_number <- "1.0-alpha"
date_text <- file.mtime('app.R')
jumbotron <-


lpUI <- function(id) {
  ns <- NS(id)
  tagList(HTML(
    paste0(
      '<div class="jumbotron">
  <h1 class="display-4">King County Stormwater Retrofit Planner</h1>
  <p class="lead">This is a simple hero unit, a simple jumbotron-style component for calling extra attention to featured content or information.</p>
  <hr class="my-4">
  <p>It uses utility classes for typography and spacing to space content out within the larger container.</p>
  <p class="lead">',actionButton(ns('start'), label = "Get Started", class = "btn btn-primary btn-lg"),
      '</p>
  </div>'))
    )
}


# tagList(shinyWidgets::setBackgroundImage(src = 'bkrnd1.jpg', shinydashboard = FALSE))





lpServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    click_out <- reactive("click")%>% bindEvent(input$start)
    return(click_out)
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
