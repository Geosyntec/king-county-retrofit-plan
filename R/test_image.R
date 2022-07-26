
library(shiny)

ui <- fluidPage(

  tags$img(src = 'bkrnd1.jpg')
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
