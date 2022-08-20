ui <- fluidPage(
  shinydashboardPlus::dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      sidebarMenu(id)
    ),
    body = shinydashboard::dashboardBody(filter_page_UI("test"))


  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(metrics = subbasin_metrics)
  filter_page_server('test',rv)
}

shinyApp(ui, server)
