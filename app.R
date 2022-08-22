library(data.table)
require(shinyBS)
load(here::here("data", "subbasin_data.rda"))
ui <-
  shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(),
    sidebar = shinydashboardPlus::dashboardSidebar(
      sidebarMenu(id = 'tabs',
       menuItem("Home",tabName = "lp"),
        menuItem("Filter Locations", tabName = "filter_locations"),
        menuItem("Criteria", tabName = "criteria"),
       menuItem("Debug",tabName = "Debug")

      )
    ),
    body =
      shinydashboard::dashboardBody(
        tabItems(
          tabItem("lp",  lpUI("main")),
          tabItem("filter_locations",filter_page_UI("main")),
          tabItem("criteria",criteria_page_UI("main")),
          tabItem("Debug",debugUI("main"))
        )
      ))

server <- function(input, output, session) {

  #reactive values to be passed to modules
  #rv <- reactiveValues(subbasin_data = subbasin_data, metrics = metrics) #, basin_ids = subbasin_metrics %>% rownames())
  filter_page_server("main",subbasin_data)
  lpServer("main")
  criteria_page_server("main")
  debugServer("main",mtcars)
}

shinyApp(ui, server)
