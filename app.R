


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
          tabItem("filter_locations",filter_page_UI("filter-main")),
       tabItem("criteria",criteria_page_UI2("criteria-main"))
         #tabItem("Debug",debugUI("main"))
        )
      ))

server <- function(input, output, session) {

  #reactive values to be passed to modules
  #rv <- reactiveValues(subbasin_data = subbasin_data, metrics = metrics) #, basin_ids = subbasin_metrics %>% rownames())
  lpServer("main")
  filtered_results<- filter_page_server("filter-main",subbasin_data)
  criteria_page_server2("criteria-main",filtered_results)
  #debugServer("main",filtered_results)
}

shinyApp(ui, server)
