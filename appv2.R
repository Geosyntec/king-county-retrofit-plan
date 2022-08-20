
ui <-
  dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      id = "sidebar",
      sidebarMenu(id = 'tabs',
       menuItem("Home",tabName = "lp"),
        menuItem("Filter Locations", tabName = "filter_locations"),
        menuItem("Criteria", tabName = "criteria")

      )
    ),
    body =
      dashboardBody(
        tabItems(
          tabItem("lp",  lpUI("main")),
          tabItem(
            "filter_locations",
          filter_page_UI("main")
          ),
          tabItem(
            "criteria",
            criteria_page_UI("one")
          )
        )
      ))

server <- function(input, output, session) {
  criteria_page_server("one")
  filter_page("main")
  lpServer("main")
}

shinyApp(ui, server)
