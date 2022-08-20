ui <-
  shinydashboardPlus::dashboardPage(
    # shinydashboard::dashboardPage(
      title = "Stormwater Retrofits Prioritizer",
       header = shinydashboardPlus::dashboardHeader(title = "Stormwater Retrofits Prioritizer"),
       sidebar = shinydashboardPlus::dashboardSidebar(
         #id = "sidebar",
         sidebarMenu(
           id = "sidebar_menu",
           #menuItem("Home", icon = icon("home"), tabName = "home"),
           menuItem("Filter Locations", icon = icon("filter"), tabName = "filter_locations"),
           #menuItem("Build Watershed Inventory", icon = icon("user-edit"), tabName = "wsmap"),
           menuItem("Prioritize Basins", icon = icon("tasks"), tabName = "prioritize_basins")
           )),

    #       #menuItem("dev", icon = icon("bug"), tabName = "dev")
    #     )
    #   ),
    body = shinydashboard::dashboardBody(
         tabItems(
    #       #tabItem("home", home()),
           tabItem(tabName = "filter_locations",
                   filter_page_UI("main")
           ),
    #       # ,
           tabItem(tabName = "prioritize_basins",
                  criteria_page_UI("main")
        ))))


#)
 #   )


  #  )
#)
server <- function(input, output, session) {
  criteria_page_server('main')
}

shinyApp(ui,server)
