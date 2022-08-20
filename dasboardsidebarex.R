library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# Basic dashboard page template

  ui = dashboardPage(
    dashboardHeader(title = "Example"),
    dashboardSidebar(#minified = TRUE,
      sidebarMenu(

        menuItem("Filter Locations", tabName = "filter_locations"),
        menuItem("Criteria", tabName = "criteria"),
        menuItem("Debug",tabName = "Debug"))



    ),
    dashboardBody(

      tags$style(
        '
        @media (min-width: 768px){
          .sidebar-mini.sidebar-collapse .main-header .logo {
              width: 230px;
          }
          .sidebar-mini.sidebar-collapse .main-header .navbar {
              margin-left: 230px;
          }
        }
        '
      ),
      tabItems(
      tabItem("filter_locations",filter_page_UI("main")),
      tabItem("criteria",criteria_page_UI("main")),
      tabItem("Debug",debugUI("main2"))
      )





  ))
  server = function(input, output) {

  rv <- reactiveValues(metrics = subbasin_metrics) #, basin_ids = subbasin_metrics %>% rownames())

  debugServer("main2",rv)}
shinyApp(ui,server)
