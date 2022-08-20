header <- dashboardHeader()

sidebar <- dashboardSidebar(

  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Home",tabName = "home"),
    menuItem("filter",  tabName = "filter"),
    menuItem("Criteria", tabName = "Criteria"),
    menuItem("debug", tabName = "debug")
    )
  )


body <- dashboardBody(
  tabItems(
    tabItem("home",
            lpUI("main")
    ),
    tabItem("debug",
            debugUI("main")
    ),
    tabItem("filter",
            filter_page_UI("main")
    ),
    tabItem("Criteria",
            criteria_page_UI2("main")
    )
  )
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) {
    load(here::here("data", "subbasin_data.rda"))
    data = subbasin_data

    filter_page_server("main",data)
    criteria_page_server2("main")
    debugServer('main',subbasin_data)

  }
)

