landingpage_ui <- function(button) {
  tagList(HTML(
    paste0(
      '<div class="jumbotron">
  <h1 class="display-4">King County Stormwater Retrofit Planner</h1>
  <p class="lead">This is a simple hero unit, a simple jumbotron-style component for calling extra attention to featured content or information.</p>
  <hr class="my-4">
  <p>It uses utility classes for typography and spacing to space content out within the larger container.</p>
  <p class="lead">',button),
      '</p>
  </div>')
  )
}

about_ui <- function(){
  tagList(HTML(
    paste0(
  '<!-- 16:9 aspect ratio -->
    <div class="embed-responsive embed-responsive-16by9">
      <iframe class="embed-responsive-item" src="https://www.kingcounty.gov"></iframe>
        </div>'
    )))
}

ui <-
  shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(),
    sidebar = shinydashboardPlus::dashboardSidebar(
      sidebarMenu(id = 'tabs',
       menuItem("Home",tabName = "lp"),
       menuItem("About",tabName = "about"),
        menuItem("Pre-screen", tabName = "filter_locations"),
        menuItem("Prioritize Subbasins", tabName = "criteria"),
       menuItem("Debug",tabName = "Debug")
      )
    ),
    body =
      shinydashboard::dashboardBody(
        tabItems(

        tabItem("lp",  landingpage_ui(actionButton("go",label = "Get Started", class = "btn btn-primary btn-lg", style = "color:white"))),
        tabItem('about',about_ui()),
        tabItem("filter_locations",filter_page_UI("filter-main")),
        tabItem("criteria",criteria_page_UI2("criteria-main"))
         #tabItem("Debug",debugUI("main"))
        )
      ))

server <- function(input, output, session) {

  #reactive values to be passed to modules
  #rv <- reactiveValues(subbasin_data = subbasin_data, metrics = metrics) #, basin_ids = subbasin_metrics %>% rownames())


  observeEvent(input$go,{
    updateTabItems(session,"tabs","filter_locations")
  })

  filtered_results <- filter_page_server("filter-main",subbasin_data)

  observe(
    updateTabItems(session,"tabs","criteria")
  ) %>% bindEvent(filtered_results())

  criteria_page_server2("criteria-main",filtered_results)
  #debugServer("main",filtered_results)
}

shinyApp(ui, server)
