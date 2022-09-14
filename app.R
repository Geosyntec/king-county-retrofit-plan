load(here::here("data", "subbasin_data.rda"))

#palette for maps and charts


#theme colors
# Create the theme



landingpage_ui <- function(go_button,learn_button) {



  tagList(HTML(
    paste0(
      '<div class="jumbotron">
  <h1 class="display-4">King County Stormwater Retrofit Planner</h1>
  <p class="lead">This is a simple hero unit, a simple jumbotron-style component for calling extra attention to featured content or information.</p>
  <hr class="my-4">
  <p>It uses utility classes for typography and spacing to space content out within the larger container.</p>
  <p class="lead">'),
      '</p>
  </div>'),
    go_button,learn_button
  )
}

about_ui <- function(){
  #shinyWidgets::setBackgroundColor(color = "#194663")
  tagList(HTML(
    paste0(
  '<div class="embed-responsive embed-responsive-4by3">
      <iframe class="embed-responsive-item" src="https://storymaps.arcgis.com/stories/da6688d548a44aea8171222b6d3ce5b7"></iframe>
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
        menuItem("Prioritize Subbasins", tabName = "criteria")
      # menuItem("Debug",tabName = "Debug")
      )
    ),
    body =
      shinydashboard::dashboardBody(
        use_theme(kingco_theme), # <-- use the theme
        tabItems(
         tabItem("lp",  landingpage_ui(
           actionButton("go",label = "Get Started", class = "btn btn-primary btn-lg", style = "color:white"),
           actionButton("learn",label = "Learn More", class = "btn btn-secondary btn-lg"))),
         tabItem("about",  about_ui()),
         tabItem("filter_locations",filter_page_UI("filter-main")),
         tabItem("criteria",criteria_page_UI2("criteria_main"))
        # tabItem("Debug",debugUI("main"))

        )
      ))

server <- function(input, output, session) {

  #reactive values to be passed to modules
  #rv <- reactiveValues(subbasin_data = subbasin_data, metrics = metrics) #, basin_ids = subbasin_metrics %>% rownames())


  observe(
    updateTabItems(session,"tabs","filter_locations"))  %>% bindEvent(input$go)

  observe(
    updateTabItems(session,"tabs","about"))  %>% bindEvent(input$learn)

  rv <- reactiveValues(base_data = subbasin_data,filtered_data = NULL,filtered_shps=NULL, top_basins = NULL, top_shps = NULL)
  #mock_filtered <- reactive(subbasin_data %>% filter(WQBE_basin ==    "White")) #%>% sample_n(100))
  rv2 <- reactiveValues(filtered_data = NULL,top_basins = NULL,filtered_shps=NULL,  top_basins = NULL, top_shps = NULL)
  # filtered_results <-
  #filtered_results <- reactive
  filtered_data <- reactive(filter_page_server("filter-main",rv))

  #observe(print(filtered_data()))



  observe({
    rv2$filtered_data <- filtered_data()[[1]] %>% select_if(is.numeric)
    rv2$filtered_shps <- filtered_data()[[2]]


    })%>% bindEvent(filtered_data())
  # observe(
  #   updateTabItems(session,"tabs","criteria"))  %>% bindEvent(input$learn)
  observe(
    updateTabItems(session,"tabs","criteria"))  %>% bindEvent(filtered_data())
  criteria_page_server2("criteria_main", rv2)
  #criteria_page_server2("criteria-main",filtered_results)
  debugServer("main",rv2)

}

shinyApp(ui, server)
