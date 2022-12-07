load(here::here("data", "subbasin_data.rda"))

#palette for maps and charts
#theme colors
# Create the theme

file.info('app.R')$ctime %>% as.Date()

landingpage_ui <- function(go_button,learn_button) {
  backgroundImageCSS <- "height: 91vh;
                       background-position: right;
                       background-repeat: no-repeat;
                       background-size: cover;
                       background-image: url('%s');
                       "
  titleCSS <- "class: page-header;
  "
  tagList(
fluidRow(
  column(
    width = 6,style = 'class: page-header',
   absolutePanel(style = "height: 91vh;",box(width = 12,
                                           style = "height: 91vh;",



 h1("King County Stormwater Retrofit Planner",style = "font-size: 5rem;
    font-weight: 600; line-height: 1.2;
    $font-family-sans-serif:
  // Cross-platform generic font family (default user interface font)
  system-ui,
  // Safari for macOS and iOS (San Francisco)
  -apple-system,
  // Windows
  'Segoe UI',!default;"),

  h1("Multi-Criteria Decision Support System for Stormwater Retrofits",
     style = "
     margin-bottom: 1.6rem;
     font-size: 2rem;
    font-weight: 300;
    $font-family-sans-serif:
  // Cross-platform generic font family (default user interface font)
  system-ui,
  // Safari for macOS and iOS (San Francisco)
  -apple-system,
  // Windows
  'Segoe UI',!default;"
     ),

  br(),
    #HTML(
 #    paste0(
 #      '<small><samp> Updated: ',file.info('app.R')$ctime %>% as.Date(),' </samp></small>'),
 #      '</div>')
 # #),
    go_button,learn_button))),
  column(width = 6,
        box(width = 12,
tags$img(src='img1.jpeg',style="height: 100%;")))
         #style = sprintf(backgroundImageCSS, "img1.jpeg")
#

    ))
}

# about_ui <- function(){
#   #shinyWidgets::setBackgroundColor(color = "#194663")
#   tagList(HTML(
#     paste0(
#   '<div class="embed-responsive embed-responsive-4by3">
#       <iframe class="embed-responsive-item"
#" src="https://storymaps.arcgis.com/stories/da6688d548a44aea8171222b6d3ce5b7"></iframe>
#         </div>'
#     )))
# }

ui <-
  shinydashboardPlus::dashboardPage(
    header=dashboardHeader(
      # These are examples of new widgets to perhaps include
      #leftUi = tagList(
      #dropdown(label= "dropdown"),
      #dropdownBlock(badgeStatus = "primary",
      #  id = 'block', icon = icon("table"),title = "block")),
       #                      controlbarIcon = icon('table')),
),
        #=


    sidebar = shinydashboardPlus::dashboardSidebar(
      sidebarMenu(id = 'tabs',
       menuItem("Home",tabName = "lp"),
       #menuItem("About",tabName = "about"),
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
           actionButton("learn",label = "Learn More", class = "btn btn-secondary btn-lg", icon = icon('external-link-alt'),
                        onclick ="window.open('https://storymaps.arcgis.com/stories/da6688d548a44aea8171222b6d3ce5b7')"
                        ))),
         #tabItem("about",  about_ui()),
         tabItem("filter_locations",filter_page_UI("filter-main")),
         tabItem("criteria",criteria_page_UI2("criteria_main"))
        # tabItem("Debug",debugUI("main"))

        )
      ),
footer  = dashboardFooter(
  right = paste0(
   "Copyright © ", Sys.Date() %>% lubridate::year(), " Geosyntec Consultants, Inc."),
)

)

server <- function(input, output, session) {



  observe(
    updateTabItems(session,"tabs","filter_locations"))  %>% bindEvent(input$go)

  observe(
    updateTabItems(session,"tabs","about"))  %>% bindEvent(input$learn)

  rv <- reactiveValues(base_data = subbasin_data,filtered_data = NULL,filtered_shps=NULL, top_basins = NULL, top_shps = NULL)
  #mock_filtered <- reactive(subbasin_data %>% filter(WQBE_basin ==    "White")) #%>% sample_n(100))
  rv2 <- reactiveValues(filtered_data = subbasin_data %>% select_if(is.numeric),top_basins = NULL,filtered_shps=subbasin_shps,  top_basins = NULL, top_shps = NULL)
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
