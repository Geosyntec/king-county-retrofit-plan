load(here::here("data", "subbasin_data.rda"))

# palette for maps and charts
# theme colors
# Create the theme

file.info("app.R")$ctime %>% as.Date()

landingpage_ui <- function(go_button, learn_button) {
  backgroundImageCSS <- "height: 650px;
                       background-position: right;
                       background-repeat: no-repeat;
                       background-size: cover;
                       background-image: url('%s');
                       "
  titleCSS <- "class: page-header;
  "
  logo_footer <-
    fluidRow(
      column(
        width = 4,
        style = "align:right;text-align:right;border-style:
          solid; border-width:0px 1px 0px 0px;",
        tags$img(src = "king-county-logo_alt.png",
                 style = "height:70px;")
      ),
      column(
        width = 4,
        style = "align:right;text-align:right;border-style:
          solid; border-width:0px 1px 0px 0px;",
        tags$img(
          src = "geosyntec_logo.png",
          style = "height:50px;"
        )),
      column(
        width = 4,
        tags$img(src = "RKI.sm2.blk-cropped.png", style = "height:60px; padding:0px; margin-left:0;
                                       align:left")
      ))


  tagList(
    wellPanel(style="background-color: white;",
    fluidRow(
      shinydashboardPlus::box(title = NULL, headerBorder = FALSE,
        width = 6,
        style = "height: 500px;",
        footer = logo_footer,
        # 91vh;",
        # column 1 --------------------------------------------
        column(
          width = 12, style = "class: page-header",
          absolutePanel(
            #style = "height: 91vh;",
            h1("King County Stormwater Retrofit Planner", style = "font-size: 5rem;
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
            go_button, learn_button,
          )
        )),
  column(width = 6,
         tags$img(src = "img1.jpeg", style = backgroundImageCSS))
      ),
  paste0(
        "Copyright © ", Sys.Date() %>% lubridate::year(), " Geosyntec Consultants, Inc."
      ),
  br(),
  a('Open Source License Info',href='https://github.com/Geosyntec/king-county-retrofit-plan/blob/c84c5d96f1f830d7de9136a70486413ccfbc031d/LICENSE')



      # column2 ---------------------------------------------



      # style = sprintf(backgroundImageCSS, "img1.jpeg")
      #
  ))
}

# about_ui <- function(){
#   #shinyWidgets::setBackgroundColor(color = "#194663")
#   tagList(HTML(
#     paste0(
#   '<div class="embed-responsive embed-responsive-4by3">
#       <iframe class="embed-responsive-item"
# " src="https://storymaps.arcgis.com/stories/da6688d548a44aea8171222b6d3ce5b7"></iframe>
#         </div>'
#     )))
# }

ui <-
  shinydashboardPlus::dashboardPage(
    header = dashboardHeader(
      # These are examples of new widgets to perhaps include
      # leftUi = tagList(
      # dropdown(label= "dropdown"),
      # dropdownBlock(badgeStatus = "primary",
      #  id = 'block', icon = icon("table"),title = "block")),
      #                      controlbarIcon = icon('table')),
    ),
    # =


    sidebar = shinydashboardPlus::dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "lp"),
        # menuItem("About",tabName = "about"),
        menuItem("Pre-screen", tabName = "filter_locations"),
        menuItem("Prioritize Subbasins", tabName = "criteria")
        # menuItem("Debug",tabName = "Debug")
      )
    ),
    body =
      shinydashboard::dashboardBody(
        use_theme(kingco_theme), # <-- use the theme
        tabItems(
          tabItem("lp", landingpage_ui(
            actionButton("go", label = "Get Started", class = "btn btn-primary btn-lg", style = "color:white"),
            actionButton("learn",
              label = "Learn More", class = "btn btn-secondary btn-lg", icon = icon("external-link-alt"),
              onclick = "window.open('https://storymaps.arcgis.com/stories/da6688d548a44aea8171222b6d3ce5b7')"
            )
          )),
          # tabItem("about",  about_ui()),
          tabItem("filter_locations", filter_page_UI("filter-main")),
          tabItem("criteria", criteria_page_UI2("criteria_main"))
          # tabItem("Debug",debugUI("main"))
        )
      )
    # footer = shinydashboardPlus::dashboardFooter(
    #   right = paste0(
    #     "Copyright © ", Sys.Date() %>% lubridate::year(), " Geosyntec Consultants, Inc."
    #   ),
    # )
  )

server <- function(input, output, session) {
  observe(
    updateTabItems(session, "tabs", "filter_locations")
  ) %>% bindEvent(input$go)

  observe(
    updateTabItems(session, "tabs", "about")
  ) %>% bindEvent(input$learn)

  rv <- reactiveValues(base_data = subbasin_data, base_shps = subbasin_shps, filtered_data = NULL, filtered_shps = NULL, top_basins = NULL, top_shps = NULL)

  rv2 <- reactiveValues(filtered_data = NULL, top_basins = NULL, filtered_shps = subbasin_shps, top_basins = NULL, top_shps = NULL)

  # get filtered data from filter-main
  filtered_data <- reactive(filter_page_server("filter-main", rv))


  # pass filtered data to second reactive values


  observe({
    rv2$filtered_data <- filtered_data()[[1]] %>% select_if(is.numeric)
    rv2$filtered_shps <- filtered_data()[[2]]
  }) %>% bindEvent(filtered_data())
  # observe(
  #   updateTabItems(session,"tabs","criteria"))  %>% bindEvent(input$learn)
  observe(
    updateTabItems(session, "tabs", "criteria")
  ) %>% bindEvent(filtered_data())
  criteria_page_server2("criteria_main", rv2)
  # criteria_page_server2("criteria-main",filtered_results)
  debugServer("main", rv2)
}

shinyApp(ui, server)
