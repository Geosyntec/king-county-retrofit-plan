
## Create main app ui ---------------------
#
#' generate the main page
#'
#' @return home tabItem for use in dashboardPage
#' @import shiny
home <- function() {
  tabItem(
    tabName = "home",
    tags$style(".fa-home {color:#00abe7}"),
    tags$style(".fa-filter {color:#00abe7}"),
    tags$style(".fa-building {color:#00abe7}"),
    tags$style(".fa-sliders {color:#00abe7}"),
    tags$style(".fa-user-edit {color:#00abe7}"),
    tags$style(".fa-tasks {color:#00abe7}"),
    # absolutePanel(
    #   top = 0, left = 0, right = 0, height = "100%", fixed = TRUE,
    # tags$img(
   # shinyWidgets::setBackgroundImage(shinydashboard = TRUE,
                                  #   src = "img4.jpg"
    #),
    # style ='opacity: 0.8')
    #)
    #),
    div(class="d-flex justify-content-center",
        fluidRow(column(
          width = 12,
          p(),
          # absolutePanel(
          # top = 100,
          # title, etc,
          # width = "100%",
          align = "center",
          style = "font-family: Tahoma, Georgia, Verdana, sans-serif;font-size: 100px; color:#ffffff;",
          "SMAPr",
          h4(
            "Stormwater Action Planning for",
            br(),
            "the Green/Duwamish Watershed"
          )
        )),
        br(),
        fluidRow(column(
          width = 8,
          offset = 2,
          align = "center",

          shiny::actionButton(inputId = "go_button", label = "Get Started")
        )),
        br(), br(), br(), # br(),br(),br(),

        # column(width = 12,
        # absolutePanel(left = 0,right= 0, bottom =0, height= "40%", style = "background-color:#ffffff;
        #               opacity:0.6"),
        fluidRow(
          # absolutePanel(left = 0,right= 0,
          #   top = 100,
          # box(width = 12, solidHeader = TRUE,

          # fluidRow(column(width = 8, offset = 2,
          # shinydashboard::box(width = 3,height = 500,
          #
          #     #box(
          #       title= (htmltools::browsable(tags$i(class = "far fa-building fa-3x"))),
          #
          #       solidHeader = FALSE,
          #       collapsible = FALSE,
          #
          #    h3("Select your city"),
          #    h5("SMAPr applies to every city in the Green/Duwamish Watershed")
          #     ),

          # div(class = "container",
          column(width = 12, align = "center",
                 div(class="d-flex justify-content-center",
                     div(#class = "col-sm-6 col-md-12 col-lg-12",
                       column(width =
                                8, offset = 2,
                              shinydashboard::box(
                                width = 12, # status = "primary",
                                solidHeader = TRUE,
                                fluidRow(column(12,
                                                # align = "center",
                                                style = 'font-family:"Segoe UI", Arial, sans-serif; font-weight:400;

                               font-size:30px; color:#263238;',
                                                ("How SMAPr Works")
                                )),
                                hr(),
                                column(
                                  4,
                                  (htmltools::browsable(tags$i(class = "fas fa-check-double fa-3x"))),
                                  h3("Select your watersheds"),
                                  h5("Choose which watersheds to prioritize. Clip watersheds to your city or
               include watersheds you share with neighbors.")
                                ),
                                # box(width = 12, status = "primary",
                                column(
                                  4,
                                  (htmltools::browsable(tags$i(class = "fas fa-user-edit fa-3x"))),
                                  h3("Build your Inventory"),
                                  h5("SMAPr applies to every city in the Green/Duwamish Watershed")
                                ),
                                # box(width = 12, status = "primary",
                                column(
                                  4,
                                  (htmltools::browsable(tags$i(class = "fas fa-tasks fa-3x"))),
                                  h3("Prioritize Watersheds"),
                                  h5("Find the highest priority watershed based on criteria you choose.")
                                )
                              )))))

          #   )

        )

    )
  )
}

# app pages
###
#' watershed selection page -------
#'
#' @return tabPanel for dashboard
filter_locations <- function() {
  tabItem(
    tabName = "filter_locations",
    filter_locations_UI("main",
                        accept_button = actionButton(inputId = "ws_select", "Save Watersheds", icon = icon("check"))),

    # )
  )
}

# app pages
###
#' watershed inventory page ----------------
#'
#' @return tabPanel for dashboard
watershed_inventory <- function() {
  tabItem(
    tabName = "watershed_inventory"
  )
}

#' priorize basins page
#'
#' @return tabPanel for dashboard
#'
prioritize_basins <- function() {
  tabItem(
    "prioritize_basins",
    # h1('priorize page'),
    #mod_mcda_outer_UI("main")
    #mod_decision_support_ui("dev")
  )
}

#' For debugging
#'
#' @return tabPanel for dashboard
#'
devpage <- function() {
  tabPanel(
    title = "dev",
    id = "dev",
    dev_ui("main")
  )
}



# build ui
ui_home <-
  shinydashboard::dashboardPage(
    #skin = "black",

    #fresh::use_theme(ogd_theme), # <-- use the theme

    # footer = shinydashboardPlus::dashboardFooter(
    #   left = p("version 0.2"),
    #   right =p("©2021 Geosyntec Consultants, Inc.",
    #   (HTML(
    #       paste0(
    #         "Licensed under Mozilla Public License Version 2.0",
    #         a(href = "https://stackoverflow.com/")
    #       )
    #     )
    #   )
    # )),
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
        #menuItem("dev", icon = icon("bug"), tabName = "dev")
      )
    ),
    body = shinydashboard::dashboardBody(
      # tags$head(
      # tags$style(HTML('
      # .form-group, .selectize-control {
      #      margin-bottom: 2px;
      # }
      # .col-sm-3 {padding: 0px;margin: 0px;
      # }
      #
      # .box-body {
      #     padding: 2px;
      # }'))
      # ),
      tabItems(
        tabItem("home", home()),
        tabItem(tabName = "filter_locations", filter_locations()),
        # ,


        tabItem(tabName = "wsmap", watershed_inventory()),
        # ,
        tabItem(tabName = "prioritize_basins",prioritize_basins())
      )

    )
  )


server_home <- function(input, output, session) {


  rv <- reactiveValues()

  rv$data <- test_data
  rv$shps <- NULL



    #  rv <- dev_server('main',rv)

    updateTabItems(session, inputId = "sidebar_menu", selected = "filter_locations")

    filter_locations_server("main", rv) # returns a reactive value df




  observeEvent(input$ws_select, {


    print("CITY")
    print(rv$city)
    print("DATA")
    #print(rv$data)
    print("SHPS")
    print(rv$shps)

   # mod_ws_inventory_server("main",rv)
    updateTabItems(session, inputId = "sidebar_menu", selected = "wsmap")
  })
 # mod_mcda_outer_server("main",rv)
  observe(print(rv$user_cols))
}





shinyApp(ui_home, server_home)


