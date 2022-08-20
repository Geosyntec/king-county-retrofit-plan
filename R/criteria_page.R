library(shiny)
library(rhandsontable)
library(shinyjs)
library(tibble)
library(dplyr)
library(shinydashboard)
library(shinydashboardPlus)






criteria_page_UI <- function(id) {
  ns <- NS(id)

  tagList(

# Inputs column -----------------------------------------------------------


    # column(width = 12,
    # box(title = "Inputs", width = 12,
    #     dropdownMenu = boxDropdown(
    #   boxDropdownItem(
    #     radioButtons(
    #       inputId = ns("goal_tiers_select"),
    #       label = "Select which level to apply weighting",
    #       choices = c("Goals","Subgoals","Criteria"),
    #       selected = "Goals",
    #       inline = TRUE),
    #   )),
    #
    #     radioButtons(
    #   inputId = ns("orientation_select"),
    #   label = "Select Orientation",
    #   choices = c("Restoration","Protection"),
    #   selected = "Restoration",
    #   inline = TRUE,
    # ),
    #
    #
    # strong("Criteria Weights")),


  h1("Table here"),
  rHandsontableOutput(ns("hot"))
  #)
   # ),

# # Results Column ----------------------------------------------------------
#
#   column(width = 8,
#   br(),
#   actionButton(ns("saveBtn"), "Save changes"),
#   actionButton(ns("resetBtn"), "Reset Table"),
#   tableOutput(ns("hotvals"))
#)
)

}


criteria_page_server <- function(id,rv) {
  moduleServer(id,
               function(input, output, session) {




  # all_metrics <- reactive({
  #   rv$metrics %>% add_column(Include = TRUE,.before=1) %>%
  #   relocate(Subgoal,.before=3) %>% add_column(Weight=0,
  #                                              Indt = 0,
  #                                              PreT = 0,
  #                                              PreF = 0,
  #                                              gaussP = 0)})


  #three different tables for inputs
  #goal_metrics <- reactive(all_metrics() %>% select(c(Goal,Include,Goal_Description,Weight)) %>% unique())
  #sub_goal_metrics <-  reactive(all_metrics() %>% select(c(Goal,Include,Goal_Description,Subgoal,Subgoal_Description,Weight)) %>% unique())


# user.tables <- reactiveValues(
#   goals = goal_metrics(),
#   subgoals = sub_goal_metrics(),
#   criteria = all_metrics())

  # observe({
  #   # if(!is.null(input$hot)){
  #   #   user.edits <- as.data.frame(hot_to_r(input$hot))
  #   #     if(input$goal_tiers_select =="Subgoals"){
  #   #       user.tables$subgoals <- user.edits
  #   #     } else {
  #   #       if(input$goal_tiers_select =="Criteria"){
  #   #         #save criteria
  #   #         user.tables$criteria <- user.edits
  #   #       } else {
  #   #         #save goals
  #   #   user.tables$goals <- user.edits
  #   #   #join criteria with weights
  #   #   user.tables$criteria <- merge(metrics %>% select("Name","Goal"),user.edits,by.x="Goal",by.y="Goal")
  #   #   #user.tables$subgoals <- merge(subgoals %>% select("Name","Goal"),user.edits,by.x="Goal",by.y="Goal")
  #   #       }
  #
  #     output$hotvals <- renderTable(all_metrics())
  #       #user.tables$criteria)
  #  # }
  #
  # #}

   # }) %>% bindEvent(input$saveBtn)
  #
  # observe({
  #   if (!is.null(input$hot)){
  #     DF <- (hot_to_r(input$hot))
  #     setHot(DF)
  #   }
  # })


  output$hot <- renderRHandsontable({


    rhandsontable(mtcars)#rv$metrics)
  })


  }
)
  }

  ui <- fluidPage(
    shinydashboardPlus::dashboardPage(
      header = dashboardHeader(),
      sidebar = dashboardSidebar(),
      body = shinydashboard::dashboardBody(criteria_page_UI("test"))


    )
  )

server <- function(input, output, session) {

  load(here::here("data", "subbasin_metrics.rda"))
  load(here::here("data", "city_names.rda"))
  rv <- reactiveValues(metrics = metrics)
  criteria_page_server('test',rv)
}

shinyApp(ui,server)




