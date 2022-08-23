library(shinydashboardPlus)

criteria_page_UI2 <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    column(width = 6,


    #dataTableOutput(ns('table')),
    uiOutput(ns("card1")),
    uiOutput(ns("card2")),
    # dataTableOutput(ns("criteria")),
    # tableOutput(ns('ranked_list')),
    dataTableOutput(ns('metrics_out'))
  ),
  column(width = 6,
                          box(title = "Inputs", width = 12,
                              dropdownMenu = boxDropdown(
                            boxDropdownItem(
                              radioButtons(
                                inputId = ns("goal_tiers_select"),
                                label = "Select which level to apply weighting",
                                choices = c("Goals","Subgoals","Criteria"),
                                selected = "Goals",
                                inline = TRUE),
                            )),

                              radioButtons(
                            inputId = ns("orientation_select"),
                            label = "Select Orientation",
                            choices = c("Restoration","Protection"),
                            selected = "Restoration",
                            inline = TRUE,
                          ),


                          strong("Criteria Weights")),


                          h1("Table here"),
                          rHandsontableOutput(ns("hot")))))
}

criteria_page_server2 <- function(id,filtered) {
  moduleServer(
    id,
    function(input, output, session) {
    #count number of basins
      cleaned_criteria <- reactive(filtered() %>% select_if(is.numeric)) #%>% na.omit())
      criteria_names <- reactive(cleaned_criteria() %>% colnames())
      #get column names and match to metrics dictionary
      all_metrics <- reactive({
        metrics %>% dplyr::filter(Name %in% criteria_names()) %>% #%>% add_column(Include = TRUE,.before=1) %>%
            relocate(Subgoal,.before=3) %>% add_column(Weight=0,
                                                       Indt = 0,
                                                       PreT = 0,
                                                       PreF = 0,
                                                       gaussP = 0)})
      goal_metrics <- reactive(all_metrics() %>% select(c(Goal,Goal_Description,Weight)) %>% unique())
      sub_goal_metrics <-  reactive(all_metrics() %>% select(c(Goal,Goal_Description,Subgoal,Subgoal_Description,Weight)) %>% unique())





      observe({
         if(!is.null(input$hot)){
           user.edits <- as.data.frame(hot_to_r(input$hot))}
      })

      DF <- reactive(goal_metrics())
      #       if(input$goal_tiers_select =="Subgoals"){
      #         user.tables$subgoals <- user.edits
      #       } else {
      #         if(input$goal_tiers_select =="Criteria"){
      #           #save criteria
      #           user.tables$criteria <- user.edits
      #         } else {
      #           #save goals
      #     user.tables$goals <- user.edits
      #     #join criteria with weights
      #     user.tables$criteria <- merge(metrics %>% select("Name","Goal"),user.edits,by.x="Goal",by.y="Goal")
      #     #user.tables$subgoals <- merge(subgoals %>% select("Name","Goal"),user.edits,by.x="Goal",by.y="Goal")
      #         }
      #
      #     #utput$hotvals <- renderTable(all_metrics())
      #       #user.tables$criteria)
      #  }
      #
      # #}
      #
      # }
      #   })%>% bindEvent(input$saveBtn)
      # #
      # observe({
      #   if (!is.null(input$hot)){
      #     DF <- (hot_to_r(input$hot))
      #     setHot(DF)
      #   }
      # })


      output$hot <- renderRHandsontable({


        rhandsontable(DF())#rv$metrics)
      })



      output$metrics_out <- renderDataTable(
        all_metrics()

      )

      output$criteria <- renderDataTable(cleaned_criteria(), extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
      num_basins <- reactive(cleaned_criteria() %>% nrow())
    #count number of criteria
      num_criteria <- reactive(cleaned_criteria() %>% ncol())

      output$card1 <- renderUI(card(num_basins(),'Basins Selected'))
      output$card2 <- renderUI(card(num_criteria(),'Criteria Selected'))
      output$table <- renderDataTable(filtered() %>% head())
      mcda_results <- reactive(
        promethee_2(dataset = cleaned_criteria()
                    ))
      pf2_outflows <- reactive(mcda_results()[1])
      output$ranked_list <- renderTable(
        pf2_outflows() )

    }
  )
}

ui <- fluidPage(
  criteria_page_UI2("criteria-test")
)

server <- function(input, output, session) {
  source(here::here("R","promethee.R"))
  source(here::here("R","fct_helpers.R"))
  load(here::here("data","subbasin_data.rda"))
  mock_filtered <- reactive(subbasin_data %>% sample_n(10))
  criteria_page_server2("criteria-test",mock_filtered)
}


shinyApp(ui, server)

