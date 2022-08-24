library(shinydashboardPlus)
source(here::here("R", "aaa.R"))

make_numeric_inputs <- function(goals.df, id) {
  ns <- NS(id)
  tl <- tagList()
  for (i in 1:nrow(goals.df)) {
    goal_row <- goals.df[i, ]
    goal_info <- paste0("Goal ", goal_row[1], ". ", goal_row[2], ":")


    tl[[i]] <- tagList(fluidRow(
      column(width = 6, strong(goal_info)),
      column(
        width = 6,
        numericInput(
          inputId = ns(paste0("goal", i)),
          label = NULL, value = 0, min = 0, max = 5, step = 1, width = 150),
          class = 'leftAlign')


    ), if(i != nrow(goals.df)){hr()})
  }
  return(tl)
}

# html_to_return =    fluidRow(
#   tags$head(
#   tags$style(type="text/css","label{ display: table-cell; text-align: center;vertical-align: middle; } .form-group { display: table-row;}")
# ),
# column(5,style='background-color:#f2f2f2;min-width: 300px;',
#
#        br(),
#       tags$table(width = "100%", tl
# ))
#
# )
# return(html_to_return)
#             }






goals <- metrics %>% select(Goal, Goal_Description) %>% unique()

criteria_page_UI2 <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(


    # Input Column ------------------------------------------------------------
    column(
      width = 4,
     shinydashboardPlus::box(title = "Preference Inputs", status = "black",solidHeader = TRUE,
                             # style = "margin:px; padding:2px;",
       width = 12,
#panel(heading = "Preference Inputs", status = "primary",extra = fluidRow(



        # box(
        #   title = "Inputs", width = 12,
        #   dropdownMenu = boxDropdown(
        #     boxDropdownItem(
        #
        #     )
        #   ),
        shinydashboardPlus::box(title = "Criteria Orientation",status = "primary",width = 12,
                               # column(width = 12,
        radioButtons(
          inputId = ns("orientation_select"),
          label = NULL, #"Select Orientation",
          choices = c("Restoration", "Protection"),
          selected = "Restoration",
          inline = FALSE
        )
        #)
        ),
        shinydashboardPlus::box(width = 12, status = "primary",title = "Criteria Weights",solidHeader = FALSE,
              dropdownMenu = boxDropdown(boxPad(strong("Advanced Settings")),

                boxDropdownItem("Weight Subgoals", id = ns("apply_at_subgoal")),
                boxDropdownItem("Weight Criteria", id = ns("apply_at_criteria")),
                boxDropdownItem("Edit Thresholds", id = ns("edit_thresholds"))
              ),
              helpText("Provide a preferred weight (0-5) for each criterion"),
        make_numeric_inputs(goals, id)),

#sum up the weights
uiOutput(ns("weight_sum")),
        footer =
        column(width = 12,
               actionButton(ns("reset_weights"),width = "40%", label = "Reset",icon = icon("refresh")),
                actionButton(ns("accept_weights"),width = "40%", label = "Accept", icon = icon("check")))



)
    ),
    # column to hold parameters
    column(
      width = 8,
      tabBox(id = ns("tabset1"),width = 12,
             tabPanel("Results",
      shinydashboardPlus::box(
        width = 12,
        "map/charts",
        shiny::selectInput(ns("n"),selected = 25, multiple = FALSE, label = "number of results to return",
                           choices = c(10,25,50,100)
                           ),
        leafletOutput(ns("map"))
        ),
        shinydashboardPlus::box(
          width = 12,
          "table",
          # dataTableOutput(ns('metrics_out'))
      DTOutput(ns('ranked_list')))
        ),
      tabPanel("debug info",
        DTOutput(ns("table2")),
        verbatimTextOutput(ns("criteria_debug")),
        "user edits:",
        DTOutput(ns("table3"))
      )),

    ), # column to hold outputs


    # dataTableOutput(ns('table')),
    # uiOutput(ns("card1")),
    # uiOutput(ns("card2")),
    # dataTableOutput(ns("criteria")),

  ))
}

criteria_page_server2 <- function(id, filtered) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      goals <- metrics %>% select(Goal, Goal_Description) %>% unique()
      # count number of basins
      cleaned_criteria <- reactive(filtered() %>% select_if(is.numeric)) # %>% na.omit())
      criteria_names <- reactive(cleaned_criteria() %>% colnames())
      # get column names and match to metrics dictionary
      all_metrics <- reactive({
        metrics %>%
          dplyr::filter(Name %in% criteria_names()) %>% # %>% add_column(Include = TRUE,.before=1) %>%
          relocate(Subgoal, .before = 3) %>%
          add_column(
            Weight = 0,
            Indt = 0,
            PreT = 0,
            PreF = 0,
            gaussP = 0
          )
      })
      goal_metrics <- reactive(all_metrics() %>% select(c(Goal, Goal_Description, Weight)) %>% unique())
      sub_goal_metrics <- reactive(all_metrics() %>% select(c(Goal, Goal_Description, Subgoal, Subgoal_Description, Weight)) %>% unique())

      observeEvent(input$apply_at_subgoal, {
        showNotification("apply_at_subgoal", duration = 1, type = "message")})

      observeEvent(input$accept_weights, {
        showNotification("accept_weights", duration = 1, type = "message")})
      # reactive user edits -----------------------------------------------------
      output$modal_info <- renderUI({
        # tagList(
        #   HTML(paste0("This is some HTML")),
        #   radioButtons(
        #     inputId = ns("goal_tiers_select"),
        #     label = "Select tier to apply weights",
        #     choices = c("Criteria", "Subgoals"),
        #     selected = "Subgoals",
        #     inline = TRUE
        #   )
        # )
      })

      user_table <- reactive({
        if (input$apply_at_subgoal) {
          sub_goal_metrics()
        } else {
          all_metrics()
        }
      })



# Advanced Edit subgoals --------------------------------------------------


      output$sub_goal_metrics_modal <-
        renderUI({
          output$advanced_subgoal_table <- renderRHandsontable(rhandsontable(sub_goal_metrics()))
          tagList(
            rHandsontableOutput(ns("advanced_subgoal_table"))
          )
        })

      observe({
        showModal(modalDialog(easyClose = TRUE,
          size = "l", footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "OK")
          ),
          htmlOutput(ns("modal_info")), htmlOutput(ns("sub_goal_metrics_modal"))
        ))
      }) %>% bindEvent(input$apply_at_subgoal)


# Advanced edit criteria --------------------------------------------------


      output$criteria_metrics_modal <-
        renderUI({
          output$advanced_criteria_table <- renderRHandsontable(rhandsontable(all_metrics()))
          tagList(
            rHandsontableOutput(ns("advanced_criteria_table"))
          )
        })


      observe({
        showModal(modalDialog(easyClose = TRUE,
          size = "l", footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "OK")
          ),
          htmlOutput(ns("modal_info")), htmlOutput(ns("criteria_metrics_modal"))
        ))
      }) %>% bindEvent(input$apply_at_criteria)


# Get user edits ----------------------------------------------------------

    user_weights <- reactive(

      c(input$goal1, input$goal2, input$goal3, input$goal4))

      #observe(print(user_weights() %>% class()))

#print(user_weights())
    weight_sum_val <- reactive(user_weights()%>% sum())
    output$weight_sum <-
      renderUI(paste(
        "Sum of Weights: ", weight_sum_val()
      ))

    user_weights.df <- reactive(
      data.frame(
        Goal = goals$Goal,
        Name = goals$Goal_Description,
        Weight = user_weights()
      ))

    user_edits_all_metrics <- reactive(merge(metrics %>% select("Name", "Goal"), user_weights.df(), by.x = "Goal", by.y = "Goal"))


      output$criteria_debug <- renderText(user_edits_all_metrics()["Weight"] %>% class())


      output$table2 <-#renderDataTable(
        DT::renderDT(
          user_weights.df())#user_weights())
      #%>% bindEvent(user_weights())
output$table3 <- renderDT(user_edits_all_metrics())

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


      output$user_edits_all_metrics<- renderDataTable(user_edits_all_metrics())
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
        rhandsontable(DF()) # rv$metrics)
      })



      output$metrics_out <- renderDataTable(
        all_metrics()
      )

      output$criteria <- renderDataTable(cleaned_criteria(), extensions = "Buttons", options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print")
      ))
      num_basins <- reactive(cleaned_criteria() %>% nrow())
      # count number of criteria
      num_criteria <- reactive(cleaned_criteria() %>% ncol())

      output$card1 <- renderUI(card(num_basins(), "Basins Selected"))
      output$card2 <- renderUI(card(num_criteria(), "Criteria Selected"))
      output$table <- renderDataTable(filtered() %>% head())


# Get user edited weights -------------------------------------------------

      # Get mcda results --------------------------------------------------------
      mcda_results <- reactive(
        promethee_2(
          dataset = cleaned_criteria(), weighting = user_edits_all_metrics()[["Weight"]]
        )
      )

      results_to_return <- reactive(as.numeric(input$n))

      pf2_outflows <- reactive(mcda_results()[1] %>% as.data.frame() %>%
                               mutate(score = scales::rescale(score,to=c(-10,10))) %>%
                               #  mutate(subbasin_rank = rank) %>%
                               slice_max(score,n = results_to_return()) %>% sig_figs())



      output$ranked_list <- renderDT(
        pf2_outflows() #%>% mutate(score =
                                      #scales::rescale(score,to=c(0,10))) %>%
          #arrange(-score) %>%
          #sig_figs()
      )

# map
      map_shps <- reactive(
        subbasin_shps %>% merge(
          pf2_outflows() %>%
          rownames_to_column("SWSID")
          %>% select(c("SWSID","score","rank"))
        ))
      # Create a continuous palette function
      map.pal <- colorNumeric(
        palette = "Blues",domain = c(0,10))
      values <- reactive(seq.int(0, results_to_return(),length.out=6) %>% round(0))
      output$map <- renderLeaflet({



        # basins_selected = input$hot_rows_selected
        leaflet() %>%
          addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
          addProviderTiles("Esri.WorldGrayCanvas", group = "Grey") %>%
          addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
          addLayersControl(
            position = "bottomright", options = layersControlOptions(collapsed = FALSE),
            baseGroups = c("Grey", "Satellite", "Dark")
          ) %>%
          addPolygons(data = subbasin_shps,
                      fillOpacity = 0.2, opacity = 0.8,
                      color = "grey",
                      weight = 0.4

          ) %>% addLegend(position = "bottomright",title = "Scores", pal = map.pal,values = c(0,6,8,10))





        #       setView(
        #   lng = (-122.2),
        #   lat = (47.6),
        #   zoom = 7
        # )
      })

      observe({
        leafletProxy("map") %>%
          clearGroup("top_sheds") %>%
          addPolygons(
            data = map_shps(), group = "top_sheds",
            opacity = 0.6,
            color = "green",
            weight = 0.5,
            # dashArray = 1,
            fillOpacity = 0.8,
            fillColor = ~map.pal(score)
          )

      }) %>% bindEvent(pf2_outflows())

      # leafletProxy("map") %>%
      #   clearGroup("selected_sheds") %>%
      #   addPolygons(
      #     data = shps_selected(), group = "selected_sheds",
      #     opacity = 0.6,
      #     color = "green",
      #     weight = 0.5,
      #     # dashArray = 1,
      #     fillOpacity = 0.1,
      #     fillColor = "green"
      #   )


    }
  )
}

ui <- dashboardPage(
  header = dashboardHeader(), sidebar = dashboardSidebar(), body =
    dashboardBody(
      criteria_page_UI2("criteria-test")
    )
)

server <- function(input, output, session) {
  source(here::here("R", "promethee.R"))
  source(here::here("R", "fct_helpers.R"))
  load(here::here("data", "subbasin_data.rda"))
  mock_filtered <- reactive(subbasin_data %>% sample_n(100))
  criteria_page_server2("criteria-test", mock_filtered)
}


shinyApp(ui, server)
