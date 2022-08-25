library(shinydashboardPlus)
source(here::here("R", "aaa_global.R"))

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
                                         boxPad(dev_pill()),

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
                actionButton(ns("accept_weights"),width = "40%", label = "Accept", icon = icon("check")),
               dev_pill("Reset in dev")
               )



)
    ),
    # column to hold parameters
    column(
      width = 8,
      tabBox(id = ns("tabset1"),width = 12,
             tabPanel("Results",
                      fluidRow(
                      column(width = 12,
      shinydashboardPlus::box(
        width = 12,
      fluidRow(column(width = 4, uiOutput(ns("count_items"))),
               column(width = 4,
        shiny::selectInput(ns("n"),selected = 25, multiple = FALSE, label = "Select number of results to return:",
                           choices = c(10,25,50,100)
                           ))),
        shinycssloaders::withSpinner(leafletOutput(ns("map")))
        ),
        shinydashboardPlus::box(
          width = 12,
          "table",
          # dataTableOutput(ns('metrics_out'))
          shinycssloaders::withSpinner(DTOutput(ns('ranked_list'))))
        ))),
      tabPanel("debug info",
        DTOutput(ns("table2")),
        verbatimTextOutput(ns("criteria_debug")),
        "user edits:",
        DTOutput(ns("table3"))

      ),
      tabPanel("min_max",
               strong("Table 4"),
               DTOutput(ns("table4"))
               )

      ),

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
      row_count <- reactive(nrow(filtered()))

      output$count_items <- renderUI({card(.num = row_count(), .description = "Total Subbasins")})
      #check for nas

      #shapes
      shps_selected <- reactive({
        req(filtered())
        subbasin_shps[which(subbasin_shps$SWSID %in% (filtered() %>% rownames())),]
      })
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


# modal for table edits ---------------------------------------------------


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
          output$advanced_criteria_table <- renderRHandsontable(rhandsontable(user_edits_all_metrics()))
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

    user_edits_all_metrics <- reactive(merge(metrics %>% select("Name", "Goal","Metric_no", "orientation_protect", "orientation_restore" ), user_weights.df(), by.x = "Goal", by.y = "Goal"))


      output$criteria_debug <- renderText(user_edits_all_metrics()["Weight"] %>% class())


      output$table2 <-#renderDataTable(
        DT::renderDT(
          user_weights.df())#user_weights())
      #%>% bindEvent(user_weights())
output$table3 <- renderDT(user_edits_all_metrics())
output$table4 <- renderDT(min_max.df())
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

      observe(print(input$orientation_select == "Protection"))
# Get user edited weights -------------------------------------------------
      min_max.df <- reactive({
        if(input$orientation_select == "Protection"){
               data.frame(min_max = metrics$orientation_protect)}else{
               data.frame(min_max = metrics$orientation_restore)}
               #pull(metrics, orientation_protect),pull(metrics, orientation_restore)) %>% as.data.frame()
        #ifelse(input$orientation_select == "Protection",mtcars,iris)
                             #user_edits_all_metrics() %>% select(c("Metric_no", "orientation_protect")),
                             #user_edits_all_metrics() %>% select(c("Metric_no", "orientation_protect")))
      })

      min_max <- reactive({min_max.df()[["min_max"]]})
      #observe(print(min_max() %>% ))
      # Get mcda results --------------------------------------------------------
      mcda_results <- reactive(
        promethee_2(
          dataset = cleaned_criteria(), weighting = user_edits_all_metrics()[["Weight"]], minmax = min_max()#, limit = results_to_return()
        )
      ) %>% bindEvent(input$accept_weights,ignoreInit = TRUE)

      results_to_return <- reactive(as.numeric(input$n))

      # radioButtons(
      #   inputId = ns("orientation_select"),
      #   label = NULL, #"Select Orientation",
      #   choices = c("Restoration", "Protection"),
      #   selected = "Restoration",
      #   inline = FALSE



      pf2_outflows <- reactive(mcda_results()[1] %>% as.data.frame() %>%
                               mutate(score = scales::rescale(score,to=c(-10,10))) %>%
                               #  mutate(subbasin_rank = rank) %>%
                               slice_max(score,n = results_to_return()) %>% sig_figs())



      output$ranked_list <- renderDT({DT::datatable(

           #req(pf2_outflows())


        pf2_outflows(),options = list(paging = FALSE))
           #%>% mutate(score =
                                      #scales::rescale(score,to=c(0,10))) %>%
          #arrange(-score) %>%
          #sig_figs()
      })

# map
      top_ids <- reactive(rownames(pf2_outflows()))


      top_shps <- reactive(
        shps_selected() %>% merge(pf2_outflows() %>% rownames_to_column("SWSID"))
        #shps_selected() %>% dplyr::filter(SWSID %in% top_ids())
        #shps_selected()[which(shps_selected()$SWSID %in% (top_ids())),]
        )
      # Create a continuous palette function

      values <- reactive(pf2_outflows()[["score"]])
      observe(print(values() %>% unlist()))


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
          addPolygons(data = shps_selected(),
                      fillOpacity = 0.2, opacity = 0.8,
                      color = "grey",
                      weight = 0.4

          )





        #       setView(
        #   lng = (-122.2),
        #   lat = (47.6),
        #   zoom = 7
        # )
      })

      observe({
        map.pal <- (colorBin("viridis", values(), 6, pretty = TRUE,reverse = FALSE))

        labels <- paste(sep = "<br/>",
             "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
             "606 5th Ave. S",
             "Seattle, WA 98138"
        )
        pal.rev <- (colorBin("viridis", values(), 6, pretty = TRUE,reverse = TRUE))
        leafletProxy("map") %>%
          clearGroup("top_sheds") %>%
          leaflet::removeControl("legend") %>%
          addPolygons(
            data = top_shps(), group = "top_sheds",
            weight = 1,
            color = "white",
            opacity = 1,
            fillColor  = ~map.pal(score),
            #weight = 0.5,
            # dashArray = 1,

            fillOpacity = 0.75,
            popup = ~ paste(
              strong("Subbasin:"), SWSID,"<br>", strong("Score:"), score,
              "<br>", strong("Rank:"), score_rank
              ),
            highlightOptions = highlightOptions( weight = 5, bringToFront = T, opacity = 1)

          ) %>% addLegend(layerId = "legend", pal = pal.rev, values = values(), opacity = 0.7, title = "Score",
                          position = "bottomleft",
                          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))

      }) %>% bindEvent(input$accept_weights, ignoreInit = TRUE)

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
  mock_filtered <- reactive(subbasin_data %>% filter(WQBE_basin ==    "White")) #%>% sample_n(100))
  criteria_page_server2("criteria-test", mock_filtered)
}


shinyApp(ui, server)
