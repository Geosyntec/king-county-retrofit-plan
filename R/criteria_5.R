

criteria_page_UI2 <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(

      # Column 1 ------------------------------------------------------------
      column(
        width = 4,

        ## box 1 -------------------------------------------------------------------
        box(
          title = "Subbasin Prioritization", status = "primary",
          # status = "black",
          solidHeader = FALSE,
          width = NULL,
          ### box 1a -------------------------------------------------------------------
          box(
            title = "Criteria Orientation", status = "primary", width = NULL, solidHeader = TRUE,
            strong("Choose Orientation:"),
            radioButtons(
              inputId = ns("orientation_select"),
              label = NULL, # "Select Orientation",
              choices = c("Restoration", "Protection"),
              selected = "Restoration",
              inline = FALSE
            ),
            make_info(
              tagList(
                "Choose whether prioritization should focus on Restoration or Protection. Restoration will prioritize areas in need of improvement, while Protection will prioritize high functioning locations.",
                br(),
                a(tagList("See the StoryMap for more info", icon("external-link-alt")),
                  href = storymap_url
                )
              )
            )
          ),
          ### box 1b -------------------------------------------------------------------

          box(
            width = NULL,
            title = "Criteria Weights", solidHeader = TRUE, status = "primary",
            helpText("Enter your preferred weight as a whole number between 0 and 5."),
            make_info("A weight of 0 indicates the metric will be ignored. Metrics with missing data are not used."),
            hr(),
            make_numeric_inputs(goals, id),

            #
            column(
              width = 12,
              actionButton(ns("reset_weights"), width = "40%", label = "Reset", icon = icon("refresh")),
              actionButton(ns("accept_weights"), width = "40%", label = "Accept", icon = icon("check"))
            )
          ),
          uiOutput(ns("count_missing")),
          ### box 1c -------------------------------------------------------------------
          box(
            width = NULL,
            title = "Advanced Settings", solidHeader = TRUE, status = "primary",
            # shinydashboardPlus::box(

            # dropdownMenu = boxDropdown(
            # boxPad(strong("Advanced Settings")),
            # boxPad(dev_pill()),
            fluidRow(column(
              width = 12,
              dev_pill(),
              # actionLink("Weight Subgoals", inputId = ns("apply_at_subgoal")),
              br(),
              # actionLink("Weight Criteria", inputId = ns("apply_at_criteria")),
              br()
              # actionLink("Edit Thresholds", inputId = ns("edit_thresholds"))
            ))
          )
        )
      ),

      # Column 2 ------------------------------------------------------------
      # Results Tabset ---------------------------------  --------------------------------
      column(
        width = 8,
        box(
          headerBorder = FALSE,
          title = "Results", status = "primary", width = NULL,
          shinyWidgets::panel(
            leafletOutput(ns("map"), height = 600)
          )
        ),
        shinydashboard::tabBox(
          width = NULL,
          tabPanel(
            width = 12, title = "Table",
            shinyWidgets::panel(
              # shinycssloaders::withSpinner(
              DTOutput(ns("mcda_results"))
            )
          ),
          tabPanel(
            width = 12,
            title = "Results by Goal",
            shinycssloaders::withSpinner(
              apexfacetOutput(ns("uc_goals"))
            )
          ),
          tabPanel(
            width = 12, title = "reports",
            strong("Scenario"),
            textOutput(ns("scenario")),
            strong("User Weights"),
            DTOutput(ns("report1")),
            strong("all metrics"),
            DTOutput(ns("report2"))
          )
        )
      )
    )
  )
}



criteria_page_server2 <- function(id, rv2) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      all_metrics <- reactive({
        metrics
      })


      goals <- metrics %>%
        select(Goal, Goal_Description) %>%
        unique() %>%
        arrange(Goal)

      # count number of basins
      cleaned_criteria <- reactive({
        # req(rv2$filtered_data)
        rv2$filtered_data %>% select_if(is.numeric)
      })



      output$count_missing <- renderUI({
        shinydashboardPlus::box(
          headerBorder = FALSE, # icon = icon("warning",style='color: black'),
          collapsed = FALSE,
          title = div(tagList((na_cols() %>% length()), "metrics have missing data"), style = "color: black"),
          closable = TRUE,
          width = NULL,
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          tagList(
            "Only metrics with available data for all selected subbasins (Pre-Screening tab) are analyzed. The following metrics have been discarded due to missing data:", br(),
            br(),
            # HTML('<ul class="list-group>'),
            HTML("<li class='list-group-item'>"),
            HTML(paste(na_cols() %>% get_pretty_names(),
              collapse = "<li class='list-group-item'>"
            )),
            # HTML('</ul>')
          )
          # actionLink(ns('view_missing'),label = 'Click to view')
        )
      })
      #
      #
      #     #card_warning(.num = missing_vals() %>% length(), .description = "Metrics with missing data")
      #
      #
      #
      #   observe(print(missing_vals()))


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
        showModal(modalDialog(
          easyClose = TRUE,
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
        showModal(modalDialog(
          easyClose = TRUE,
          size = "l", footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "OK")
          ),
          htmlOutput(ns("modal_info")), htmlOutput(ns("criteria_metrics_modal"))
        ))
      }) %>% bindEvent(input$apply_at_criteria)






      # %>% na.omit())
      criteria_names <- reactive(cleaned_criteria() %>% colnames())
      alternatives <- reactive(cleaned_criteria() %>% rownames())
      row_count <- reactive(nrow(cleaned_criteria()))

      all_metrics <- reactive({
        ## req(cleaned_criteria())

        metrics %>%
          dplyr::filter(Name %in% criteria_names())
      }) # %>% # %>% add_column(Include = TRUE,.before=1) %>%

      # Get user weights ----------------------------------------------------------


      # validator for weights

      iv <- InputValidator$new()
      iv$add_rule("goal1", sv_between(0, 5))
      iv$add_rule("goal2", sv_between(0, 5))
      iv$add_rule("goal3", sv_between(0, 5))
      iv$add_rule("goal4", sv_between(0, 5))
      iv$enable()
      user_weights.df <- reactive({
        data.frame(
          Goal = goals$Goal,
          Name = goals$Goal_Description,
          Weight = user_weights()
        )
      })

      user_edits_all_metrics <- reactive({
        merge(
          all_metrics() %>%
            select("Criteria_Name" = "Name", "Goal", "Metric_no", "orientation_protect", "orientation_restore", "Indifference_Threshold_Percentage"),
          user_weights.df(),
          by.x = "Goal", by.y = "Goal"
        )
      })

      # Get user min max

      # Clean user inputs for mcda ----------------------------------------------
      cleaned_list <- reactive(remove_nas_from_pt(pt.df = cleaned_criteria()))
      cleaned_pt <- reactive(cleaned_list()[["cleaned_pt"]])
      na_cols <- reactive(cleaned_list()[["na_cols"]])

      cleaned_user_table <- reactive({
        clean_mcda_inputs(
          ex_user_metrics = user_edits_all_metrics(),
          na_cols = na_cols()
        )
      })




      # Get user edited weights -------------------------------------------------
      min_max.df <- reactive({
        if (isTRUE(input$orientation_select == "Protection")) {
          data.frame(min_max = cleaned_user_table()[["orientation_protect"]])
        } else {
          data.frame(min_max = cleaned_user_table()[["orientation_restore"]])
        }
      })



      user_weights <- reactive(
        c(input$goal1, input$goal2, input$goal3, input$goal4)
      )

      observe(print(user_weights()))






      min_max.vec <- reactive({
        if (input$orientation_select == "Protection") {
          cleaned_user_table()[["orientation_protect"]] # %>%
          # "names<-"(criteria_names() %>%
          # select(-all_of(removed_cols()))
        } else {
          cleaned_user_table()[["orientation_restore"]] # %>%
          # "names<-"(criteria_names()) %>%
          #      select(-all_of(removed_cols()))
        }
      })

      # cleaned_pt_output <- reactive(remove_nas_from_pt(pt.df = cleaned_criteria()
      #                                                  ,metrics_to_remove = zero.weights()))
      # cleaned_pt <- reactive(cleaned_pt_output()[["cleaned_pt"]])
      # weights_with_nas <- reactive(
      #   cleaned_pt_output()[["removed_cols"]])

      # final_weights <- reactive(cleaned_user_metrics()[["Weight"]])


      # Calc Weighted Sum -------------------------------------------------------

      weights_oriented <- reactive({
        orient_weights(
          minmax = min_max.vec(),
          weights = cleaned_user_table()[["Weight"]]
        )
      })
      # to do
      # start here
      top_PT <- reactive({
        # req(ncol(cleaned_user_metrics() > 0 ))
        # req(cleaned_criteria())
        # req(input$n)
        scaled_weighted_sum(
          performanceTable = cleaned_pt(),
          weights = weights_oriented()
        ) # ,num_to_return = input$n)
      })




      # Get mcda vals -----------------------------------------------------------

      output$top_basins <- renderDT(
        datatable(

          top_PT() %>% sig_figs(3),
          colnames = colnames(top_PT()) %>% get_pretty_names(),
          extensions = c("Buttons", "FixedColumns"),
          options = list(
            scrollX = TRUE,
            fixedColumns = TRUE,
            dom = "tiplrB",
            buttons = c("copy", "csv", "excel")
          )
        )
      )

      output$user_edits_all_metrics <- renderDT(datatable(
        user_edits_all_metrics(),
        extensions = c("Buttons", "FixedColumns"),
      ))
      output$min_max.vec <- renderDT(min_max.vec() %>% as.data.frame())




      mcda_results <- reactive({
        return(
          promethee_2(
            dataset = top_PT() %>% dplyr::select(-weighted_sum_score),
            weighting = cleaned_user_table()[["Weight"]], #
            minmax = min_max.vec(),
            IndT = cleaned_user_table()[["Indifference_abs"]],
            limit = return_vals
          )
        )
      }) %>% bindEvent(input$accept_weights, ignoreInit = TRUE)
      # observe(print(mcda_results()))

      # outputs -----------------------------------------------------------------
      output$count_items <- renderUI({
        card(.num = rv2$filtered_data %>% nrow(), .description = "Total Subbasins")
      })

      ## tables ------------------------------------------------------------------
      ## reporting


      out_tables <- reactive(
        # metrics %>%
        # select(c(Metric_no, Name, Goal, Subgoal, Goal_Description,
        #       Subgoal_Description)) %>%
        # add_column(
        user_edits_all_metrics()
        # cleaned_user_table()[["Weight"]] %>% as.data.frame()
      )


      # report1 -----------------------------------------------------------------

      output$scenario <- renderText(input$orientation_select)
      output$report1 <- renderDT(user_weights.df())
      output$report2 <- renderDT(mcda_results()[["UnicriterionNetFlows"]])

      # table showing final scores
      output$mcda_results <- renderDT(
        datatable(
          mcda_results()[["out_flows"]] %>%
            rownames_to_column("SWSID") %>%
            relocate(score_rank, .before = 1),
          rownames = FALSE,
          colnames = c("Rank", "SWSID", "ϕ+", "ϕ-", "Score"),
          extensions = "Buttons"
        ) %>% formatStyle("score",
          background = styleColorBar(c(-1, 1), "#81A1C1", angle = -90),
          backgroundSize = "98% 88%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
      )
      # reactable table of the same with linked map
      # A SpatialPointsDataFrame for the map.
      # Set a group name to share data points with the table.



      # Reactable table ---------------------------------------------------------


      # Render a bar chart in the background of the cell
      bar_style <- function(width = 1, fill = "#e6e6e6", height = "75%",
                            align = c("left", "right"), color = NULL) {
        align <- match.arg(align)
        if (align == "left") {
          position <- paste0(width * 100, "%")
          image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
        } else {
          position <- paste0(100 - width * 100, "%")
          image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
        }
        list(
          backgroundImage = image,
          backgroundSize = paste("100%", height),
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center",
          color = color
        )
      }





      output$tbl2 <- renderReactable(
        reactable(
          mcda_results()[["out_flows"]] %>%
            rownames_to_column("SWSID") %>%
            relocate(score_rank, .before = 1),
          columns = list(
            score = colDef(
              style = function(value) {
                bar_style(width = value / 1, fill = "#2c5e77", color = "#fff")
              },
              align = "left",
              format = colFormat(digits = 1)
            )
          ),
          bordered = TRUE
        )
      )

      # top items to return
      return_vals <- 9

      pf2_outflows <- reactive(mcda_results()[["out_flows"]])



      # output$table <- renderDT(rv2$filtered_data)
      ## map ------------------------------------------------------------------

      top_shps <- reactive(
        subbasin_shps %>% merge(pf2_outflows() %>% rownames_to_column("SWSID"))
      )
      # Create a continuous palette function

      values <- reactive(pf2_outflows()[["score"]])

      map.pal.fun <- reactive((colorBin(king_co_palette, values(), 5, pretty = TRUE, reverse = FALSE)))

      pal.rev.fun <- reactive((colorBin(king_co_palette, values(), 5, pretty = TRUE, reverse = TRUE)))

      output$map <-
        # req(reactive(rv2$filtered_shps))
        # basins_selected = input$hot_rows_selected
        renderLeaflet({
          basemap %>%
            addLayersControl(
              position = "bottomright", options = layersControlOptions(collapsed = FALSE), baseGroups = c("Base", "Satellite", "Grey"),
              overlayGroups = c("City Limits")
            ) %>%
            addPolygons(
              color = "#28a745",
              data = rv2$filtered_shps,
              fillOpacity = 0.2, opacity = 0.8,
              fillColor = "grey",
              weight = 0.4
            ) %>%
            addPolygons(data = cities_shp, group = "City Limits") %>%
            hideGroup("City Limits")
        })



      observe({
        map.pal <- (colorBin(king_co_palette, values(), 5, pretty = TRUE, reverse = FALSE))

        pal.rev <- (colorBin(king_co_palette, values(), 5, pretty = TRUE, reverse = TRUE))
print(pal.rev)
        leafletProxy("map") %>%
          clearGroup("top_sheds") %>%
          leaflet::removeControl("legend") %>%
          addPolygons(
            data = top_shps(), group = "top_sheds",
            label = ~score_rank,
            labelOptions = labelOptions(
              noHide = TRUE, textOnly = TRUE,
              permanent = TRUE,
              style = list(
                color = "black",
                textShadow = "0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white, 0 0 4px white"
              )
            ),
            weight = 1,
            color = "white",
            opacity = 1,
            fillColor = ~ map.pal(score),
            # weight = 0.5,
            # dashArray = 1,

            fillOpacity = 0.75,
            popup = ~ paste(
              strong("Subbasin:"), SWSID, "<br>", strong("Score:"), score,
              "<br>", strong("Rank:"), score_rank
            ),
            highlightOptions = highlightOptions(weight = 5, bringToFront = T, opacity = 1)
          ) %>%
          addLegend(
            layerId = "legend", pal = pal.rev, values = values(), opacity = 0.7, title = "Score",
            position = "bottomleft",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
          )
      }) %>% bindEvent(input$accept_weights, ignoreInit = TRUE)


      # Charts ------------------------------------------------------------------

      # Unicriterion Net flow Chart -----------------------------------------------------------
      uc.df <- reactive(mcda_results()[["UnicriterionNetFlows"]] %>%
        rownames_to_column("SWSID"))
      observe(print(uc.df()))
      output$uc_goals <-
        renderApexfacet(
          uc_goal_chart(uc.df())
        )
      # output$scatter_chart <- renderEcharts4r(mcda_scatter(mcda_results()[["out_flows"]]))


      # heatmap -----------------------------------------------------------------

      adj_table <- reactive(adjacency_matrix(
        out_flows = mcda_results()[["out_flows"]],
        basins == mcda_results()[["out_flows"]] %>% rownames()
      ))
      df2 <- reactive(adj_table() %>% as.data.frame() %>% rownames_to_column("SWSID") %>% pivot_longer(-c(SWSID)))

      # TODO

      # return a list of items

      # final ranking table

      # weighted sum table

      # user weights

      # preference table
    }
  )
}

# library(shiny)
# source("~/Documents/repos/king-county-retrofit-plan/R/aaa_global.R")
ui <- dashboardPage(
  header = dashboardHeader(), sidebar = dashboardSidebar(), body =
    dashboardBody(
      use_theme(kingco_theme),
      criteria_page_UI2("test")
    )
)

test_basins <- subbasin_data %>%
  select_if(is.numeric) %>%
  mutate(BIBI = 0) %>%
  mutate(Sidewalk_Density = 0)

# test_basins[which(is.na(test_basins %>% colSums()))] <- 0

server <- function(input, output, session) {
  rv2 <- reactiveValues(
    # base_data = test_basins,
    filtered_data = subbasin_data, # %>% na.omit(),
    filtered_shps = subbasin_shps
  )
  criteria_page_server2("test", rv2)
}

shinyApp(ui, server)
