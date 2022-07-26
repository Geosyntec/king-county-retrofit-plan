library(rhandsontable)
library(htmlwidgets)
library(visNetwork)
library(tidyr)
library(dplyr)
load("~/Documents/repos/king-county-retrofit-plan/data/test_data.rda")
#source("~/kingCoDecision/R/fct_helpers.R")
base_data <- test_data %>% select(c(10:36)) %>% head(100) %>% select(-c(MedianPavementCondition,starts_with('WQBE'))) %>%
  na.omit()
#base_data <- cars_example

criteria <- colnames(base_data)

library(shinydashboard)

# sidebar <- dashboardSidebar(
#   sidebarMenu(
#     # Setting id makes input$tabs give the tabName of currently-selected tab
#     id = "tabs",
#     menuItem("1. Filter Criteria", tabName = "filter", icon = icon("dashboard")),
#     menuItem("2. Prioritize subwatersheds", icon = icon("th"), tabName = "Prioritize", badgeLabel = "new",
#              badgeColor = "green"),
#
#
#   )
# )

# body <- dashboardBody(
#   tabItems(
#     tabIte"filter")
# )


ui <- dashboardPage(header = dashboardHeader(),sidebar = dashboardSidebar(), body = dashboardBody(

  #bulmaSection(



#ui <- bslib::page_fluid(theme = bslib::bs_theme(version = 5,bootswatch = "materia"),
#  titlePanel("beta"),
#

  #   sidebarPanel(
  #     helpText("Handsontable demo output. Column add/delete does work ",
  #              "for tables with defined column properties, including type."),
  #     radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
  #   ),
    #mainPanel(

          fluidRow(
        box(width = 4,
      h2("Summary Table"),
        wellPanel(
      rHandsontableOutput("summary_table")),
      #box(width = 6,
        h2("Input Data"),
      wellPanel(
      rHandsontableOutput("hot"))),
      box(width = 8,title = 'network diagram',height='1200px',
  visNetworkOutput("pflows",height="1000px"))),
  #box(width = 12, fluidRow(verbatimTextOutput('table')))
    #)
  )

  #))
)


server <- (function(input, output, session) {
  # this caching step is no longer necessary
  # it was left as an example

  stats_table <- make_summary_table(base_data)

  output$summary_table <- renderRHandsontable(
    rhandsontable(
    stats_table)      %>%
      hot_col("chart", readOnly= TRUE, halign = "htCenter", renderer = htmlwidgets::JS("renderSparkline")) %>%
    hot_col('means',readOnly= TRUE,halign = "htRight") %>%
    hot_col('mins',readOnly= TRUE,halign = "htRight") %>%
    hot_col('sdevs',readOnly= TRUE,halign = "htRight") %>%
    hot_col('maxs',readOnly= TRUE,halign = "htRight"))


  values = reactiveValues()
  df0 <- data.frame(
    Criteria = criteria,
    Selected = rep(TRUE,length(criteria)),
    MinMax = "max",
    weight = 0
    )
  data = reactive({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF = df0
      else
        DF = values[["DF"]]
    }


    values[["DF"]] = DF
    DF
  })

  output$hot <- renderRHandsontable({
    DF = data()
    if (!is.null(DF))
      rhandsontable(DF,useTypes = TRUE,rowHeaders = FALSE) %>%
      #  hot_col("chart", readOnly= TRUE, halign = "htCenter", renderer = htmlwidgets::JS("renderSparkline")) %>%
      # hot_col('means',readOnly= TRUE,halign = "htRight") %>%
      # hot_col('mins',readOnly= TRUE,halign = "htRight") %>%
      # hot_col('sdevs',readOnly= TRUE,halign = "htRight") %>%
      # hot_col('maxs',readOnly= TRUE,halign = "htRight") %>%
     hot_col(col = "MinMax",halign = "htCenter", type = "dropdown", strict = TRUE, source = c("min","max")) %>%
      hot_col('weight',format = "0",type = "numeric",strict = TRUE, source = seq(0,10)) %>%
       hot_validate_numeric(col = "weight", min = 0, max = 10) %>%
       hot_validate_character(col = "MinMax", choice = c("min","max"))
  })


  mcda_output <- reactive({
    promethee_2(dataset = base_data,weighting = values$DF$weight,minmax = values$DF$MinMax)
  })
  output$table <-

    renderPrint(mcda_output())#values$DF$MinMax)
  output$pflows <- renderVisNetwork(

    plot_jittered_pref_flows(tolerance = 0.1, pf2 = mcda_output()[[1]],adj_mat_numeric = mcda_output()[[2]]))
})

shinyApp(ui,server)
