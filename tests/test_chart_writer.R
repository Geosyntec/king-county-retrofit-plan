# The evaluation table
library(PROMETHEE)
library(tidyverse)

source(here::here("R", "aaa_global.R"))
#
make_test_pf <- function(num = 9) {
  dataset <-
    subbasin_data %>%
    select_if(is.numeric) %>%
    na.omit() %>%
    head(num)

  basins <- rownames(dataset)
  criteria <- colnames(dataset)
  n <- length(basins)
  c <- length(criteria)

  IndT <- runif(c, 0, 0.02) %>% make.matrix(n)

  weighting <- sample(1:10, c, replace = TRUE) %>% make.matrix(n)

  minmax <- rep("min", c) %>% make.matrix(n)

  PreT <- runif(c, 0, 0) %>% make.matrix(n)

  PreF <- rep("V-shape", c) %>% make.matrix(n)

  gaussP <- runif(c, 0, 0) %>% make.matrix(n)


  PF <- promethee_2(
    dataset = dataset,
    weighting = weighting,
    minmax = minmax,
    IndT = IndT,
    PreT = PreT,
    PreF = PreF,
    gaussP = gaussP
  )
  return(PF)
}


PF <- make_test_pf()
aa <- PF[['out_flows']] %>% rownames()
out_tables <- function() {
  metrics %>%
    select(Metric_no, Name, Goal, Subgoal, Goal_Description, Subgoal_Description) %>%
    add_column(weighting[1, ])
}
tables <- PF
my_workbook <- createWorkbook()

addWorksheet(wb=my_workbook,
             sheetName = "Summary"
             )

writeData(wb = my_workbook,sheet = 1,
          x = c("Subbasin Prioritization Report",
                "King County Stormwater Retrofit Planner",
                "https://geosyn.shinyapps.io/king-county-retrofit-plan/",
                paste("Report Date:",Sys.Date() %>% as.character())

                )
          )



#styles for first page
style1 <- createStyle(border = "TopBottom", borderColour = "#4F81BD",fontSize = 18,
                         textDecoration = 'bold')
addStyle(my_workbook, sheet = 1, style1, rows = 1, cols = 1:4)

for (i in 1:4) {


addWorksheet(
  wb = my_workbook,
  sheetName = paste0('sheet_',i+1)
)

writeDataTable(
               wb = my_workbook,
               rowNames = TRUE,
               x = tables[[i]] %>% as.data.frame(),
          sheet = i+1
)}
saveWorkbook(my_workbook,'f.xlsx',overwrite = TRUE)
