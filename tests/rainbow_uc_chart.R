library(PROMETHEE)
library(tidyverse)
library(highcharter)
source(here::here('dev','dev_global.R'))
make_pf = function(dataset){
basins <- rownames(dataset)
criteria <- colnames(dataset)
n <- length(basins)
c <- length(criteria)
IndT <- runif(c, 0, 0.02) %>% make.matrix(n)
weighting <- sample(1:10, c,replace = TRUE) %>% make.matrix(n)
minmax <- rep("min", c)%>% make.matrix(n)
PreT <- runif(c, 0, 0) %>% make.matrix(n)
PreF <- rep("V-shape", c)%>%make.matrix(n)
gaussP <- runif(c, 0, 0)%>% make.matrix(n)
PF = promethee_2(dataset = dataset, weighting = weighting, minmax = minmax, IndT = IndT, PreT = PreT, PreF = PreF,gaussP = gaussP,limit=9)
}

dataset <- subbasin_data  %>%
  select_if(is.numeric) %>% na.omit() %>%
  head(21)

PF = make_pf(dataset)





uc <- PF$UnicriterionNetFlows |>
  rownames_to_column('SWSID')
uc <- (PF[['UnicriterionNetFlows']] |>
                    rownames_to_column("SWSID"))
ch <-
  uc_goal_chart(uc)

uc.long |>  group_by(Goal) |> mutate(tot = sum(value)) |> View()


bars <- count(mpg, class) %>%

  apex(mapping = aes(class, pct), type = "column")

ch2 <- uc.long |> filter(score_rank == 1) |>
  apex(mapping = aes(x=Goal,y=value))
ui <-
  shinydashboardPlus::dashboardPage(
    header =dashboardHeader() , sidebar = dashboardSidebar(), body =dashboardBody(


 fluidRow(
   box(width = 12,

  apexfacetOutput('a')))
))
server <- function(input, output, session) {
  output$a <- renderApexfacet(ch |> ax_legend(show=FALSE))
}

shinyApp(ui, server)

