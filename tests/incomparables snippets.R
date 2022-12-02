# The evaluation table
library(PROMETHEE)
library(tidyverse)
library(reactable)
library(parallelPlot)
source(here::here('dev','dev_global.R'))
#

dataset <- subbasin_data  %>% select_if(is.numeric) %>% na.omit() %>%  head(10)

basins <- rownames(dataset)
criteria <- colnames(dataset)
n <- length(basins)
c <- length(criteria)

#IndT <-




IndT <- runif(c, 0, 0.02) %>% make.matrix(n)

weighting <- sample(1:10, c,replace = TRUE) %>% make.matrix(n)


minmax <- rep("min", c)%>% make.matrix(n)

PreT <- runif(c, 0, 0) %>% make.matrix(n)


PreF <- rep("V-shape", c)%>%make.matrix(n)


gaussP <- runif(c, 0, 0)%>% make.matrix(n)

nest_vec <- function(data, value, ...){
  value <- enquo(value)
  value_name <- quo_name(value)
  groups <- enquos(...)

  output <- data %>%
    group_by(!!!groups) %>%
    summarize(value_name := list(!!value))

  return(output)
}
PF = promethee_2(dataset = dataset %>% top_n(12), weighting = weighting, minmax = minmax, IndT = IndT, PreT = PreT, PreF = PreF,gaussP = gaussP)
#order by phi_plus
incomparables <- PF$out_flows %>%
  adjacency_matrix() %>%
  as.data.frame() %>%
  rownames_to_column('SWSID') %>%
  pivot_longer(cols = -SWSID,names_to = 'incomparable') %>%
  filter(value == 'R') %>%
  select(-value)
incomparables.values <-
incomparables %>%
  #select(incomparable) %>%
  left_join(PF$out_flows %>% rownames_to_column('SWSID'),by= c("incomparable"="SWSID"),keep = FALSE)
df <- PF$out_flows %>%
  rownames_to_column("SWSID") %>%
  right_join(suffix = c(".SWSID",".incomparable"), incomparables.values, by = c('SWSID'='SWSID'))
#order by phi_minus
#df1 <-
  df %>%
  select(c(score_rank.SWSID, SWSID, incomparable, starts_with(c(
    'phi_plus','phi_minus')))) %>%
  arrange(score_rank.SWSID) %>%
    select(-score_rank.SWSID) %>%
  reactable(groupBy = 'SWSID')

library(tidyr)

sales_by_mfr <- group_by(df, SWSID) %>%
  summarize(others = n())

reactable(
  sales_by_mfr,
  details = function(index) {
    sales <- filter(df, SWSID == sales_by_mfr$SWSID[index]) %>% select(-SWSID)
    tbl <- reactable(sales, outlined = TRUE, highlight = TRUE, fullWidth = FALSE)
    htmltools::div(style = list(margin = "12px 45px"), tbl)
  },
  onClick = "expand",
  rowStyle = list(cursor = "pointer")
)
df3 <- PF$out_flows %>% select(c(phi_plus,phi_minus))


parallelPlot(df3, invertedAxes=list(FALSE,TRUE),
             histoVisibility=list(FALSE,FALSE),
             cutoffs = list(
               list(c(
                 min(df3),max(df3))),
               list(c(
                 min(df3),max(df3))))) #%>% crosstalk::bscols()

