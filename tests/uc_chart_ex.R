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

DT::datatable(out_tables())

basins <- rownames(PF$out_flows)
df <- PF$UnicriterionNetFlows %>%
  `colnames<-`(c(criteria, "score_rank", "score")) %>%
  `rownames<-`(basins) %>%
  as.data.frame() %>%
  rownames_to_column("SWSID") %>%
  mutate(SWSID = SWSID %>% as.factor())


df.long <- df %>%
  pivot_longer(-SWSID, names_to = "Name") %>%
  left_join(metrics %>% select(
    Name, Goal, Subgoal
  ))


uc.goals <- df.long %>%
  group_by(SWSID, Goal) %>%
  summarise(value = sum(value))


apex(uc.goals,
  type = "bar",
  mapping = aes(x = Goal, y = value)
) %>%
  ax_chart(
    toolbar = list(show = FALSE)
  )

a_chart <-
  apex(
    uc.goals,
    type = "bar",
    mapping = aes(x = Goal, y = value),
    toolbar = list(show = FALSE)
    # height = 400,
  ) %>%
  ax_plotOptions(
    bar = bar_opts(
      horizontal = TRUE,
      # borderRadius = 4,
      barHeight = "60%",
      columnWidth = "60%",
      dataLabels = list(position = "middle"),
      distributed = TRUE
    )
  ) %>%
  ax_xaxis(
    type = "categories"
    # min=-1,max=1,tickAmount = 4,
    # categories = c("Goal 1",'Goal 2','Goal 3','Goal 4')
  ) %>%
  add_vline(0, color = "#999", dash = 0) %>%
  ax_dataLabels(
    enabled = TRUE,
    textAnchor = "start",
    style = list(fontSize = "9px", fontWeight = 300, colors = c("#555"))
  ) %>%
  ax_facet_wrap(
    vars(basin_name),
    scales = "fixed", ncol = 3, chart_height = "150px"
  )


merge(
  all_metrics() %>%
    select("Criteria_Name" = "Name", "Goal", "Metric_no", "orientation_protect", "orientation_restore", "Indifference_Threshold_Percentage"),
  user_weights.df(),
  by.x = "Goal", by.y = "Goal"
)
