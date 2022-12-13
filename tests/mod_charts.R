# load data
source(here::here("dev", "dev_global.R"))
require(echarts4r)
require(PROMETHEE)
require(tibble)
require(tidyr)
dataset <- subbasin_data %>%
  na.omit() %>%
  select_if(is.numeric) %>%
  head(10) %>%
  select(-1)

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


dataset <- dataset %>%
  select_if(is.numeric) %>%
  na.omit() %>%
  as.matrix()
PF <- promethee_2(dataset = dataset, weighting = weighting, minmax = minmax, IndT = IndT, PreT = PreT, PreF = PreF, gaussP = gaussP)

# Multiply Weights by Unicriterion Net Flows
UnicriterionNetFlows <- PF$UnicriterionNetFlows %>% `rownames<-`(basins)
Weights <- weighting

# summarize by goal


unet.df <- (UnicriterionNetFlows * Weights) %>%
  as.data.frame() %>%
  `colnames<-`(criteria) %>%
  add_column(SWSID = basins)

unet.long <- unet.df %>%
  pivot_longer(cols = -c(SWSID), names_to = "Name") %>%
  merge((metrics %>% select(Name, Goal, Subgoal)), by.y = "Name", by.x = "Name")





rainbow.goals <- unet.df %>%
  group_by(Goal, Subgoal, SWSID, score) %>%
  dplyr::summarise(pos = sum(pos), neg = sum(neg), total = sum(value)) %>%
  select(c(Goal, Subgoal, SWSID, pos, neg)) %>%
  mutate(Goal = paste("Goal", Goal)) %>%
  pivot_longer(cols = where(is.numeric))

apex(
  data = unet.long %>% group_by(SWSID),
  type = "column",
  mapping = aes(x = Subgoal, fill = Name, y = value)
) %>%
  ax_facet_wrap(vars(SWSID), ncol = 3) %>%
  ax_chart(
    stacked = TRUE
  )




merged.df <- rainbow.goals %>% merge(basin.scores)
# rainbow.subgoals <- rainbow.df %>% group_by(Subgoal,SWSID, Goal) %>%
#   dplyr::summarise(pos = sum(pos),neg=sum(neg),total = sum(value)) %>% arrange(desc(total))#%>% arrange(total)

# rainbow.goals %>% arrange(SWSID)

merged.df %>%
  group_by(Goal) %>%
  arrange((score)) %>%
  e_charts(SWSID) %>%
  e_bar(pos, stack = "stack2") %>%
  e_bar(neg, stack = "stack1") %>%
  e_flip_coords() %>%
  e_labels(position = "inside", fontSize = 9, formatter = "{a} ") %>%
  e_tooltip() %>%
  e_x_axis(
    axisLine =
      list(symbol = "arrow", show = TRUE),
    axisLabel = list(show = FALSE)
  ) %>%
  e_toolbox_feature("dataView")

#
# rainbow.goals  %>% group_by(Goal) %>%
#   e_charts(SWSID,name = "subbasin")  %>%
#   #e_bar(total,stack = "total") %>%
#   e_bar(total,stack = "pos") %>%
#   #e_bar(score,stack = "score") %>%
#  echarts4r::e_flip_coords() %>%
# #  e_bar()
#   #e_bar(neg,stack = "neg") %>%
# #  e_bar(pos,stack = "pos")  %>%
#  # e_bar(neg,stack = "neg") %>%
#   #e_axis(inverse = TRUE) %>%
#   e_labels(position  ='inside',fontSize = 9,formatter = '{a} ') %>%
#   e_tooltip() %>% e_x_axis(axisLine =
#                              list(symbol = 'arrow', show = TRUE
#                                   ),
#                                   axisLabel = list(show=FALSE)) %>% e_toolbox_feature("dataView")
#
#
#
# transform the format
data_long <- PF$out_flows %>%
  dplyr::select(c(phi_minus, phi_plus, score)) %>%
  mutate(phi_minus = phi_minus * -1) %>%
  rownames_to_column("subbasin") %>%
  arrange(desc(score)) %>% # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(subbasin = factor(subbasin, levels = subbasin)) %>%
  pivot_longer(cols = -c(subbasin, score))

data_long %>% ggplot(aes(fill = name, y = value, x = subbasin)) +
  geom_bar(stat = "identity", position = "stack")
