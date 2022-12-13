# The evaluation table
library(PROMETHEE)
library(tidyverse)
library(highcharter)
source(here::here("dev", "dev_global.R"))
#

dataset <- subbasin_data %>%
  select_if(is.numeric) %>%
  na.omit() %>%
  head(100)

basins <- rownames(dataset)
criteria <- colnames(dataset)
n <- length(basins)
c <- length(criteria)

# IndT <-




IndT <- runif(c, 0, 0.02) %>% make.matrix(n)

weighting <- sample(1:10, c, replace = TRUE) %>% make.matrix(n)


minmax <- rep("min", c) %>% make.matrix(n)

PreT <- runif(c, 0, 0) %>% make.matrix(n)


PreF <- rep("V-shape", c) %>% make.matrix(n)


gaussP <- runif(c, 0, 0) %>% make.matrix(n)


PF <- promethee_2(dataset = dataset %>% top_n(12), weighting = weighting, minmax = minmax, IndT = IndT, PreT = PreT, PreF = PreF, gaussP = gaussP)

PF$PROMETHEE1

df <- dataset %>%
  sample_n(10) %>%
  select(1:5)
df2 <- subbasin_data %>% select(1:5)

df %>% merge(subbasin_data %>% select(WQBE_basin), by = 0)
dataset %>%
  select(1:4) %>%
  top_n(12)

# adj_table <- adjacency_matrix(out_flows = PF$out_flows,basins=PF$out_flows %>% rownames())
# df2 <- adj_table %>% as.data.frame() %>% rownames_to_column("SWSID") %>% pivot_longer(-c(SWSID))
#
# df2 %>%
#   e_charts(SWSID) %>%
# e_heatmap(y = name,z=value) %>%
#   e_visual_map(value)
#
# df45 <- data.frame(phi_plus=c(0,5),phi_minus=c(0,5))
#
# PF$out_flows %>%
#   e_charts(phi_plus) %>%
#   e_scatter(phi_minus) %>%
#   e_mark_line(data = df45)
#
# plot_jittered_pref_flows(pf2 =
# PF$out_flows %>% as.data.frame(), adj_mat_numeric = adj_table)
#
#
#
# ###############################
#
# PCA_UNIC <- prcomp(PF$UnicriterionNetFlows,center = TRUE)
# GAIA=predict(PCA_UNIC)[,1:2]
# rownames(GAIA)=basins
#
# gaia.df <- GAIA %>% merge(PF$out_flows,by=0)
# print(gaia.df)
# plot(GAIA)
# GAIA
#
# PCA_UNIC <- prcomp(PF$UnicriterionNetFlows,center = TRUE,scale. = TRUE)
# GAIA=predict(PCA_UNIC)[,1:2]
# rownames(GAIA)=basins
# df <- GAIA %>% merge(PF$out_flows,by=0)
# PF$UnicriterionNetFlows
df <- PF$UnicriterionNetFlows %>%
  `colnames<-`(criteria) %>%
  `rownames<-`(basins) %>%
  as.data.frame() %>%
  rownames_to_column("SWSID") %>%
  sample_n(10) %>%
  mutate(SWSID = SWSID %>% as.character())


df.long <- df %>%
  pivot_longer(-SWSID, names_to = "Name") %>%
  left_join(metrics) %>%
  mutate_if()

apex(data = df.long, type = "column", mapping = aes(x = Name, y = value, fill = Goal)) %>%
  ax_facet_wrap(vars(SWSID), ncol = 1) %>%
  ax_chart(stacked = TRUE)
