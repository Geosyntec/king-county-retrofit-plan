library(PerformanceAnalytics)
library(readr)
library(tidyverse)
library("Hmisc")
library(GGally)
library(corrplot)
kc_reduced <- read_csv("data/ee_results/kc_reduced_3.csv")  %>%
  filter(Comment != "water" | is.na(Comment)) %>%
  #rename(canopy_height = '1') %>%
  mutate(traffic_AADT = sqrt(traffic_AADT),
         age_of_impervious_surface = sqrt(age_of_impervious_surface)) #log-transform traffic data


cor_df <- kc_reduced %>%
  select_if(is.numeric) %>% select(
    -c(occurrence,Outlet,Ph1_SWSID, Ph2_DSID,Ph2_SWSID, UCI_END_1, UCI_END_2,area_acr_1,settlement)


      ) %>%
  scale(center=TRUE,scale=TRUE) %>% as.data.frame()

landscape_cols <- c("age_of_impervious_surface",
                    "flow_duration_index",
                    "imperviousness",
                    "mean_annual_runoff_mm",
                    "traffic_AADT"
                    )
#cor_df %>% dplyr::select(all_of(landscape_cols))




cor_plot_img <- PerformanceAnalytics::chart.Correlation(cor_df %>%
                                                          dplyr::select(
  all_of(landscape_cols)
))
cor_plot_img


cor_plot_img2 <- PerformanceAnalytics::chart.Correlation(cor_df %>%
                                                           dplyr::select(
                                                             -all_of(landscape_cols)
                                                           ))
cor_plot_img3 <- PerformanceAnalytics::chart.Correlation(cor_df)

COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)

#rcorr plots
cor_5 <- rcorr(as.matrix(cor_df))
M <- cor_5$r
p_mat <- cor_5$P


ibm.palette <- colorRampPalette(c(
  "#648FFF",
  "#785EF0",
  "#DC267F",
  #"#785EF0",
  #"#648FFF",
  "#FE6100",
  "#FFB000"
),
space = "Lab")

ccpal <- colorRampPalette(c(
  "#2887a1",
  "#6cc2da",
  #"#778868",
  #"#b5b991",
  "#bdbdbd", "#bdbdbd", "#bdbdbd", "#bdbdbd", "#bdbdbd", "#bdbdbd","#bdbdbd", "#bdbdbd","#bdbdbd",
  #"#edbb8a",
  #"#de8a5a",
  "#F3693F",
  "#bf360c"

))


corrplot(M, type = "lower", order = "hclust",method = "number" ,tl.col = "black",
         addCoefasPercent = FALSE, col =ccpal(20),
         p.mat = p_mat, sig.level = 0.01)

cbPalette <- colorRampPalette(c(
  '#a6611a',
  '#dfc27d','#dfc27d',
  '#f5f5f5','#f5f5f5','#f5f5f5','#f5f5f5','#f5f5f5',  '#f5f5f5','#f5f5f5','#f5f5f5','#f5f5f5','#f5f5f5',
  '#80cdc1','#80cdc1',
  '#018571'



                               ))


ggpairs(cor_df, title="correlogram with ggpairs()")
