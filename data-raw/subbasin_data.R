library(readxl)
library(tidyverse)
subbasin_metrics <- read_excel(
  "data-raw/source_data/20220722_KC_SPS_Complete_Metrics_Join_Only.xls") %>%
  mutate_at(c("Presence_of_Shellfish",
              "Drains_to_P_Sensitive_Lake",
              "Presence_of_Coho_Bearing_Streams",
              "Is_Headwater_Basin",
              "Contains_Swimming_Beaches"
              ),as.logical)

usethis::use_data(subbasin_metrics,overwrite = TRUE)
