library(dplyr)
library(tidyr)
library(tibble)
library(visNetwork)
source(here::here("R","fct_helpers.R"))
load(here::here("data","pf_test.rda"))

plot_jittered_pref_flows(pf_test[[1]],pf_test[[2]],tolerance = 0.6)

