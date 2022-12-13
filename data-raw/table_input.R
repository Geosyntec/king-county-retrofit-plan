library(readr)
test_data <- read_csv("data-raw/source_data/cor_updated.csv",
  col_types = cols(SWSID = col_character())
) %>%
  column_to_rownames("SWSID")



usethis::use_data(test_data, overwrite = TRUE)
