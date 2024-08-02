## code to prepare the internal datasets
sections <- read.csv(here::here("data-raw", "result_sections.csv"))
summary.data <- read.csv(here::here("data-raw", "summary_analysis.csv"))
usethis::use_data(sections, summary.data, internal = TRUE)



