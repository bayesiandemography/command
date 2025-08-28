
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(command)
})

cmd_assign(.raw_data = "data/raw_data.csv",
           .out = "out/cleaned_data.rds")

raw_data <- read_csv(.raw_data, show_col_types = FALSE)

cleaned_data <- raw_data |>
  mutate(agriculture = scale(Agriculture),
         fertility = scale(Fertility),
         agriculture = as.numeric(agriculture),
         fertility = as.numeric(fertility)) |>
  select(province = Province, agriculture, fertility)

saveRDS(cleaned_data, file = .out)
