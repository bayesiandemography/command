
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(command)
})

cmd_assign(.swiss = "data/swiss.csv",
           .out = "out/scaled.rds")

swiss <- read_csv(.swiss, show_col_types = FALSE)

scaled <- swiss |>
  mutate(infant_mort = scale(Infant.Mortality),
         agriculture = scale(Agriculture),
         fertility = scale(Fertility)) |>
  mutate(across(where(is.numeric), as.numeric)) |>
  select(province = Province, infant_mort, agriculture, fertility)

saveRDS(scaled, file = .out)
