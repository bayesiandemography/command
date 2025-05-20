
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(command)
})

cmd_assign(.vals_fitted = "out/vals_fitted.rds",
           digits = 2,
           .out = "out/tab_fitted.csv")

vals_fitted <- readRDS(.vals_fitted)

tab_fitted <- vals_fitted |>
  mutate(across(where(is.numeric), ~round(.x, digits = digits)))

write_csv(tab_fitted, file = .out)
