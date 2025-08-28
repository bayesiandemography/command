
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(command)
})

cmd_assign(.cleaned_data = "out/cleaned_data.rds",
           .model = "out/model.rds",
           .out = "out/vals_fitted.rds")

cleaned_data <- readRDS(.cleaned_data)
model <- readRDS(.model)

vals_fitted <- cleaned_data |>
  mutate(fitted = fitted(model))
           
saveRDS(vals_fitted, file = .out)

