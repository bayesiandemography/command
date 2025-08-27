
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(command)
})

cmd_assign(.cleaned_data = "out/cleaned_data.rds",
           .model_m = "out/model_m.rds",
           .model_mm = "out/model_mm.rds",
           .out = "out/vals_fitted.rds")

cleaned_data <- readRDS(.cleaned_data)
model_m <- readRDS(.model_m)
model_mm <- readRDS(.model_mm)

vals_fitted <- cleaned_data |>
  mutate(m = fitted(model_m),
         mm = fitted(model_mm)) |>
  pivot_longer(c(m, mm),
               names_to = "variant",
               values_to = "fitted")
           
saveRDS(vals_fitted, file = .out)

