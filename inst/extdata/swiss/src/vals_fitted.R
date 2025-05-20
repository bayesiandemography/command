
suppressPackageStartupMessages({
  library(dplyr)
  library(command)
})

cmd_assign(.scaled = "out/scaled.rds",
           .model = "out/model.rds",
           .out = "out/vals_fitted.rds")

scaled <- readRDS(.scaled)
model <- readRDS(.model)

vals_fitted <- scaled |>
  mutate(fitted = fitted(model),
         residuals = residuals(model))
         
saveRDS(vals_fitted, file = .out)

