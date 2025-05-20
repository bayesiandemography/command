
suppressPackageStartupMessages({
  library(dplyr)
  library(command)
})

cmd_assign(.model = "out/model.rds",
           .out = "out/tab_coef.rds")

model <- readRDS(.model)

coef <- coef(model)

tab_coef <- tibble(Coefficient= names(coef),
                   Value = coef) |>
  filter(Coefficient != "(Intercept)")

saveRDS(tab_coef, file = .out)
