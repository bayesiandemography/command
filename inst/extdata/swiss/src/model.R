
suppressPackageStartupMessages({
  library(MASS)
  library(command)
})

cmd_assign(.cleaned_data = "out/cleaned_data.rds",
           method = "M",
           .out = "out/model_m.rds")

cleaned_data <- readRDS(.cleaned_data)

model <- rlm(fertility ~ agriculture, 
             data = cleaned_data,
             method = "M")

saveRDS(model, file = .out)

