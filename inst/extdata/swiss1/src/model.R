
suppressPackageStartupMessages({
  library(MASS)
  library(command)
})

cmd_assign(.cleaned_data = "out/cleaned_data.rds",
           .out = "out/model.rds")

cleaned_data <- readRDS(.cleaned_data)

model <- rlm(fertility ~ agriculture, 
             data = cleaned_data)

saveRDS(model, file = .out)

