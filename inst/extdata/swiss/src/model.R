
suppressPackageStartupMessages({
  library(MASS)
  library(command)
})

cmd_assign(.scaled = "out/scaled.rds",
           method = "M",
           .out = "out/model.rds")

scaled <- readRDS(.scaled)

model <- rlm(fertility ~ agriculture + infant_mort, 
             data = scaled,
             method = method)

saveRDS(model, file = .out)

