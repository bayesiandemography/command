
library(command)
library(MASS)

cmd_assign(dataset = "out/dataset.rds",
           robust_method = "M")

model <- rlm(Fertility ~ Education, 
            data = dataset,
            method = robust_method)

saveRDS(model, file = "out/model.rds")

