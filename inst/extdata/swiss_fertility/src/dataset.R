
library(command)

cmd_assign(p_dataset = "data/swiss.csv")

dataset <- read.csv(p_dataset)

saveRDS(dataset, file = "out/dataset.rds")
