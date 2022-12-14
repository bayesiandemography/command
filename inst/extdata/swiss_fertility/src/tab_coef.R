
library(command)

cmd_assign(model = "out/model.rds")

coef <- coef(model)
tab_coef <- as.data.frame(coef)

write.csv(tab_coef, file = "out/tab_coef.csv")
