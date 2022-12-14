
library(command)

cmd_assign(model = "out/model.rds")

vals <- data.frame(education = model$x[, "Education"])
vals$fertility_modelled <- fitted(model)
residuals <- residuals(model)
vals$fertility_actual <- vals$fertility_modelled + residuals

saveRDS(vals, file = "out/vals_fertility.rds")

