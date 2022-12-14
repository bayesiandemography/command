
library(command)

cmd_assign(vals = "out/vals_fertility.rds",
           digits_round = 2)

fertility <- data.frame(modelled = vals$fertility_modelled,
                        actual = vals$fertility_actual)
fertility <- round(vals, digits = digits_round)

write.csv(fertility,
          file = "out/tab_fertility.csv",
          row.names = FALSE)
