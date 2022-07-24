



f <- function(p_target = "out/fit-indig-ever-20-svd-5.rds",
              p_model = "src/model_simple.stan",
              p_vals = "../out/vals_direct.rds",
              p_bX = "../../svd/out/bX-5.rds",
              indig = "indig",
              algo = "ever",
              phi = 20,
              model = "svd",
              ncomp = 5,
              iter = 1000,
              ...) {
    NULL
}



file_args <- function(...) {
    substitute(list(...))
}
               




f <- function(p_target = "out/fit-indig-ever-20-svd-5.rds",
              p_model = "src/model_simple.stan",
              p_vals = "../out/vals_direct.rds",
              p_bX = "../../svd/out/bX-5.rds",
              indig = "indig",
              algo = "ever",
              phi = 20,
              model = "svd",
              ncomp = 5,
              iter = 1000,
              ...) {
    NULL
}

f()




file_args(p_target = "out/fitted.rds",
          n_iter = 100,
          model,
          ...)




library(dplyr)
library(argfun)


args_xnnamed(p_target = "out/fit-indig-ever-20-svd-5.rds",
             p_model = "src/model_simple.stan",
             p_vals = "../out/vals_direct.rds",
             p_bX = "../../svd/out/bX-5.rds")

args_named(indig = "indig",
           algo = "ever",
           phi = 20,
           model = "svd",
           ncomp = 5,
           iter = 1000)

dots_args()



            
            
          
          


set_opts(indigenous = "indig",
         rule = "ever",
         phi = 20,
         n_component = 5)




args <- get_args()


assign_args(path = c(model = "src/model.stan",
                     vals = "out/vals_direct.rds"),
            opt = c(indigenous = "indig",
                    rule = "ever",
                    phi = "20",
                    n_component = 5),
            control = c(iter = 500))



get_args(return = "fitted_(indigenous)-(rule)-(phi)-(n_component)",
         inputs = c("bX-(n_component)",
                    "vals_direct",
                    "model-(simple)"),
         control = "iter")

## creates paths 'p_return', 'p_bX', 'p_vals_direct', 'p_model'
## creates strings/numbers 'indigenous', 'rule', 'phi', 'n_component', 'iter'


