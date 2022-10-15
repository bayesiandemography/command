

args <- "
@file fn11 data-raw/xxx.xlsx
@file fn_erp16 data-raw/xxx.xlsx
@load age_conc out/age_conc.rds
@load vals_deaths out/vals_deaths-{smooth}.rds
@named n_comp 12
@named algo ndi
"

file_args(p_erp11 = "data-raw/xxx.xlsx"
          p_erp16 = "data-raw/xxx.xlsx"
          age_conc = "out/age_conc.rds",
          vals_death = "out/vals_deaths-smooth.rds",
          n_comp = 12,
          algo = "ndi")

p_target()


          p_target = "out/mort.rds")



           
           

assign_nms(fn_indig = "data-raw/xxx.xlsx",
           fn_nonindig = "data-raw/xxx.xlsx")

load_rds("




unnamed_args(* = "../out/vals_direct.rds",
             * = "../../svd/out/bX-5.rds",
             p_model = "src/model_simple.stan",
             p_out = "out/mod_base-NDI-5")

named_args(algo = "NDI",
           n_pc = 5,
           n_iter_mod = 20)
             
             p(
             

set_unnamed("

arg_set(



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


library(tm)
library(igraph)
## Read makefile into R
makefile <- readLines("Makefile")
## Find relevant lines in makefile
dep <- grep(":", makefile, value = TRUE)
## Select target files
target <- gsub(":.*", "", dep)
## Select files target depends on
depends <- gsub(".*:", "", dep)
depends <- strsplit(depends, " ")
names(depends) <- target
## Create a dependency matrix (using igraph package)
dlist <- lapply(target, function(t) {
  d <- if(length(depends[[t]]) == 0) NA else depends[[t]]
  data.frame(depends = d, target = t)
  })
dependencymat <- na.omit(do.call("rbind", dlist))
dependencymat <- dependencymat[dependencymat$depends != "", ]                         
makegraph <- graph.data.frame(dependencymat)
## ... and plot
plot(makegraph, vertex.shape = "none", edge.arrow.size = 0.5)
