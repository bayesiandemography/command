
#' Get the 'target' for the current file
#'
#' Get the 'target' for the current file, ie get a path
#' to the file that holds the output created by the
#' current file.
#'
#' By default, \code{target} assumes that (i) the
#' current file is being run from the command line,
#' and that the name of the target was passed
#' as a command line argument. See the examples below
#' and the vignette for this package.
#'
#' If the current file is not being run from the command line,
#' and if a \code{default} argument has been supplied, then
#' \code{target} will use that instead. A default is
#' helpful when developing code. A default can, however, disguise
#' problems. Once the code has matured, removing the default
#' can make the code safer.
#'
#' @param default A value to return if the current
#' file is not being run from the command line.
#'
#' @return A character vector of length 1,
#' giving the path to a file.
#'
#' @seealso \code{\link{variant}}
#'
#' @examples
#' ## -- Single target ------------------------------------
#' 
#' ## create a file called "product.R"
#' txt <- c(
#'   "library(makr)",
#'   "product <- 6 * 7"\n",
#'   "target <- target()\n",
#'   "saveRDS(product, file = target)"
#' )
#' writeLines(txt, file = "product.R")
#' 
#' ## check that the file was created correctly
#' dir()
#' readLines("product.R")
#' 
#' ## run the file from the command line, passing the
#' ## name of the target file ("product.rds") as a
#' ## command line argument
#' system("Rscript product.R product.rds")
#' 
#' ## check that the new file was created correctly
#' dir()
#' 
#' ## read the contents of the new file
#' readRDS("product.rds")
#' 
#' ## tidy up
#' file.remove("product.R", "product.rds")
#' 
#'
#' ## -- Reuse file to make multiple targets --------------
#' 
#' ## create a file called "product.R"
#' txt <- c(
#'   "library(makr)
#'   "target <- target()\n",
#'   "variant <- variant(target)\n",
#'   "multipler <- if (variant == "high") 10 else 5\n",
#'   "product <- multiplier * 7\n",
#'   "saveRDS(product, file = target)"
#' )
#' writeLines(txt, file = "product.R")
#' 
#' ## check that the file was created correctly
#' dir()
#' readLines("product.R")
#' 
#' ## run the file from the command line twice,
#' ## with two different targets
#' system("Rscript product.R product-high.rds")
#' system("Rscript product.R product-low.rds")
#'
#' ## check that the new files were created correctly
#' dir()
#'
#' ## read the contents of the new files
#' readRDS("product-high.rds")
#' readRDS("product-low.rds")
#'
#' ## tidy up
#' file.remove("product.R", "product-high.rds", "product-low.rds")
#'
#' ## -- Use the 'default' argument -----------------------
#' 
#' ## call 'target' when working interactively
#' target(default = "product-high.rds")
#' @export
target <- function(default = NULL) {
    p_args <- "^--args$"
    p_interact <- "^--interactive$"
    args <- commandArgs()
    i_args <- grep(p_args, args)
    has_target <- length(i_args) > 0L
    if (has_target) {
        n_args <- length(args)
        i_args <- i_args[[1L]]
        too_many_args <- n_args > i_args + 1L
        if (too_many_args) {
            s_passed <- seq(from = i_args + 1L, to = n_args)
            args_passed <- args[s_passed]
            args_passed <- sprintf("%s", args_passed)
            args_passed <- paste(args_passed, collapse = ", ")
            stop("more than one command line argument passed : ",
                 args_passed,
                 call. = FALSE)
        }
        ans <- args[[n_args]]
    }
    else {
        has_default <- !is.null(default)
        if (has_default) {
            assert_string(default, min.chars = 1L)
            ans <- default
            message("'target' using default value [\"", ans, "\"]")
        }
        else {
            msg <- "could not find path to target"
            is_interact <- any(grepl(p_interact, args))
            if (is_interact)
                msg <- paste0(msg, " : appears to be an interactive session")
            stop(msg, call. = FALSE)
        }
    }
    ans
}
