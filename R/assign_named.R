
#' Create objects based on named command line arguments
#'
#' Create objects in the current environment
#' based on named arguments passed at the command line,
#' or on values supplied via \dots.
#'
#' The behaviour of \code{assign_named} depends the
#' on type of R session it is called from: whether it
#' is a standard interactive session, or whether the
#' session was invoked from the command line, e.g
#' via \code{\link[utils]{Rscript}}. Typically,
#' \code{assign_named} is used iteractively when
#' developing code and via the command line
#' when the code is mature.
#'
#' In an interactive session, \code{assign_named}
#' takes the names and values for the objects
#' from \dots. In a session invoked from the command
#' line, \code{assign_unnamed} takes the names
#' and the values from named command line
#' arguments, having checked that the names
#' match those in \dots (not necessarily in the same order).
#' \code{assign_named} tries to coerce values
#' passed at the command line to the same classes as
#' the corresponding arguments in \dots, raising
#' an error if this cannot be done.
#'
#' \code{assign_named} assumes that named arguments have
#' the form
#' \itemize{
#'   \item \code{-<single-letter>=<value>}
#'      e.g. \code{-n=10} or \code{-f=params.rds}, or
#'   \item \code{--<name>=<value>}
#'      e.g. \code{--nchains=10} or
#'        \code{--filename=params.rds},
#' }
#' or a combination of the two. (Note that there are
#' no spaces around the \code{=} sign.) \code{assign_named}
#' processes both types of argument in the same way.
#' 
#' @param \dots Names and values for objects.
#' The values must have type logical, integer, numeric,
#' or character.
#' 
#' @return \code{assign_named} returns a named list of objects
#' invisibly, but is normally called for its side effect,
#' which is to create objects in the current environment.
#'
#' @seealso Unnamed values passed at the command line
#' can be processed using function \code{\link{assign_unnamed}}.
#'
#' \code{assign_named} uses function
#' \code{\link[base]{interactive}} to decide
#' whether a function is interactive.
#'
#' @examples
#' ## used interactively ----------------------------------
#' assign_named(outfile = "results.rds",
#'              i = 100)
#' ls()
#' outfile
#' iter
#' rm(outfile, iter)
#'
#' 
#' ## used in script run from command line ----------------
#'
#' ## create script and place in
#' ## temporary directory
#' txt <- "library(argfun)
#'         assign_named(outfile = "results.rds",
#'                      i = 100)
#'         print(ls())
#'         print(outfile)
#'         print(iter)"
#' dir_current <- getwd()
#' setwd(tempdir())
#' cat(txt, file = "myscript.R")
#' ## run script from command line
#' system("Rscript myscript.R -i=500 --outfile=output.rds")
#' ## tidy up
#' file.remove("myscript.R")
#' setwd(dir_current)
#' @export
assign_named <- function(...) {
    args_dots <- get_args_dots(..., fun_name = "assign_named")
    if (interactive())
        assign_args(arg_dots)
    else {
        args_cmd <- get_args_cmd_named(args_dots)
        args_comb <- make_args_comb(args_dots = args_dots,
                                    args_cmd = args_cmd)
        assign_args(args_comb)
    }
}
