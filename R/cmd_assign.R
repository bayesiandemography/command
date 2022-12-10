
#' Process command line arguments
#'
#' Create objects in the current environment,
#' based on arguments passed at the command line.
#' Allows scripts to be used like functions.
#'
#' # Types of session
#'
#' The behavior of `cmd_assign()` depends how it is called:
#'
#' * If `cmd_assign()` is called in a script that is
#'   run from the command line,
#'   then `cmd_assign()` processes
#'   values that were passed at the command line.
#' * If `cmd_assign()` is used interactively, then
#'   it processes values values supplied to it
#'   when it is called.
#' 
#' `cmd_assign()` is designed mainly for use with the
#' command line. However, using it interactively can be
#' helpful when developing or testing code.
#'
#' # How arguments are processed
#'
#' ## Matching names and values
#'
#' `cmd_assign()` fist processes named arguments
#' `cmd_assign()` treats an argument as named
#' if it has the form
#' 
#' `-<single-letter>=<value>`
#'
#' or
#'
#' `--<name>=<value>`
#'
#' Examples are `-n=100` and `--n_iteration=100`.
#' Note that there are no spaces around the 
#' equals sign.
#'
#' If a named argument supplied at the command line
#' does not have a counterpart in the call to
#' `cmd_assign()`, then `cmd_assign()` raises an error.
#'
#' Once `cmd_assign()` has processed all the named 
#' command line arguments, it matches any unnamed
#' command to any unused names from the call to
#' `cmd_assign()`.
#'
#' When `cmd_assign()` is processing command line
#' arguments, it does not fall back on defaults. 
#' Every name specified in the call to `cmd_assign()`
#' must be supplied with a value at the command line.
#'
#' ## Creating objects
#'
#' A command line argument is a character string.
#' The object that `cmd_assign()` creates in 
#' the current environment is just a copy of
#' this string, except in two situations:
#'
#' 1. The string is the file path for an `.rds` file,
#'   and the file path does not start with `"p_"`, `"p."`
#'   or `"p<capital letter>"`. In this case,
#'   `cmd_assign()` loads the file.
#' 1. The default value is a logical, integer or
#'   double. In this case, then `cmd_assign()`
#'   converts the string to the same type.
#'
#' # More information
#'
#' The vignette NAME OF VIGNETTE gives examples
#' of the use of `cmd_assign()`, and discusses how
#' `cmd_assign` can be used in a data analysis
#'  workflow.
#'
#' @param ... Name-value pairs.
#' 
#' @returns `cmd_assign()` is normally called
#' for its side effect, which is to create objects
#' in the current environment. However, `cmd_assign()`
#' also invisibly returns a named list of objects.
#'
#' @seealso Function [utils::Rscript()] and
#' package `littler` can be used to run
#' scripts from the command line.
#' 
#' Internally, `cmd_assign()` uses
#' [base::interactive()] to decide whether
#' the current session is interactive, and uses
#' [base::commandArgs()] to access command line
#' arguments.
#'
#' @examples
#' \dontrun{
#' cmd_assign(p_inputs = "data/inputs.csv",
#'            fitted_values = "out/fitted_values.rds",
#'            variant = "low",
#'            size = 12)
#' ## In an interactive session, this is equivalent to
#' p_inputs <- "data/inputs.csv"
#' fitted_valus <- readRDS("out/fitted_values.rds")
#' variant <- low
#' size <- 12
#' }
#' @export
cmd_assign <- function(...) {
    envir <- parent.frame()
    args_dots <- list(...)
    check_args_dots(args_dots)
    if (interactive())
        args <- args_dots
    else {
        args_cmd <- get_args_cmd()
        check_args_cmd(args_cmd = args_cmd,
                       args_dots = args_dots)
        args_cmd <- align_cmd_to_dots(args_cmd = args_cmd,
                                      args_dots = args_dots)
        args_cmd <- coerce_to_dots_class(args_cmd = args_cmd,
                                         args_dots = args_dots)
        args <- args_cmd
    }
    args_new <- replace_rds_with_obj(args)
    assign_args(args_new = args_new,
                args_old = args,
                envir = envir)
}
