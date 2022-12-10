
#' Process command line arguments
#'
#' Create objects in the current environment
#' based on arguments passed at the command line.
#' Makes running a script a little like calling
#' a function.
#'
#' # Types of session
#'
#' The behaviour of `cmd_assign()` depends on the
#' type of session.
#' 
#' ## Sessions invoked from a command line
#'
#' `cmd_assign()` is typically used in scripts that
#' are launched from the command line, eg via
#' [utils::Rscript()]. `cmd_assign()` uses
#' command line arguments that are passed when
#' the script is launched to create objects in
#' the current environment.
#'
#' ## Interactive sessions
#'
#' `cmd_assign()` can also be used interactively,
#' which is useful when developing or testing code.
#' When used interactively, `cmd_assign()`
#' relies on defaults specified via ... .
#' 
#'
#' # How arguments are processed
#'
#' ## Matching names to arguments
#'
#' The way that `cmd_assign` matches values supplied
#' at the command line to names specified in the call
#' to `cmd_assign` is loosely based of the way that R
#' matches values supplied in a function call
#' to names specified in the function definition.
#'
#' `cmd_assign` begins by identifying values
#' passed at the command line that are named. 
#' `cmd_assign()` treats a value as named
#' if it has the form
#' 
#' `-<single-letter>=<value>`
#'
#' or
#'
#' `--<name>=<value>`
#'
#' (note that there are no spaces around the `=`.)
#' Examples are `-n=100` and `--n_iteration=100`.
#'
#' If a named argument is supplied at the command line
#' but that name was not specified in the call to
#' `cmd_assign(), then `cmd_assign()` raises an error.
#'
#' Once it has matched all the named command line
#' arguments, `cmd_assign()` matches unnamed command
#' line arguments to the remaining names in
#' .... As with R function calls, unnamed arguments
#' are matched in the order that they appear.
#'
#' Every argument that is specified in the
#' call to `cmd_assign()` must be supplied with
#' a value at the command line. If no value is
#' supplied, `cmd_assign()` does not fall back
#' on the default, but instead raises an error.
#'
#' ## Creating objects
#'
#' A command line argument is just a character string.
#' There are two circumstances in which `cmd_assign()`
#' converts the string into something else:
#'
#' 1. If the string is the file path for an `.rds` file,
#'   and the file path does not start with `"p_"`, `"p."`
#'   or `"p<capital letter>"`, then `cmd_assign()` loads
#'   the file.
#' 1. If the default argument is a logical, integer or
#'   double, then `cmd_assign` will try to convert the
#'   string to the same type.
#'
#' Otherwise `cmd_assign()` leaves the string untouched.
#'
#' # More information
#'
#' For examples of the use of `cmd_assign()`,
#' plus discussion of how `cmd_assign` can fit
#' into a data analysis workflow,
#' see NAME OF VIGNETTE.
#'
#' @param ... Name-value pairs.
#' 
#' @returns `cmd_assign()` invisible returns
#' a named list of objects, but is normally called
#' for its side effect, which is to create objects
#' in the current environment.
#'
#' @seealso Internally, `cmd_assign()` uses
#' [base::interactive()] to decide whether
#' the current session is interactive, and uses
#' [base::commandArgs()] to access command line
#' arguments.
#'
#' @examplesIf interactive()
#' ## Running
#' cmd_assign(p_inputs = "data/inputs.csv",
#'            fitted_values = "out/fitted_values.rds",
#'            variant = "low",
#'            size = 12)
#' ## in an interactive session is equivalent to
#' p_inputs <- "data/inputs.csv"
#' fitted_valus <- readRDS("out/fitted_values.rds")
#' variant <- low
#' size <- 12
#' ## If the script containing the lines above
#' ## is run from the command line, then
#' ## "data/inputs.csv", "out/fitted_values.rds",
#' ## "low", and 12 are replaced by values
#' ## passed at the command line.
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
