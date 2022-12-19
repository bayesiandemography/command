
#' Process command line arguments
#'
#' Create objects in the current environment,
#' based on arguments passed at the command line.
#'
#' # Types of session
#'
#' `cmd_assign()` behaves different depending on how it is called:
#'
#' * If `cmd_assign()` is called in a script that is
#'   being run from the command line, then it uses
#'   values that were passed at the command line.
#' * If `cmd_assign()` is called in an interactive
#'   session, then it uses default values.
#' 
#' `cmd_assign()` is typically used interactively
#' when developing or testing code, and in command
#' line mode once the code is mature.
#'
#' # Matching names and values
#'
#' `cmd_assign()` starts by processing named arguments.
#' `cmd_assign()` treats an argument as named
#' if the argument has the form
#' 
#' `-<single-letter>=<value>`
#'
#' or
#'
#' `--<name>=<value>`
#'
#' for instance
#' 
#' `-n=100`
#' 
#' or
#' 
#' `--n_iteration=100`
#' 
#' (Note that there are no spaces around the 
#' equals signs.)
#'
#' Once `cmd_assign()` has processed named 
#' command line arguments, it matches unnamed
#' arguments to unused names in the call to
#' `cmd_assign()`.
#' 
#' Consider, for instance, the script `analysis.R`
#' containing the lines
#' 
#' ```R
#' cmd_assign(data = "person.csv",
#'            impute = TRUE,
#'            max_age = 85)
#' ```
#' 
#' If `analysis.R` is launched from the
#' command line using
#' ```R
#' Rscript analysis.R --max_age=90 person.csv --impute=TRUE
#' ```
#' then `cmd_assign()` will proceed as follows
#' - look for a named command line argument `impute`,
#'   and, in R, assign the name `impute` to whatever
#'   value was passed (`"TRUE"` in this case)
#' - look for a named command line argument `max_age`,
#'   and, in R, assign the name `max_age` to whatever
#'   value was passed (`"90"` in this case)
#' - look for an unnamed command line argument,
#'   and, in R, assign the name `data` to whatever
#'   value was passed (`"person.csv"` in this case).
#' 
#' `cmd_assign()` only reverts to defaults when 
#' it is in interactive mode. When it is
#' in command line mode, every 
#'  `cmd_assign()`
#' must be supplied with a value at the command line.
#'
#' # Creating objects
#'
#' The way that an object is created in the
#' current R environment depends on the
#' type of value passed at the command line
#' and/or the value supplied as a default.
#'
#' ## Value looks like a `.rds` file path
#'
#' If the value looks like the file path
#' for an `.rds` object (ie an object created
#' by a function  such as [base::saveRDS]),
#' then `cmd_assign()` tries to load the file.
#' For instance, in an interactive session,
#' 
#' `cmd_assign(costs = "costs.rds")`
#' 
#' is equivalent to
#'
#' `costs <- readRDS("costs.rds")`
#'
#' To avoid automatically loading an `.rds`
#' file, give the argument
#' a name starting with a dot. For instance,
#' in an interactive session,
#'
#' `cmd_assign(.costs = "costs.rds")`
#' 
#' is equivalent to
#'
#' `.costs <- "costs.rds"`
#' 
#' ## Default is logical, integer, or double
#'
#' If the default value is a logical, integer,
#' or double, then `cmd_assign()` will try to
#' convert the value supplied at the command
#' line to the same type. For instance,
#' if the call to `cmd_assign` is
#' 
#' `cmd_assign(x_lgl = FALSE,
#'             x_int = 3L,
#'             x_dbl = 3.142)`
#'
#' then passing the values `"TRUE"`, `"2"`,
#' and `"2.718"` at the command line is equivalent
#' to
#'
#' ```R
#' x_lgl <- TRUE
#' x_int <- 2L
#' x_dbl <- 2.718
#' ```
#'
#' ## Everything else
#'
#' Otherwise, arguments passed at the command line
#' are simply copied into the current environment,
#' as named strings.
#'
#' # More information
#'
#' The vignette `vignette("command")`
#' gives examples of the use of `cmd_assign()`,
#' and discusses how `cmd_assign` can be used in the
#' workflow for a data analysis.
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
#'            size = 12.2)
#' ## In an interactive session, this is equivalent to
#' p_inputs <- "data/inputs.csv"
#' fitted_values <- readRDS("out/fitted_values.rds")
#' variant <- "low"
#' size <- 12.2
#' }
#' @export
cmd_assign <- function(...) {
    envir <- parent.frame()
    args_dots <- list(...)
    check_args_dots(args_dots)
    if (interactive())
        args <- args_dots # nocov
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
