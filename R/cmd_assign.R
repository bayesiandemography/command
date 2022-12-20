
#' Process command line arguments
#'
#' @description 
#' Create objects in the current environment,
#' based on arguments passed at the command line.
#' For instance, if a script `tidy_data.R` containing the
#' lines
#' 
#' `cmd_assign(raw_data = "raw_data.rds",
#'             unit = "kg")`
#'             
#' is run at the command line using
#' 
#' `Rscript tidy_data.R raw_data.rds --unit=kg`
#' 
#' then objects called `raw_data` and `unit` are
#' created in the current environment.
#'
#' @details
#' # Types of session
#'
#' `cmd_assign()` behaves differently depending
#'  on how it is called:
#'
#' * If `cmd_assign()` is called inside a script
#'   being run from the command line, it processes
#'   values passed at the command line.
#' * If `cmd_assign()` is called in an interactive
#'   session, it uses the name-value pairs from the call.
#' 
#' `cmd_assign()` is typically used interactively
#' when developing or testing code, and in command
#' line mode once the code is mature.
#'
#' # Matching names and values
#'
#' When used in command line mode, `cmd_assign()`
#' first processes named command-line arguments,
#' and then processes unnamed command-line arguments.
#' 
#' `cmd_assign()` treats a command-line argument as named
#' if the argument has the form `-<single-letter>=<value>`,
#' eg `-n=100`, or `--<name>=<value>`, 
#' eg `--n_iteration=100`. (Note that there are no 
#' spaces around the equals signs.)
#'
#' `cmd_assign()` matches unnamed command line arguments
#' in the order in which they appear.
#' 
#' If, for instance, the script `analysis.R`
#' containing the lines
#' 
#' ```R
#' cmd_assign(data = "person.csv",
#'            impute = TRUE,
#'            max_age = 85)
#' ```
#' 
#' is launched from the command line using
#' ```R
#' Rscript analysis.R --max_age=90 person.csv --impute=TRUE
#' ```
#' then `cmd_assign()` proceeds as follows:
#' 1. Named command-line arguments
#'     - `--max_age=90`. Look in the call to `cmd_assign()` 
#'      for an argument called `max_age`. If there is  one,
#'      create an object called `max_age` holding the value
#'      `90`. If not, raise an error.
#'    - `--impute=TRUE`. Look in the call to `cmd_assign()` 
#'      for an argument called `impute`. If there is one,
#'      create an object called `impute` holding the value
#'      `TRUE`. If not, raise an error.
#' 2. Unnamed command-line arguments
#'     - `person.csv`. Count the number of unused name-value
#'        pairs from the call to `cmd_assign()`. If there is
#'        exactly one, create an object with that name
#'        holding the value `"person.csv"`. If not, 
#'        raise an error.
#'        
#'
#' # Creating objects
#'
#' The way that an object is created in the
#' current R environment depends on the
#' type of value passed at the command line
#' and sometimes on the value supplied as a default.
#'
#' ## Value looks like a `.rds` file path
#'
#' If the value looks like the file path
#' for an `.rds` object (ie an object created
#' by a function  such as [base::saveRDS]),
#' then `cmd_assign()` tries to load the `.rds` file.
#' For instance, in an interactive session,
#' 
#' `cmd_assign(costs = "costs.rds")`
#' 
#' is equivalent to
#'
#' `costs <- readRDS("costs.rds")`
#'
#' To avoid automatically loading an `.rds`
#' file, give the argument a name  that starts
#' with a dot. For instance,
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
#' are copied into the current environment,
#' as named strings.
#'
#' @param ... Name-value pairs.
#' 
#' @returns `cmd_assign()` is called
#' for its side effect, which is to create objects
#' in the current environment. However, `cmd_assign()`
#' also invisibly returns a named list of objects.
#'
#' @seealso 
#' The vignette `vignette("command")`
#' gives examples of the use of `cmd_assign()`,
#' and discusses how `cmd_assign` can be used in the
#' workflow for a data analysis.#' 
#' 
#' Function [utils::Rscript()] and
#' package 
#' [littler](https://CRAN.R-project.org/package=littler)
#' can be used to run R
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
