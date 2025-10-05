
#' Assign Values Passed at the Command Line or Interactively
#'
#' @description
#' Assign values to names in the working environment.
#' The values are typically supplied through the command line,
#' but can be supplied interactively.
#'
#' Specifying the inputs and outputs of scripts through the
#' command line can contribute to safter, more modular
#' workflows.
#'
#' @details
#' # Types of session
#'
#' `cmd_assign()` behaves differently depending
#'  on how it whether it is called
#'
#' 1. interactively, or
#' 2. inside an R script that is run from the command line.
#'
#' For instance, if the code
#'
#' `cmd_assign(use_log = TRUE)`
#'
#' is run interactively, it creates an object 
#' called `use_log` with value `TRUE`.
#'
#' But if the same code is run inside a
#' script via the command
#' 
#' `Rscript tidy_data.R --use_log=FALSE`
#' 
#' it creates an object called `use_log`
#' with value `FALSE`.
#'
#' `cmd_assign()` is typically called interactively
#' when a workflow is being developed,
#' and through the command line when the
#' workflow has matured.
#'
#' # Matching names and values
#'
#' When used in a script called from the
#' command line, `cmd_assign()`
#' first matches named command line arguments,
#' and then matches unnamed command line arguments,
#' in the order in which they are supplied.
#'
#' If, for instance, the script `person.R` contains
#' the lines
#'
#' ```
#' cmd_assign(.data = "raw_data.csv",
#'            max_age = 85,
#'            .out = "person.rds")
#' ```
#'
#' and if `person.R` is run from the command line using
#'
#' ```
#' Rscript person.R raw_data.csv person.rds --max_age=100
#' ```
#' 
#' then `cmd_assign()` first matches named
#' command line argument `--max_age=100` to `cmd_assign()
#' argument `max_age`, and then matches
#' unnamed command line arguments
#' `raw_data.csv` and `person.rds` to `cmd_assign()`
#' arugments `.data` and `.out`.
#'
#' # Coercing values passed at the command line
#' 
#' Values passed at the command line start out as
#' text strings. `cmd_assign()` coerces these text strings
#' to have the same class as the corresponding values
#' in the call to `cmd_assign()`. For instance,
#' if a script called `fit.R` contains
#' the lines
#' 
#' ```
#' cmd_assign(.data = "cleaned.rds",
#'            impute = TRUE,
#'            date = as.Date("2026-01-01"),
#'            .out = "fit.rds")
#' ```
#'
#' and if `fitted.R` is run from the command line using
#'
#' ```
#' Rscript fitted.R cleaned.rds fit.rds --impute=TRUE --date=2025-01-01
#' ```
#'
#' then `cmd_assign()` will create
#'
#' - a character vector called `.data` with value "cleaned.rds",
#' - a logical vector called `impute` with value `TRUE`,
#' - a date vector called `date` with value `"2025-01-01"`, and
#' - a character vector called `.out` with value `"fit.rds".
#'
#' @param ... Name-value pairs.
#' 
#' @returns `cmd_assign()` is called
#' for its side effect, which is to create objects
#' in the global environment. However, `cmd_assign()`
#' also invisibly returns a named list of objects.
#'
#' @seealso
#' - [extract_shell()] Turn a `cmd_assign()` call into a shell command
#' - [extract_make()] Turn a `cmd_assign()` call into a Makefile rule
#' - [shell_script()] Create a shell script
#' - [makefile()] Create a Makefile
#' - [Quick Start Guide](https://bayesiandemography.github.io/command/articles/quickstart.html)
#'   How to use `cmd_assign()`
#' - [Modular Workflows for Data Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
#'   Safe, flexible data analysis workflows.
#' - Base R function [commandArgs()] uses a more general,
#'   lower-level approach to processing command line arguments.
#'   (`commandArgs()` is called internally by `cmd_assign()`.)
#' - [littler](https://CRAN.R-project.org/package=littler) Alternative to Rscript
#'
#' @references
#' - [Command-Line Programs](https://swcarpentry.github.io/r-novice-inflammation/05-cmdline.html)
#'   Introduction to Rscript
#'   
#' @examples
#' if (interactive()) {
#'   cmd_assign(.data = "mydata.csv",
#'              n_iter = 2000,
#'              .out = "results.rds")
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
    assign_args(args = args,
                envir = envir)
}
