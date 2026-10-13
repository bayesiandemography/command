
#' Assign Values Passed at the Command Line or Interactively
#'
#' @description
#' Assign values to names in the working environment.
#' The values are typically supplied through the command line,
#' but can be supplied interactively.
#'
#' Specifying the inputs and outputs of scripts through the
#' command line can make workflows safer and more modular.
#'
#' `cmd_assign_quiet()` is identical to `cmd_assign()`,
#' but does not print progress messages to the console.
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
#' # File names
#'
#' `cmd_assign()` does not require the names of arguments
#' that hold file paths to start with a dot.
#' The leading dot is a convention used by
#' [extract_make()], [extract_shell()], [makefile()],
#' and [shell_script()] when they decide which
#' arguments are file names:
#'
#' - If any argument name starts with a dot, those
#'   arguments are the file names.
#' - Otherwise an argument is a file name when the
#'   file exists, or when the value looks like a path.
#'
#' Dotted names such as `.data` and `.out` make that
#' choice explicit. They are not required when the
#' script is run.
#'
#' @param ... Name-value pairs.
#' 
#' @returns `cmd_assign()` is called
#' for its side effect, which is to create objects
#' in the global environment. However, `cmd_assign()`
#' also invisibly returns a named list of objects.
#'
#' @seealso
#' - [cmd_assign_dots()] Collect unnamed command line arguments
#'   into one character vector
#' - [extract_shell()] Turn a `cmd_assign()` call into a shell command
#' - [extract_make()] Turn a `cmd_assign()` call into a Makefile rule
#' - [shell_script()] Create a shell script
#' - [makefile()] Create a Makefile
#' - [use_renv()] Activate a renv project before loading packages
#' - [Quick Start](https://bayesiandemography.github.io/command/articles/quickstart.html)
#'   How to use `cmd_assign()`
#' - [Modular Workflows for Data Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
#'   Safe, flexible data analysis workflows.
#' - [Using command with renv](https://bayesiandemography.github.io/command/articles/a5_renv.html)
#'   Running pipeline scripts with renv
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
              envir = envir,
              quiet = FALSE)
}

#' @export
#' @rdname cmd_assign
cmd_assign_quiet <- function(...) {
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
              envir = envir,
              quiet = TRUE)
}


#' Assign a Variable Number of Command Line Arguments
#'
#' @description
#' Assign values to names in the working environment,
#' like [cmd_assign()], but collect every unnamed
#' command line argument into one character vector.
#'
#' Use `cmd_assign_dots()` when a script must accept
#' a number of file paths that is known only when the
#' script is run. The script can then decide whether
#' the first element, the last element, or neither
#' is special.
#'
#' @details
#' # Matching names and values
#'
#' Named command line arguments are matched by name,
#' as in [cmd_assign()]. Exactly one argument must
#' be left over. That argument is assigned every
#' unnamed command line value, in order, as a character
#' vector. The vector can have any length, including zero,
#' and its length need not match the default.
#'
#' If, for instance, the script `combine.R` contains
#'
#' ```
#' cmd_assign_dots(files = c("data/a.csv", "data/b.csv", "out/combined.rds"),
#'                 use_log = TRUE)
#' inputs <- head(files, -1)
#' out <- tail(files, 1)
#' ```
#'
#' and if `combine.R` is run from the command line using
#'
#' ```
#' Rscript combine.R data/a.csv data/c.csv out/combined.rds --use_log=TRUE
#' ```
#'
#' then `files` is `c("data/a.csv", "data/c.csv", "out/combined.rds")`
#' and `use_log` is `TRUE`. `files` was not passed as
#' `--files=...`. Leaving more than one argument unnamed
#' on the command line is an error, as is passing every
#' argument by name. The argument that remains must be
#' character.
#'
#' Values passed at the command line are not expanded.
#' A shell or Makefile that uses a glob expands it
#' before `Rscript` runs, and `cmd_assign_dots()`
#' receives one string per path.
#'
#' Interactive use assigns the values written in the call.
#'
#' # File names
#'
#' `cmd_assign_dots()` does not require the name of the
#' character vector to start with a dot.
#' The leading dot is a convention used by
#' [extract_make()], [extract_shell()], [makefile()],
#' and [shell_script()] when they decide which
#' arguments are file names:
#'
#' - If any argument name starts with a dot, those
#'   arguments are the file names.
#' - Otherwise an argument is a file name when the
#'   file exists, or when the value looks like a path.
#'
#' Dotted names such as `.data` and `.out` make that
#' choice explicit. They are not required when the
#' script is run.
#'
#' For `cmd_assign_dots()`, a character vector is handled
#' in the same way. If its name starts with a dot, or if
#' no argument name starts with a dot and each element
#' looks like a path, every element is a file name.
#' In a Makefile rule, the last element is the target
#' and the earlier elements are prerequisites.
#'
#' @param ... Name-value pairs. Exactly one argument
#' receives the unnamed command line values. That
#' argument must be character, and can have any length.
#' Every other argument must have length 1, and on the
#' command line must be passed by name.
#'
#' @returns `cmd_assign_dots()` is called
#' for its side effect, which is to create objects
#' in the global environment. However, `cmd_assign_dots()`
#' also invisibly returns a named list of objects.
#'
#' @seealso
#' - [cmd_assign()] Process command line arguments
#'   with one value per argument
#' - [extract_shell()] Turn a `cmd_assign()` or
#'   `cmd_assign_dots()` call into a shell command
#' - [extract_make()] Turn a `cmd_assign()` or
#'   `cmd_assign_dots()` call into a Makefile rule
#' - [shell_script()] Create a shell script
#' - [makefile()] Create a Makefile
#' - [Quick Start](https://bayesiandemography.github.io/command/articles/quickstart.html)
#'   How to use `cmd_assign()`
#'
#' @examples
#' if (interactive()) {
#'   cmd_assign_dots(files = c("data/a.csv", "data/b.csv", "out/combined.rds"),
#'                   use_log = TRUE)
#' }
#' @export
cmd_assign_dots <- function(...) {
  envir <- parent.frame()
  args_dots <- list(...)
  check_args_dots(args_dots, variadic = TRUE)
  if (interactive())
    args <- args_dots # nocov
  else {
    args_cmd <- get_args_cmd()
    check_args_cmd_dots(args_cmd = args_cmd,
                        args_dots = args_dots)
    args_cmd <- align_cmd_dots(args_cmd = args_cmd,
                               args_dots = args_dots)
    args_cmd <- coerce_to_dots_class(args_cmd = args_cmd,
                                     args_dots = args_dots)
    args <- args_cmd
  }
  assign_args(args = args,
              envir = envir,
              quiet = FALSE)
}

