
#' Assign Values Passed at the Command Line or Interactively
#'
#' @description
#' Assign names to values in the global environment.
#' The values are typically supplied through the command line,
#' but can be supplied interactively.
#'
#' `cmd_assign_quiet()` is identical to
#' `cmd_assign()` except that it does not print
#' progress messages to the console.
#'
#' @details
#' `cmd_assign()` is a user-friendly alternative to [commandArgs()],
#' designed specifically for data analysis workflows.
#' 
#' `cmd_assign()` can be used to record
#' and enforce assumptions about inputs and outputs.
#' This provides scripts with better-defined
#' interfaces, making them clearer and safer.
#'
#' # Types of session
#'
#' `cmd_assign()` behaves differently depending
#' on whether it is called interactively, or
#' from within an R script being run from the command line.
#'
#' For instance, if the code
#'
#' `cmd_assign(use_log = TRUE)`
#'
#' is run interactively, it creates an object 
#' called `use_log` with value `TRUE`.
#'
#' But if the same code is run from within a
#' script that has been invoked using the command
#' 
#' `Rscript tidy_data.R --use_log=FALSE`
#' 
#' it creates an object called `use_log`
#' with value `FALSE`.
#'
#' `cmd_assign()` is typically called interactively
#' when a workflow is being developed,
#' and through the command line when the
#' workflow is in production.
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
#' cmd_assign(
#'   .data = "raw_data.csv",
#'    max_age = 85,
#'    .out = "person.rds"
#' )
#' ```
#'
#' and if `person.R` is run from the command line using
#'
#' ```
#' Rscript person.R \
#'   raw_data.csv \
#'   person.rds \
#'   --max_age=100
#' ```
#' 
#' then `cmd_assign()` assigns `cmd_assign()` names to
#' command line arguments in the following order:
#'
#' | order | `cmd_assign()` name   | command line argument   | 
#' |:------|:--------------------- |:------------------------|
#' | 1     | `max_age`             | `--max_age=100`         | 
#' | 2     | `.data`               | `raw_data.csv`          |
#' | 3     | `.out`                | `person.rds`            |
#'
#' 
#' # The values specified in the call to cmd_assign() are not defaults
#'
#' The values specified in the call to `cmd_assign()` are
#' not defaults in the sense of values to fall back on
#' when no value is supplied. For instance, if we take a script
#' containing the call
#'
#' ```
#' cmd_assign(
#'   n_draw = 10,
#'   .out = "simulation.rds"
#' )
#' ```
#'
#' and run it from the command line without passing a value
#' for `n_draw`, then `cmd_assign()` will throw an error.
#' It will *not* set `n_draw` to `10`.
#'
#' Defaults can  hide bugs and make code less predictable.
#'
#' # Coercing values passed at the command line
#' 
#' Values passed at the command line start out as
#' text strings. `cmd_assign()` coerces these strings
#' to have the same class as the corresponding values
#' in the call to `cmd_assign()`. For instance,
#' if script `fit.R` contains the lines
#' 
#' ```
#' cmd_assign(
#'   .data = "data/individuals.rds",
#'   impute = TRUE,
#'   date = as.Date("2025-01-01"),
#'   .out = "output/fitted.rds"
#' )
#' ```
#'
#' and if `fit.R` is run from the command line using
#'
#' ```
#' Rscript src/fit.R \
#'   data/individuals.rds \
#'   output/fitted.rds \
#'   --impute=TRUE \
#'   --date=2025-01-01
#' ```
#'
#' then `cmd_assign()` will create the following length-1 vectors
#'
#' | name | class     | value         |
#' |:-----|:----------|:--------------|
#' |`.data` | `"character"` | `"data/individuals.rds"` |
#' | `impute` | `"logical"` | `TRUE` |
#' | `date` | `"Date"` | `"2025-01-01"` |
#' | `.out` | `"character"` | `"output/fitted.rds"` |
#'
#' # The dot-name convention
#'
#' When naming arguments to `cmd_assign()`, it is typically
#' good practice to use names starting with dots for file paths,
#' and names without dots for ordinary objects, as in
#' ```
#' cmd_assign(
#'   .households = "data/households.csv",
#'   n_record = 500,
#'   sex = "Female",
#'   .out = "out/subsample_female.rds"
#' )
#' ```
#'
#' Function `cmd_assign()` does not in fact distinguish between
#' names with dots and names without dots. But the dot-name convention
#' is still useful because of the way it calls attention
#' to the distinction between file paths and
#' ordinary objects. For instance, a dot-name is a
#' reminder that the object being named is a reference to
#' an input or output, rather than the input or output itself.
#'
#' In contrast, functions [extract_make()],
#' [extract_shell()], [makefile()], and [shell_script()]
#' *do* treat names with dots differently from
#' names without dots. See the help for
#' [extract_make()], [extract_shell()],
#' [makefile()], and [shell_script()]
#' for details.
#' 
#' @param ... Name-value pairs.
#' 
#' @returns `cmd_assign()` is called
#' for its side effect, which is to create objects
#' in the global environment. However, `cmd_assign()`
#' also invisibly returns a named list of objects.
#'
#' @seealso
#' - [cmd_assign_collect()] Collect unnamed command line arguments
#'   into one character vector
#' - [extract_shell()] Turn a `cmd_assign()` call into a shell command
#' - [extract_make()] Turn a `cmd_assign()` call into a Makefile rule
#' - [shell_script()] Create a shell script
#' - [makefile()] Create a Makefile
#' - [use_renv()] Activate a renv project before loading packages
#' - [Quick Start](https://bayesiandemography.github.io/command/articles/a1_quickstart.html)
#'   How to use `cmd_assign()`
#' - [Modular Workflows for Data Analysis](https://bayesiandemography.github.io/command/articles/a4_workflow.html)
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


#' Assign Varying Set of Values Passed at the Command Line or Interactively
#'
#' @description
#' Assign names to values in the global environment,
#' like [cmd_assign()], but without specifying the number
#' of values in advance. Values for unnamed command
#' line arguments are collected into a single character vector.
#'
#' `cmd_assign_collect_quiet()` is identical to
#' `cmd_assign_collect()` except that it does not print
#' progress messages to the console.
#'
#' @details
#' # Matching names and values
#'
#' `cmd_assign_collect()`, like [cmd_assign()],
#' starts by processing named command line arguments
#' (i.e. arguments of the form `--n_draw=100`).
#' Unlike `cmd_assign()`, however, `cmd_assign_collect()`
#' puts all the values that are left over into a single
#' character vector. This vector can be any length,
#' including zero. `cmd_assign_collect()` must
#' contain a single name-value pair that is not
#' used for named command line arguments.
#' `cmd_assign_collect()` assigns the name from
#' this name-value to the character vector.
#'
#' If, for instance, the script `combine.R` contains
#'
#' ```
#' cmd_assign_collect(
#'   .files = c("data/a.csv", "data/b.csv", "out/combined.rds"),
#'   use_log = TRUE
#' )
#' ```
#'
#' and if `combine.R` is run from the command line using
#'
#' ```
#' Rscript combine.R \
#'   data/a.csv \
#'   data/c.csv \
#'   out/combined.rds \
#'   --use_log=TRUE
#' ```
#'
#' then `use_log` will be assigned the value `TRUE`
#' and `.files` will be assigned the value
#' `c("data/a.csv", "data/c.csv", "out/combined.rds")`.
#'
#' # Processing the vector of unnamed arguments
#'
#' The R script in which `cmd_assign_collect()` is
#' called has responsibility for processing the
#' character vector of values from unnamed command
#' line arguments. The call to `cmd_assign_collect()` above,
#' for instance, might be followed by statements such as
#' ```
#' .inputs <- head(.files, -1)
#' .out <- tail(.files, 1)
#' inputs <- lapply(.inputs, read.csv)
#' ```
#'
#' # Use cmd_assign() where possible
#'
#' `cmd_assign_collect()` is more flexible than `cmd_assign()`,
#' in that it does not require the number, name, or type of values
#' to be specified in advance. But if these things
#' are in fact known in advance, then it is
#' good practice to document and test for them,
#' which is what `cmd_assign()` does.
#'
#' @param ... Name-value pairs.
#'
#' @returns `cmd_assign_collect()` is called
#' for its side effect, which is to create objects
#' in the global environment. However, `cmd_assign_collect()`
#' also invisibly returns a named list of objects.
#'
#' @seealso
#' - [cmd_assign()] Process command line arguments
#'   with one value per argument
#' - [extract_shell()] Turn a `cmd_assign()` or
#'   `cmd_assign_collect()` call into a shell command
#' - [extract_make()] Turn a `cmd_assign()` or
#'   `cmd_assign_collect()` call into a Makefile rule
#' - [shell_script()] Create a shell script
#' - [makefile()] Create a Makefile
#' - [Quick Start](https://bayesiandemography.github.io/command/articles/a1_quickstart.html)
#'   How to use `cmd_assign()`
#'
#' @examples
#' if (interactive()) {
#'   cmd_assign_collect(
#'     files = c("data/x.csv", "data/y.csv"),
#'     use_log = TRUE
#'   )
#' }
#' @export
cmd_assign_collect <- function(...) {
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

#' @export
#' @rdname cmd_assign_collect
cmd_assign_collect_quiet <- function(...) {
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
              quiet = TRUE)
}

