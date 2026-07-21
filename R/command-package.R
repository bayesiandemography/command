
#' command: Process command line arguments
#'
#' Process command line arguments, allowing scripts to
#' behave like functions, with well-defined inputs and outputs.
#' Helps make data analysis workflows more modular,
#' and therefore more transparent, flexible, and reliable.
#'
#' - [cmd_assign()] Process command line arguments
#' - [extract_shell()] Turn a `cmd_assign()` call into a shell command
#' - [extract_make()] Turn a `cmd_assign()` call into a Makefile rule
#' - [shell_script()] Create a shell script
#' - [makefile()] Create a Makefile
#' - [Quick Start](https://bayesiandemography.github.io/command/articles/quickstart.html)
#'   How to use `cmd_assign()`
#' - [Modular Workflows for Data Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
#'   Safe, flexible data analysis workflows
#' 
#' @docType package
#' @name command-package
#' @aliases command
"_PACKAGE"
utils::globalVariables("argv")
## usethis namespace: start
## usethis namespace: end
NULL
