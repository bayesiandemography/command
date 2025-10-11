
#' Create a Shell Script
#'
#' Create a shell script for a data analysis workflow
#' consisting of commands
#' extracted from existing R files.
#'
#' To create a shell script in the `files`
#' directory, set `files` to `"."`.
#'
#' To obtain the contents of the shell script
#' without creating a file on disk,
#' creating the file on disk, set
#' `name_shell` to `NULL`.
#'
#' Supplying a value for `files` is
#' compulsory for `shell_script()`,
#' but optional for [makefile()].
#' The output from `shell_script()`
#' is generated entirely from `files`
#' while the output from [makefile()]
#' also includes some general-purpose Makefile
#' commands.
#'
#' @param path_files A path from `dir_shell` to a
#' directory with R scripts containing
#' calls to [cmd_assign()].
#' @param dir_shell The directory where
#' `shell_script()` will create the shell script.
#' If no value is supplied, then `shell_script()`
#' creates the shell script in the current working directory.
#' @param name_shell The name of the shell script.
#' The default is `"workflow.sh"`.
#' @param overwrite Whether to overwrite
#' an existing shell script. Default is `FALSE`.
#' @param quiet Whether to suppress
#' progress messages. Default is `FALSE`.
#' 
#' @returns `shell_script()` is called for its
#' side effect, which is to create a
#' file. However, `shell_script()` also
#' returns a string with the contents of the
#' shell script.
#'
#' @seealso
#' - [Creating a Shell Script](https://bayesiandemography.github.io/command/articles/a2_shell_script.html) More on `shell_script()`
#' - [extract_shell()] Turn a [cmd_assign()] call into a shell command
#' - [makefile()] Makefile equivalent of `shell_script()`
#' - [cmd_assign()] Process command line arguments
#' - [Modular Workflows for Data Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
#'   Safe, flexible data analysis workflows
#' - [littler](https://CRAN.R-project.org/package=littler) Alternative to Rscript
#'
#' @references
#' - Episodes 1--3 of [The Unix Shell](https://swcarpentry.github.io/shell-novice/index.html)
#'   Introduction to the command line
#' - [Command-Line Programs](https://swcarpentry.github.io/r-novice-inflammation/05-cmdline.html)
#'   Introduction to Rscript
#'
#' @examples
#' library(fs)
#' library(withr)
#'
#' with_tempdir({
#'
#'   ## create 'src'  directory
#'   dir_create("src")
#'
#'   ## put R scripts containing calls to
#'   ## 'cmd_assign' in the 'src' directory
#'   writeLines(c("cmd_assign(x = 1, .out = 'out/results.rds')",
#'                "results <- x + 1",
#'                "saveRDS(results, file = .out)"),
#'              con = "src/results.R")
#'   writeLines(c("cmd_assign(x = 1, .out = 'out/more_results.rds')",
#'                "more_results <- x + 2",
#'                "saveRDS(more_results, file = .out)"),
#'              con = "src/more_results.R")
#'
#'   ## call 'shell_script()'
#'   shell_script(path_files = "src",
#'                dir_shell = ".")
#'
#'   ## shell script has been created
#'   dir_tree()
#'
#'   ## print contents of shell script
#'   cat(readLines("workflow.sh"), sep = "\n")
#' 
#' })
#' @export
shell_script <- function(path_files,
                         dir_shell = NULL,
                         name_shell = "workflow.sh",
                         overwrite = FALSE,
                         quiet = FALSE) {
  has_dir_shell <- !is.null(dir_shell)
  if (has_dir_shell)
    check_dir(dir_shell, nm = "dir_shell")
  else
    dir_shell <- getwd()
  check_path_files_valid(path_files = path_files,
                         dir = dir_shell,
                         nm_dir_arg = "dir_shell",
                         has_dir_arg = has_dir_shell)
  has_name_shell <- !is.null(name_shell)
  if (has_name_shell)
    check_valid_filename(x = name_shell,
                         nm = "name_shell")
  check_flag(x = overwrite,
             nm = "overwrite")
  lines <- ""
  commands <- make_commands(path_files = path_files,
                            dir_shell = dir_shell,
                            quiet = quiet)
  if (length(commands) > 0L)
    lines <- c(lines,
               commands)
  lines <- c(lines,
             "")
  if (has_name_shell) {
    path_shell <- fs::path(dir_shell, name_shell)
    if (fs::file_exists(path_shell) && !overwrite)
      cli::cli_abort(c(paste("Directory {.file {dir_shell}} already contains a",
                             "file called {.file {name_shell}}."),
                       i = "To overwrite an existing file, set {.arg overwrite} to {.val {TRUE}}."))
    writeLines(lines, con = path_shell)
  }
  lines <- paste(lines, collapse = "\n")
  invisible(lines)
}



      

               
  
