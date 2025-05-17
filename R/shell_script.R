
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
#' @param files A path from `dir_shell` to a
#' directory with R files that have calls to [cmd_assign()].
#' @param dir_shell The directory where
#' `shell_script()` will create the shell script.
#' If no value is supplied, then `shell_script();
#' creates the shell script the current working directory.
#' @param name_shell The name of the shell script.
#' The default is `"workflow.sh"`.
#' 
#' @returns `shell_script()` is called for its
#' side effect, which is to create a
#' file. However, `shell_script()` also
#' returns a string with the text for a
#' shell script.
#'
#' @seealso
#' - [cmd_shell()] Turn a [cmd_assign()] call
#'   into a shell command
#' - [makefile()] Create a Makefile
#'   for a data analysis workflow
#' - [cmd_assign()] Process command line arguments
#' - [A Workflow for Data Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
#'   Data analysis workflows using `cmd_assign()`.
#' - Episodes 1--3 of [The Unix Shell](https://swcarpentry.github.io/shell-novice/index.html)
#'   Introduction to the command line
#' - [Command-Line Programs](https://swcarpentry.github.io/r-novice-inflammation/05-cmdline.html)
#'   Introduction to Rscript
#'
#' @examples
#' library(fs)
#'
#' ## Create project directory containing
#' ## 'src' and 'out' directories
#' path_project <- file_temp()
#' dir_create(path_project)
#' dir_create(path(path_project, "src"))
#' dir_create(path(path_project, "out"))
#' 
#' ## Put R code file in 'src' directory
#' writeLines(c("cmd_assign(x = 1, .out = 'out/results.rds')",
#'              "results <- x + 1",
#'              "saveRDS(results, file = .out)"),
#'            con = path(path_project, "src/results.R"))
#'
#' ## Look at directories
#' dir_tree(path_project)
#'
#' ## Call 'shell_script()'
#' shell_script(files = "src",
#'              dir_shell = path_project)
#'
#' ## Look at directories
#' dir_tree(path_project)
#'
#' ## Look at contents of shell script
#' lines <- readLines(path(path_project, "workflow.sh"))
#' cat(paste(lines, collapse = "\n"))
#'
#' ## Get the text of the shell script
#' ## without creating a file on disk
#' text <- shell_script(files = "src",
#'                      dir_shell = path_project,
#'                      name_shell = NULL)
#' cat(text)
#'
#' ## Clean up
#' dir_delete(path_project)
#' @export
shell_script <- function(files,
                         dir_shell = NULL,
                         name_shell = "workflow.sh") {
  has_dir_shell <- !is.null(dir_shell)
  if (has_dir_shell)
    check_dir(dir_shell, nm = "dir_shell")
  else
    dir_shell <- getwd()
  check_files_exists(files = files,
                     dir = dir_shell,
                     nm_dir = "dir_shell",
                     has_dir_arg = has_dir_shell)
  has_name_shell <- !is.null(name_shell)
  if (has_name_shell)
    check_valid_filename(x = name_shell,
                         nm = "name_shell")
  lines <- ""
  commands <- make_commands(files = files,
                            dir_shell = dir_shell)
  if (length(commands) > 0L)
    lines <- c(lines,
               commands)
  lines <- c(lines,
             "")
  if (has_name_shell) {
    path_shell <- fs::path(dir_shell, name_shell)
    writeLines(lines, con = path_shell)
  }
  lines <- paste(lines, collapse = "\n")
  invisible(lines)
}



      

               
  
