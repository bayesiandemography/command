
## HAS_TESTS
#' Turn a 'cmd_assign' Call into a Shell Command
#'
#' Construct a shell command from a call to
#' [cmd_assign()] inside an R file.
#'
#' The shell script for a data analysis workflow
#' normally goes in the project directory.
#'
#' # The components of a shell command
#'
#' A shell command produced by `cmd_make()`
#' normally looks something like
#' ```
#' Rscript src/model.R \
#'   data/cleaned.rds \
#'   out/model.rds \
#'   --use_log=TRUE
#' ```
#'
#' In this command
#' - `Rscript` is a call to [utils::Rscript()];
#' - `\` is a "line continuation character";
#' - `data/cleaned.rds` and `out/model.rds` are unnamed
#'   arguments that Rscript passes to `src/model.R`; and
#' - `--use_log=TRUE` is a named argument that
#'   Rscript passes to `src/model.R`
#'
#' # Using `cmd_shell()` to build a data analysis workflow
#'
#' - Step 1. Write the R file that carries out
#'   the step in analysis (eg tidying data, fitting
#'   a model, making a graph.) This file
#'   will contain a call to [cmd_assign()],
#'   and  will be the first argument passed to
#'   Rscript in the shell command.
#'   When writing and testing the file,
#'   use [cmd_assign()] interactively.
#' - Step 2. Once the R file is working correctly,
#'   call `cmd_shell()`, and add the command
#'   to your shell script.
#'
#' # Identifying file arguments
#'
#' To construct the rule, `cmd_shell()` needs to
#' be able to identify arguments that refer to a
#' file name. To do so, it uses the following heuristic:
#' - if the call includes arguments whose names start with
#'   a dot, then these arguments are assumed to
#'   refer to file names;
#' - otherwise, find arguments whose values
#'   actually are file names
#'   (as determined by [file.exists()]) or that look
#'   like they could be.
#' 
#' @param file Path to the R code file containing
#' the call to [cmd_assign()]. The path
#' starts at `dir_shell`.
#' @param dir_shell The directory that contains
#' the shell script. The default is
#' the current working directory.
#'
#' @returns `cmd_shell()` is typically called
#' for its side effect, which is to print a
#' shell command. However, `cmd_shell()`
#' invisibly returns a text string with the command.
#'
#' @seealso
#' - [cmd_make()] Makefile equivalent of `cmd_shell()`
#' - [shell_script()] Create a shell script
#'   from calls to [cmd_assign()]
#' - [cmd_assign()] Process command line arguments
#' - [Quick Start Guide](https://bayesiandemography.github.io/command/articles/quickstart.html)
#'   How to use `cmd_assign()`
#' - [Data Analysis Workflows](https://bayesiandemography.github.io/command/articles/workflow.html)
#'   Safe, flexible workflows using `cmd_assign()`
#' - Episodes 1--3 of [The Unix Shell](https://swcarpentry.github.io/shell-novice/index.html)
#'   Introduction to the command line
#' - [Command-Line Programs](https://swcarpentry.github.io/r-novice-inflammation/05-cmdline.html)
#'   Introduction to Rscript
#' - [littler](https://CRAN.R-project.org/package=littler) Alternative to Rscript
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
#' ## Put R file in 'src' directory
#' writeLines(c("cmd_assign(x = 1, .out = 'out/results.rds')",
#'              "results <- x + 1",
#'              "saveRDS(results, file = .out)"),
#'            con = path(path_project, "src/results.R"))
#'
#' ## Look at directories
#' dir_tree(path_project)
#'
#' ## Look at contents of R file
#' lines <- readLines(path(path_project, "src/results.R"))
#' cat(paste(lines, collapse = "\n"))
#'
#' ## call 'cmd_shell()'
#' cmd_shell(file = "src/results.R",
#'           dir_shell = path_project)
#'
#' ## clean up
#' dir_delete(path_project)
#' @export
cmd_shell <- function(file, dir_shell = NULL) {
  has_dir_arg <- !is.null(dir_shell)
  if (has_dir_arg)
    check_dir(dir = dir_shell,
              nm = "dir_shell")
  else
    dir_shell <- getwd()
  check_file_exists(file = file,
                    dir = dir_shell,
                    nm_dir = "dir_shell",
                    has_dir_arg = has_dir_arg)
  path_file <- fs::path(dir_shell, file)
  check_is_r_code(path_file)
  args <- extract_args(path_file)
  ans <- format_args_shell(file = file,
                           args = args)
  cat(ans)
  invisible(ans)
}  

