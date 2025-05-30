
#' Create a Makefile
#'
#' Create a Makefile for a data analysis workflow.
#' The Makefile can include
#' rules extracted from existing R files.
#'
#' To create a Makefile in the `files`
#' directory, set `files` to `"."`.
#'
#' To obtain the contents of the Makefile
#' without creating a file on disk,
#' creating the file on disk, set
#' `name_make` to `NULL`.
#'
#' Supplying a value for `files` is
#' optional for `makefile()`,
#' but compulsory for [shell_script()].
#' The output from `makefile()`
#' includes some general-purpose Makefile
#' commands, while the output from
#' [shell_script()] is generated entirely
#' from `files`.
#'
#' @param files A path from `dir_make` to a
#' directory with R files that have calls to [cmd_assign()].
#' Optional.
#' @param dir_make The directory where
#' `makefile()` will create the Makefile.
#' If no value is supplied, then `makefile();
#' creates the Makefile the current working directory.
#' @param name_make The name of the Makefile.
#' The default is `"Makefile"`.
#' @param overwrite Whether to overwrite
#' an existing file. Default is `FALSE`.
#' 
#' @returns `makefile()` is called for its
#' side effect, which is to create a
#' file. However, `makefile()` also
#' returns a string with the text for a
#' Makefile.
#'
#' @seealso
#' - [Creating a Makefile](https://bayesiandemography.github.io/command/articles/a3_makefile.html)
#'   More on `makefile()`
#' - [cmd_make()] Turn a [cmd_assign()] call into a Makefile rule
#' - [shell_script()] Shell script equivalent of `makefile()`
#' - [cmd_assign()] Process command line arguments
#' - [Data Analysis Workflows](https://bayesiandemography.github.io/command/articles/workflow.html)
#'   Safe, flexible workflows using `cmd_assign()`
#' - [Project Management with Make](https://jeroenjanssens.com/dsatcl/chapter-6-project-management-with-make)
#'   Makefiles in data analysis workflows
#' - [GNU make](https://www.gnu.org/software/make/manual/make.html#SEC_Contents)
#'   Definitive guide
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
#' ## Put R code file in 'src' directory
#' writeLines(c("cmd_assign(x = 1, .out = 'out/results.rds')",
#'              "results <- x + 1",
#'              "saveRDS(results, file = .out)"),
#'            con = path(path_project, "src/results.R"))
#'
#' ## Look at directories
#' dir_tree(path_project)
#'
#' ## Call 'makefile()'
#' makefile(files = "src",
#'          dir_make = path_project)
#'
#' ## Look at directories
#' dir_tree(path_project)
#'
#' ## Look at contents of makefile
#' lines <- readLines(path(path_project, "Makefile"))
#' cat(paste(lines, collapse = "\n"))
#'
#' ## Get the text of the Makefile
#' ## without creating a file on disk
#' text <- makefile(files = "src",
#'                  dir_make = path_project,
#'                  name_make = NULL)
#' cat(text)
#'
#' ## Clean up
#' dir_delete(path_project)
#' @export
makefile <- function(files = NULL,
                     dir_make = NULL,
                     name_make = "Makefile",
                     overwrite = FALSE) {
  has_dir_make <- !is.null(dir_make)
  if (has_dir_make)
    check_dir(dir_make, nm = "dir_make")
  else
    dir_make <- getwd()
  has_files <- !is.null(files)
  if (has_files)
    check_files_exists(files = files,
                       dir = dir_make,
                       nm_dir = "dir_make",
                       has_dir_arg = has_dir_make)
  has_name_make <- !is.null(name_make)
  if (has_name_make)
    check_valid_filename(x = name_make,
                         nm = "name_make")
  check_flag(x = overwrite,
             nm = "overwrite")
  lines <- c("",
             ".PHONY: all",
             "all:\n\n")
  if (has_files) {
    rules <- make_rules(files = files,
                        dir_make = dir_make) 
    if (length(rules) > 0L)
      lines <- c(lines,
                 rules,
                 "")
  }
  lines <- c(lines, 
             ".PHONY: clean",
             "clean:",
             "\trm -rf out",
             "\tmkdir out\n\n")
  if (has_name_make) {
    path_make <- fs::path(dir_make, name_make)
    if (fs::file_exists(path_make) && !overwrite)
      cli::cli_abort(c(paste("Directory {.file {dir_make}} already contains a",
                             "file called {.file {name_make}}."),
                       i = "To overwrite an existing file, set {.arg overwrite} to {.val {TRUE}}."))
    writeLines(lines, con = path_make)
  }
  lines <- paste(lines, collapse = "\n")
  invisible(lines)
}



      

               
  
