
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
#' @param path_files A path from `dir_make` to a
#' directory with R scripts containing
#' calls to [cmd_assign()].
#' Optional.
#' @param dir_make The directory where
#' `makefile()` will create the Makefile.
#' If no value is supplied, then `makefile();
#' creates the Makefile the current working directory.
#' @param name_make The name of the Makefile.
#' The default is `"Makefile"`.
#' @param overwrite Whether to overwrite
#' an existing Makefile. Default is `FALSE`.
#' @param quiet Whether to suppress
#' progress messages. Default is `FALSE`.
#' 
#' @returns `makefile()` is called for its
#' side effect, which is to create a
#' file. However, `makefile()` also
#' returns a string with the contents of the
#' Makefile.
#'
#' @seealso
#' - [Creating a Makefile](https://bayesiandemography.github.io/command/articles/a3_makefile.html)
#'   More on `makefile()`
#' - [extract_make()] Turn a [cmd_assign()] call into a Makefile rule
#' - [shell_script()] Shell script equivalent of `makefile()`
#' - [cmd_assign()] Process command line arguments
#' - [Modular Workflows for Data Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
#'   Safe, flexible data analysis workflows
#' - [littler](https://CRAN.R-project.org/package=littler) Alternative to Rscript
#'
#' @references
#' - [Project Management with Make](https://jeroenjanssens.com/dsatcl/chapter-6-project-management-with-make)
#'   Makefiles in data analysis workflows
#' - [GNU make](https://www.gnu.org/software/make/manual/make.html#SEC_Contents)
#'   Definitive guide
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
#'   ## call 'makefile()'
#'   makefile(path_files = "src",
#'            dir_make = ".")
#'
#'   ## Makefile has been created
#'   dir_tree()
#'
#'   ## print contents of Makefile
#'   cat(readLines("Makefile"), sep = "\n")
#' 
#' })
#' @export
makefile <- function(path_files = NULL,
                     dir_make = NULL,
                     name_make = "Makefile",
                     overwrite = FALSE,
                     quiet = FALSE) {
  has_dir_make <- !is.null(dir_make)
  if (has_dir_make)
    check_dir(dir_make, nm = "dir_make")
  else
    dir_make <- "."
  has_path_files <- !is.null(path_files)
  if (has_path_files)
    check_path_files_valid(path_files = path_files,
                           dir = dir_make,
                           nm_dir_arg = "dir_make",
                           has_dir_arg = has_dir_make)
  has_name_make <- !is.null(name_make)
  if (has_name_make)
    check_valid_filename(x = name_make,
                         nm = "name_make")
  check_flag(x = overwrite,
             nm = "overwrite")
  check_flag(x = quiet,
             nm = "quiet")
  lines <- c("",
             ".PHONY: all",
             "all:\n\n")
  if (has_path_files) {
    rules <- make_rules(path_files = path_files,
                        dir_make = dir_make,
                        quiet = quiet) 
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



      

               
  
