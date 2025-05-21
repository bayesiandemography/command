
## HAS_TESTS
#' Turn a 'cmd_assign' Call into a Makefile Rule
#'
#' Construct a Makefile rule from a call to
#' [cmd_assign()] inside an R file.
#'
#' The Makefile for a data analysis workflow
#' normally goes in the project directory.
#' 
#' # The components of a Makefile rule
#'
#' A Makefile rule produced by `cmd_make()`
#' normally looks something like
#' ```
#' out/model.rds: src/model.R \
#'   data/cleaned.rds
#'        Rscript $^ $@ --use_log=TRUE
#' ```
#'
#' In this rule
#' - `out/model.rds` is the "target", i.e. the file that the rule creates;
#' - `src/model.R` and `data/timeseries.rds` are "prerequisites",
#'   i.e. files that are used to create the target;
#' - `\` is a "line continuation character";
#' - `        ` at the start of the third line is a tab,
#'   telling `make` that the recipe for creating the target from
#'   the starts here;
#' - `Rscript` is a call to [utils::Rscript()];
#' - `$^` is an [automatic variable](https://www.gnu.org/software/make/manual/make.html#Automatic-Variables)
#'   meaning "all the prerequisites"
#'   and `$@` is an automatic variable meaning "the target",
#'   so that `Rscript $^ $@` expands to
#'   `Rscript src/model.R data/cleaned.rds out/model.rds`; and
#' - `--use_log=TRUE` is a named argument that
#'   Rscript passes to `src/model.R`
#'
#'
#' # Using `cmd_make()` to build a data analysis workflow
#'
#' - Step 1. Write the R file that carries out
#'   the step in analysis (eg tidying data, fitting
#'   a model, making a graph.) This file
#'   will contain a call to [cmd_assign()],
#'   and  will appear in the first line of the
#'   Makefile rule. When writing and testing the file,
#'   use [cmd_assign()] interactively.
#' - Step 2. Once the R file is working correctly,
#'   call `cmd_make()`, and add the rule
#'   to your Makefile.
#'
#' When using `cmd_make()`, it is a good idea to
#' set the current working directory to the project
#' directory (something that will happen automatically
#' if you are using RStudio projects.)
#'
#' # Identifying file arguments
#'
#' To construct the Makefile rule, `cmd_make()` needs to
#' be able to pick out arguments that refer to
#' file names. To do so, it uses the following heuristic:
#' - if the call includes arguments whose names start with
#'   a dot, then these arguments are assumed to
#'   refer to file names;
#' - otherwise, find arguments whose values
#'   actually are file names
#'   (as determined by [file.exists()]), or that look
#'   like they could be.
#' 
#' @param file Path to the R code file containing
#' the call to [cmd_assign()]. The path
#' starts at `dir_make`.
#' @param dir_make The directory that contains
#' the Makefile. The default is
#' the current working directory.
#'
#' @returns `cmd_make()` is typically called
#' for its side effect, which is to print a
#' Makefile rule. However, `cmd_make()`
#' invisibly returns a text string with the rule.
#'
#' @seealso
#' - [cmd_shell()] Shell script equivalent of `cmd_make()`
#' - [makefile()] Create a Makefile
#'   from calls to [cmd_assign()]
#' - [cmd_assign()] Process command line arguments
#' - [Quick Start Guide](https://bayesiandemography.github.io/command/articles/quickstart.html)
#'   How to use `cmd_assign()`
#' - [Data Analysis Workflows](https://bayesiandemography.github.io/command/articles/workflow.html)
#'   Safe, flexible workflows using `cmd_assign()`
#' - [Project Management with Make](https://jeroenjanssens.com/dsatcl/chapter-6-project-management-with-make)
#'   Makefiles in data analysis workflows
#' - [GNU make](https://www.gnu.org/software/make/manual/make.html#SEC_Contents)
#'   Definitive guide
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
#' ## call 'cmd_make()'
#' cmd_make(file = "src/results.R",
#'          dir_make = path_project)
#'
#' ## clean up
#' dir_delete(path_project)
#' @export
cmd_make <- function(file, dir_make = NULL) {
  has_dir_arg <- !is.null(dir_make)
  if (has_dir_arg)
    check_dir(dir = dir_make,
              nm = "dir_make")
  else
    dir_make <- getwd()
  check_file_exists(file = file,
                    dir = dir_make,
                    nm_dir = "dir_make",
                    has_dir_arg = has_dir_arg)
  path_file <- fs::path(dir_make, file)
  check_is_r_code(path_file)
  args <- extract_args(path_file)
  ans <- format_args_make(file = file,
                          args = args)
  cat(ans)
  invisible(ans)
}


