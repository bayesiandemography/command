

#' Turn a Call to 'cmd_assign()' Into a
#' Rule to Use in a Makefile
#'
#' `extract_cmd_make()` locates a call to
#' `cmd_assign()` within an R code file,
#' constructs a Makefile rule to use with that
#' call, and prints the rule.
#'
#' # The components of a Makefile rule
#'
#' A typical rule produced by `extract_cmd_make()`
#' looks like this:
#'
#' ```
#' out/model.rds: src/model.R \
#'   data/timeseries.rds
#'        Rscript $^ $@ --use_log=TRUE \
#'                      --date=2021-07-01
#' ```
#'
#' In this rule
#' - `out/model.rds` is the 'target', the file that the rule creates
#' - `src/model.R` and `data/timeseries.rds` are "prerequisites",
#'   ie files that are used to create the target.
#' - `\` is a "line continuation character" that tells `make` to
#'   treat the next line as if it was part of the current line.
#' - `        Rscript $^ $@` is the first part of a 'recipe', ie
#'   the instructions to create the target. `make` knows that this
#'   is a recipe because the line starts with a tab.
#'   `$^` and `$@` are an [automatic variables](https://www.gnu.org/software/make/manual/make.html#Automatic-Variables)
#'   meaning "all the prerequisites" and "the target", implying that `Rscript $^ $@`
#'   is equivalent to `Rscript src/model.R data/timeseries out/model.rds`.
#' - `--use_log=TRUE` and `--date=2021-07-01` are also part of the
#'   recipe. They are options Rscript passes to `src/model.R
#'
#'
#' # How to use `extract_cmd_make()`.
#'
#' - Step 1. Write and test the R file
#'   will be appear in the Makefile rule (eg `model.R`
#'   in the example above.) When doing the writing and
#'   testing, use [cmd_assign()] interactively.
#' - Step 2. Once the R file is working correctly,
#'   call `extract_cmd_make()`, and add the results
#'   to your Makefile.
#'
#' # How `extract_cmd_make()` identifies file arguments
#'
#' To construct the rule, `extract_cmd_make()` needs to
#' be able to distinguish between arguments
#' in the call to `cmd_assign()` which refer to a
#' file name (eg `.data = mydata.csv`) and 
#' arguments which refer to something else
#' (eg `use_log = FALSE`).
#'
#' `extract_cmd_make()` uses the following heuristic
#'  to identify file arguments:
#' - if any arguments start with a dot (eg `.data = mydata.csv`)
#'   then assume that all such arguments are file arguments;
#' - otherwise, find values that that actually are file names
#'   (as determined by [base::file.exists()]) or that look
#'   like they could be.
#' 
#' @param file Path to an R script that
#' contains a call to [cmd_assign()].
#'
#' @returns `extract_cmd_make()` is typically called
#' for its side effect, which is to print a
#' Makefile rule. However, `extract_cmd_make()`
#' invisibly returns a text string with the rule.
#'
#' @seealso
#' - [extract_cmd_shell()] Turn a call to [cmd_assign()]
#'   into a command to use in a shell script
#' - [makefile()] Create a makefile
#' - [cmd_assign()] Process command line arguments
#' - [Quick Start Guide](https://bayesiandemography.github.io/command/articles/quickstart.html)
#'   How to use `cmd_assign()`
#' - [A Workflow for Data Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
#'   Build data analysis pipelines using `cmd_assign()`.
#' - [Project Management with Make](https://jeroenjanssens.com/dsatcl/chapter-6-project-management-with-make)
#'   Introduction to using `make` in data analysis pipelines.
#' - [GNU make](https://www.gnu.org/software/make/manual/make.html#SEC_Contents)
#'   The definitive guide.
#'
#' @examples
#' file <- system.file("extdata/airmiles/fig_smoothed.R",
#'                     package = "command")
#' extract_cmd_make(file)
#' @export
extract_cmd_make <- function(file) {
  check_is_r_code(file)
  args <- extract_args(file)
  ans <- format_args_make(file = file, args = args)
  cat(ans)
  invisible(ans)
}

extract_cmd_shell <- function(file) {
  check_is_r_code(file)
  args <- extract_args(file)
  ans <- format_args_shell(file = file, args = args)
  cat(ans)
  invisible(ans)
}  

