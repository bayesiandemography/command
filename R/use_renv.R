
## HAS_TESTS
#' Directory Containing the Current Script, If Known
#'
#' When the session was started with `Rscript path/to/file.R`,
#' returns the directory of that file. Otherwise returns `NULL`.
#'
#' @returns A length-1 character string, or `NULL`
#'
#' @noRd
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg))
    return(NULL)
  path <- sub("^--file=", "", file_arg[[1L]])
  ## Rscript may pass a path that does not yet exist as normalized;
  ## dirname still gives the containing directory string.
  dir <- dirname(path)
  if (!nzchar(dir) || identical(dir, "."))
    dir <- getwd()
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}


## HAS_TESTS
#' Walk Up From a Directory Looking for a renv Project Root
#'
#' A directory is treated as a renv root if it contains
#' `renv/activate.R` or `renv.lock`.
#'
#' @param start Directory to start from
#' @param max_depth Maximum number of parents to visit
#'
#' @returns Normalized path to the root, or `NULL`
#'
#' @noRd
find_renv_root <- function(start, max_depth = 20L) {
  dir <- normalizePath(start, winslash = "/", mustWork = FALSE)
  if (!dir.exists(dir) && file.exists(dir))
    dir <- dirname(dir)
  for (i in seq_len(max_depth)) {
    if (!dir.exists(dir))
      return(NULL)
    if (file.exists(file.path(dir, "renv", "activate.R")) ||
        file.exists(file.path(dir, "renv.lock")))
      return(normalizePath(dir, winslash = "/", mustWork = TRUE))
    parent <- dirname(dir)
    if (identical(parent, dir))
      return(NULL)
    dir <- parent
  }
  NULL
}


## HAS_TESTS
#' Is renv Already Active for This Project?
#'
#' @param project Normalized project root
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
renv_active_for <- function(project) {
  env_proj <- Sys.getenv("RENV_PROJECT", unset = "")
  if (!nzchar(env_proj))
    return(FALSE)
  env_proj <- normalizePath(env_proj, winslash = "/", mustWork = FALSE)
  identical(env_proj, project)
}


#' Activate a renv Project for the Current Session
#'
#' Call this as the first line of a pipeline script run with
#' `Rscript` (or littler), **before** any `library()` calls.
#' `Rscript` does not source `.Rprofile`, so renv is otherwise
#' not activated and packages may be loaded from the wrong library.
#'
#' `use_renv()` looks for a renv project root, and if one is found
#' and not already active, sources `renv/activate.R`. If no renv
#' project is found, it does nothing. Non-renv projects are fine.
#'
#' @param project Optional path to a project root. If `NULL`
#'   (the default), search upward from the script's directory
#'   when known, otherwise from `getwd()`.
#' @param quiet If `TRUE` (the default), suppress messages.
#'
#' @returns The project root path (invisibly), or `NULL` if
#'   no renv project was activated.
#'
#' @details
#' # Finding the `command` package
#'
#' To call `command::use_renv()` before `library(command)`,
#' `command` must already be findable—typically because it is
#' installed in your user or system library (e.g. from CRAN).
#' That is the usual setup for a CRAN package used across projects.
#'
#' # What `use_renv()` does not do
#'
#' It does not run `renv::restore()`, install packages, or
#' activate other environment managers. It does not run
#' automatically inside [cmd_assign()].
#'
#' @seealso
#' - [cmd_assign()] Process command line arguments
#' - [Using command with renv](https://bayesiandemography.github.io/command/articles/a5_renv.html)
#' - [renv](https://rstudio.github.io/renv/)
#'
#' @examples
#' \dontrun{
#' # At the top of a pipeline script:
#' command::use_renv()
#'
#' library(dplyr)
#' library(command)
#'
#' cmd_assign(.data = "data/cleaned.rds",
#'            .out = "out/model.rds")
#' }
#' @export
use_renv <- function(project = NULL, quiet = TRUE) {
  check_flag(x = quiet, nm = "quiet")
  if (is.null(project)) {
    start <- script_dir()
    if (is.null(start))
      start <- getwd()
    root <- find_renv_root(start)
  } else {
    if (!identical(length(project), 1L) || !is.character(project) || is.na(project))
      cli::cli_abort(c("{.arg project} must be a single character string.",
                       i = "{.arg project} has class {.cls {class(project)}}."))
    if (!dir.exists(project))
      cli::cli_abort(c("Can't find project directory.",
                       i = "Directory: {.path {project}}"))
    root <- normalizePath(project, winslash = "/", mustWork = TRUE)
    has_activate <- file.exists(file.path(root, "renv", "activate.R"))
    has_lock <- file.exists(file.path(root, "renv.lock"))
    if (!has_activate && !has_lock) {
      if (!quiet)
        cli::cli_alert_warning("No renv project found at {.path {root}}.")
      return(invisible(NULL))
    }
  }

  if (is.null(root)) {
    if (!quiet)
      cli::cli_alert_info("No renv project found.")
    return(invisible(NULL))
  }

  if (renv_active_for(root))
    return(invisible(root))

  activate <- file.path(root, "renv", "activate.R")
  if (!file.exists(activate)) {
    if (!quiet)
      cli::cli_alert_warning(
        "Found {.file renv.lock} but no {.file renv/activate.R} at {.path {root}}."
      )
    return(invisible(root))
  }

  source(activate, local = FALSE)
  if (!quiet)
    cli::cli_alert_success("Activated renv project at {.path {root}}.")
  invisible(root)
}
