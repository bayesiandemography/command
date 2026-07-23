
## HAS_TESTS
#' Directory Containing the Current Script, If Known
#'
#' Tries, in order:
#' 1. `Rscript`'s `--file=` argument in [commandArgs()];
#' 2. under [littler](https://CRAN.R-project.org/package=littler), a
#'    walk of the call stack for `srcref` information
#'    (littler keeps source references; `Rscript` does not).
#'
#' Returns `NULL` if the script directory cannot be determined
#' (caller typically falls back to [getwd()]).
#'
#' @returns A length-1 character string, or `NULL`
#'
#' @noRd
script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  ## 1) Rscript --file=/path/to/script.R
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    path <- sub("^--file=", "", file_arg[[1L]])
    ## Rscript may pass a relative path; dirname still works.
    dir <- dirname(path)
    if (!nzchar(dir) || identical(dir, "."))
      dir <- getwd()
    return(normalizePath(dir, winslash = "/", mustWork = FALSE))
  }
  ## 2) littler: no --file=, but top-level expressions carry srcrefs
  if (length(args) >= 1L && identical(args[[1L]], "littler")) {
    dir <- script_dir_from_srcref()
    if (!is.null(dir))
      return(dir)
  }
  NULL
}


## HAS_TESTS
#' Script Directory From Call-Stack Source References
#'
#' Used under littler, which parses scripts with source references.
#' Walks caller frames so this works when invoked from package code
#' (e.g. `use_renv()` → `script_dir()`), not only at top level.
#'
#' @returns Normalized directory path, or `NULL`
#'
#' @noRd
script_dir_from_srcref <- function() {
  n <- sys.nframe()
  for (i in seq_len(n)) {
    sr <- attr(sys.call(i), "srcref")
    if (is.null(sr)) {
      fun <- sys.function(i)
      if (!is.null(fun))
        sr <- attr(fun, "srcref")
    }
    if (is.null(sr))
      next
    d <- utils::getSrcDirectory(sr)
    if (length(d) == 1L && nzchar(d))
      return(normalizePath(d, winslash = "/", mustWork = FALSE))
  }
  NULL
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


#' Activate an renv Project for the Current Session
#'
#' Make sure `renv` is activated, even if a script is run from 
#' Rscript or littler.
#'
#' Package [renv](https://rstudio.github.io/renv/)
#' makes workflows reproducible by locking down the versions of 
#' R packages. Normal `renv` mechanisms can fail, however,
#' if a script is run from Rscript or littler.
#' Adding
#' ```
#' command::use_renv()
#' ```
#' to the top of R scripts fixes the problem.
#' 
#' For more details, see the article
#' [Using command with renv](https://bayesiandemography.github.io/command/articles/a5_renv.html).
#'
#' @param project Optional path to a project root. If `NULL`
#'   (the default), search upward from the script's directory
#'   when known, otherwise from `getwd()`. The script directory
#'   is taken from `Rscript`'s `--file=` argument when present,
#'   or from source references under littler.
#' @param quiet If `TRUE` (the default), suppress messages.
#'
#' @returns The project root path (invisibly) if renv was activated
#'   or was already active; otherwise `NULL`.
#'
#' @seealso
#' - [cmd_assign()] Process command line arguments
#' - [Using command with renv](https://bayesiandemography.github.io/command/articles/a5_renv.html)
#'
#' @examples
#' command::use_renv() ## if renv not used here, no effect
#' 
#' ## see article for full example
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
    ## Always warn: quiet only suppresses happy-path messages.
    cli::cli_warn(c(
      "Found {.file renv.lock} but no {.file renv/activate.R}.",
      i = "Project: {.path {root}}.",
      i = paste("Nothing was activated. If this project uses renv,",
                "restore or recreate {.file renv/activate.R}.")
    ))
    return(invisible(NULL))
  }

  source(activate, local = FALSE)
  if (!quiet)
    cli::cli_alert_success("Activated renv project at {.path {root}}.")
  invisible(root)
}
