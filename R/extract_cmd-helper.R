
## HAS_TESTS
#' Check that a File Contains R Code
#'
#' @param file File path
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_r_code <- function(file) {
  R <- "R"
  r <- "r"
  if (!file.exists(file))
    cli::cli_abort("File {.file {file}} does not exist.")
  ext <- tools::file_ext(file)
  if (!ext %in% c(R, r))
    cli::cli_alert_warning("File {.file {file}} does not have extension {.val {R}} or {.val {r}}.")
  value <- tryCatch(parse(file = file),
                    error = function(e) e)
  if (inherits(value, "error"))
    cli::cli_abort(c("Can't parse file {.file {file}}.",
                     i = value$message))
  invisible(TRUE)
}


## HAS_TESTS
#' Extract Arguments From 'cmd_assign()' Call in File
#'
#' @param File path for an R script with a call to `cmd_assign()`
#'
#' @returns A named list, or NULL
#'
#' @noRd
extract_args <- function(file) {
  code <- paste(readLines(file), collapse = "\n")
  exprs <- parse(text = code)
  nm_cmd <- as.name("cmd_assign")
  for (expr in exprs) {
    nm_expr <- expr[[1L]]
    if (is.call(expr) && identical(nm_expr, nm_cmd)) {
      ans <- as.list(expr)[-1L]
      ans <- lapply(ans, eval)
      return(ans)
    }
  }
  cli::cli_alert_warning("No call to {.fn cmd_assign} found.")
  NULL
}


## HAS_TESTS
#' Turn List of Arguments into a Makefile Rule
#'
#' @param file File path to script with R code
#' @param args Named list obtained from call to `cmd_assign()`
#'
#' @returns A string
#'
#' @noRd
format_args_make <- function(file, args) {
  n_space <- 2L
  n_tab <- 8L
  is_file_arg <- is_file_arg(args)
  args <- lapply(args, as.character)
  n_file_arg <- sum(is_file_arg)
  if (identical(n_file_arg, 0L))
    cli::cli_abort(c("Can't find any file arguments.",
                     i = "Nothing to use as a target in Makefile rule."))
  args_is_file <- args[is_file_arg]
  target <- args_is_file[[n_file_arg]]
  ans <- sprintf("%s: %s", target, file)
  if (sum(is_file_arg) > 1L) {
    args_is_file <- args_is_file[-n_file_arg]
    space_left <- strrep(" ", times = n_space)
    args_is_file <- paste0(space_left, args_is_file)
    args_is_file <- paste0(" \\\n", args_is_file)
    ans_is_file <- paste(args_is_file, collapse = "")
    ans <- paste0(ans, ans_is_file)
  }
  ans_recipe <- "\n\tRscript $^ $@"
  ans <- paste0(ans, ans_recipe)
  if (any(!is_file_arg)) {
    args_not_file <- args[!is_file_arg]
    nms_not_file <- names(args_not_file)
    args_not_file <- paste0("--", nms_not_file, "=", args_not_file)
    space_not_file <- strrep(" ", times = n_tab + nchar(ans_recipe) - 1L)
    collapse <- paste0(" \\\n", space_not_file)
    ans_not_file <- paste0(args_not_file, collapse = collapse)
    ans <- paste(ans, ans_not_file)
  }
  ans <- paste0(ans, "\n")
  ans
}


## HAS_TESTS
#' Turn a List or Arguments into a Shell Command
#'
#' @param file File path to R script
#' @param args Named list or arguments from call to `cmd_assign()`
#'
#' @returns A string
#'
#' @noRd
format_args_shell <- function(file, args) {
  n_space <- 2L
  ans <- paste("Rscript", file)
  if (length(args) > 0L) {
    nms <- names(args)
    space_left <- strrep(" ", times = n_space)
    is_file_arg <- is_file_arg(args)
    args <- lapply(args, as.character)
    if (any(is_file_arg)) {
      args_is_file <- args[is_file_arg]
      args_is_file <- paste0(space_left, args_is_file)
      args_is_file <- paste0(" \\\n", args_is_file)
      ans_is_file <- paste(args_is_file, collapse = "")
      ans <- paste0(ans, ans_is_file)
    }
    if (any(!is_file_arg)) {
      args_not_file <- args[!is_file_arg]
      nms_not_file <- names(args_not_file)
      args_not_file <- paste0(space_left, "--", nms_not_file, "=", args_not_file)
      args_not_file <- paste0(" \\\n", args_not_file)
      ans_not_file <- paste(args_not_file, collapse = "")
      ans <- paste0(ans, ans_not_file)
    }
  }
  ans <- paste0(ans, "\n")
  ans
}


## HAS_TESTS
#' Test Whether an Argument is a File Path
#'
#' Can be actual file path, or a valid potential file path.
#'
#' @param arg Value that might be a file path
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_actual_or_potential_file_path <- function(arg) {
  if (!is.character(arg) || is.na(arg))
    return(FALSE)
  if (file.exists(arg))
    return(TRUE)
  looks_like_file <- grepl("[/\\\\]", arg) || grepl("\\.[a-zA-Z0-9]+$", arg)
  if (looks_like_file)
    return(TRUE)
  FALSE
}


## HAS_TESTS
#' Guess Whether Arguments are File Path
#'
#' If one or more arguments start with dots,
#' then assume that these arguments are file
#' paths. Otherwise use function
#' `is_actual_or_potential_file_path()`.
#'
#' @param args Named list of arguments from
#' call to `cmd_assign()`.
#'
#' @return Logical vector
#'
#' @noRd
is_file_arg <- function(args) {
  nms <- names(args)
  is_dot_arg <- grepl("^\\.", nms)
  if (any(is_dot_arg))
    ans <- is_dot_arg
  else
    ans <- vapply(args,
                  FUN = is_actual_or_potential_file_path,
                  FUN.VALUE = TRUE,
                  USE.NAMES = FALSE)
  ans
}





