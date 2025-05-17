
## HAS_TESTS
#' Reorder 'args_cmd' and Add Names So That
#' It Aligns With 'args_dots'
#'
#' Use 'args_dots' to name and order elements
#' of 'args_cmd'. The process mimics argument
#' matching in function calls in R, in that
#' elements of 'args_cmd' are matched by name
#' where possible and by position where not.
#'
#' Assume that `args_dots` has been
#' checked via [check_arg_dots()] and
#' `args_cmd` has been checked via
#' [check_args_cmd()].
#' 
#' @param args_cmd A list, possibly with names.
#' @param args_dots A named list, the same
#' length as `arg_dots`.
#'
#' @return A list with the same length
#' and names as `arg_dots`.
#' 
#' @noRd
align_cmd_to_dots <- function(args_cmd, args_dots) {
  n <- length(args_cmd)
  nms_cmd <- names(args_cmd)
  nms_dots <- names(args_dots)
  if (is.null(nms_cmd))
    is_named <- rep(FALSE, times = n)
  else
    is_named <- nzchar(nms_cmd)
  args_cmd_named <- args_cmd[is_named]
  args_cmd_unnamed <- args_cmd[!is_named]
  nms_cmd_named <- nms_cmd[is_named]
  i_unnamed <- 1L
  ans <- vector(mode = "list", length = n)
  for (i in seq_len(n)) {
    nm_dots <- nms_dots[[i]]
    i_named <- match(nm_dots, nms_cmd_named, nomatch = 0L)
    if (i_named > 0L)
      ans[[i]] <- args_cmd_named[[i_named]]
    else {
      ans[[i]] <- args_cmd_unnamed[[i_unnamed]]
      i_unnamed <- i_unnamed + 1L
    }
  }
  names(ans) <- nms_dots
  ans
}


## HAS_TESTS
#' Create Objects in the Specified Environment
#'
#' Use the names and values in 'args' to create
#' objects in environment 'envir'.
#'
#' @param args A named list.
#' @param envir The environment where the
#' objects are to be created.
#'
#' @return Returns 'args' invisibly,
#' and creates objects as a side effect.
#'
#' @noRd
assign_args <- function(args, envir) {
  nms <- names(args)
  for (i in seq_along(args)) {
    arg <- args[[i]]
    nm <- nms[[i]]
    assign(x = nm,
           value = arg,
           envir = envir)
    assigned <- cli::col_grey("Assigned object")
    nm <- sprintf("`%s`", nm)
    nm <- cli::col_blue(nm)
    value <- cli::col_grey("with value")
    class <- cli::col_grey("and class")
    cli::cli_alert_success("{assigned} {nm} {value} {.val {arg}} {class} {.val {class(arg)}}.")
  }
  invisible(args)
}    


## HAS_TESTS
#' Internal Version of 'extact_cmd_shell' that Returns
#' Text Where Possible and NULL Otherwise
#'
#' @param file Path to R file from 'dir_shell'
#' @param dir_shell Directory where the
#' shell script is
#'
#' @returns A text string or NULL
#'
#' @noRd
cmd_shell_if_possible <- function(file, dir_shell) {
  path_file <- fs::path(dir_shell, file)
  ext <- fs::path_ext(path_file)
  if (!ext %in% c("r", "R"))
    return(NULL)
  text <- paste(readLines(path_file), collapse = "\n")
  exprs <- parse(text = text)
  nm_cmd <- as.name("cmd_assign")
  for (expr in exprs) {
    nm_expr <- expr[[1L]]
    if (is.call(expr) && identical(nm_expr, nm_cmd)) {
      args <- as.list(expr)[-1L]
      args <- lapply(args, eval)
      tryCatch(
        check_args_dots(args),
        error = function(e)
          cli::cli_abort(c(paste("Problem extracting call to {.fun cmd_assign}",
                                 "in file {.file {file}}."),
                           i = "Call to {.fun cmd_assign} malformed?",
                           i = e$message))
      )
      ans <- format_args_shell(file = file, args = args)
      cli::cli_alert_success("Extracted call to {.fun cmd_assign} in {.file {file}}.")
      return(ans)
    }
  }
  NULL
}


## HAS_TESTS
#' Internal Version of 'extact_cmd_make' that Returns
#' Text Where Possible and NULL Otherwise
#'
#' @param file Path to R file from 'dir_make'
#' @param dir_make Directory where the
#' Makefile is
#'
#' @returns A text string or NULL
#'
#' @noRd
cmd_make_if_possible <- function(file, dir_make) {
  path_file <- fs::path(dir_make, file)
  ext <- fs::path_ext(path_file)
  if (!ext %in% c("r", "R"))
    return(NULL)
  text <- paste(readLines(path_file), collapse = "\n")
  exprs <- parse(text = text)
  nm_cmd <- as.name("cmd_assign")
  for (expr in exprs) {
    nm_expr <- expr[[1L]]
    if (is.call(expr) && identical(nm_expr, nm_cmd)) {
      args <- as.list(expr)[-1L]
      args <- lapply(args, eval)
      tryCatch(
        check_args_dots(args),
        error = function(e)
          cli::cli_abort(c(paste("Problem extracting call to {.fun cmd_assign}",
                                 "in file {.file {file}}."),
                           i = "Call to {.fun cmd_assign} malformed?",
                           i = e$message))
      )
      ans <- format_args_make(file = file, args = args)
      cli::cli_alert_success("Extracted call to {.fun cmd_assign} in {.file {file}}.")
      return(ans)
    }
  }
  NULL
}


## HAS_TESTS
#' Coerce a Single Value Supplied at Command Line to the
#' Class of the Corresponding Value from Dots
#'
#' @param arg_cmd Value supplied at command line
#' @param arg_dots Corresponding value from dots
#' @param nm_cmd Name supplied at command line, or blank if none supplied
#' @param nm_dots Name supplied in call to cmd_assign()
#'
#' @returns Coerced version of `arg_cmd`
#'
#' @noRd
coerce_arg_cmd <- function(arg_cmd, arg_dots, nm_cmd, nm_dots) {
  if (is.character(arg_dots))
    ans <- arg_cmd
  else if (is.integer(arg_dots))
    ans <- tryCatch(as.integer(arg_cmd),
                    error = function (e) e,
                    warning = function(w) w)
  else if (is.numeric(arg_dots))
    ans <- tryCatch(as.numeric(arg_cmd),
                    error = function (e) e,
                    warning = function(w) w)
  else if (is.logical(arg_dots))
    ans <- tryCatch(as.logical(arg_cmd),
                    error = function (e) e,
                    warning = function(w) w)
  else if (methods::is(arg_dots, "Date"))
    ans <- tryCatch(as.Date(arg_cmd),
                    error = function (e) e,
                    warning = function(w) w)
  else if (methods::is(arg_dots, "POSIXct")) {
    tz <- attr(arg_dots, "tzone")
    ans <- tryCatch(as.POSIXct(arg_cmd, tz = tz),
                    error = function (e) e,
                    warning = function(w) w)
  }
  else if (methods::is(arg_dots, "POSIXlt")) {
    tz <- attr(arg_dots, "tzone")
    ans <- tryCatch(as.POSIXlt(arg_cmd, tz = tz),
                    error = function (e) e,
                    warning = function(w) w)
  }
  else if (is.null(arg_dots))
    ans <- if (identical(arg_cmd, "NULL")) NULL else tryCatch(stop(), error = function(e) e)
  else {
    cli::cli_abort("Internal error: {.arg arg_dots} has class {.cls {class(arg_dots)}}.")
  }
  if (inherits(ans, "error") || inherits(ans, "warning")) {
    cli::cli_abort(c(paste("Can't coerce value passed at command line to class",
                           "specified by {.fun cmd_assign}."),
                     i = "Value passed at command line: {.val {arg_cmd}}.",
                     i = "Value specified by {.fun cmd_assign}: {.val {arg_dots}}.",
                     i = "Name of value specified by {.fun cmd_assign}: {.val {nm_dots}}.",
                     i = "Class of value specified by {.fun cmd_assign}: {.val {class(arg_dots)}}."))
  }
  ans
}
    

## HAS_TESTS
#' Coerce Values Supplied at Command Line
#' to Classes Used in Dots
#'
#' Coerce each element of 'args_cmd' to have
#' the same class as the corresponding
#' element of 'args_dots'. Raise an error
#' if this cannot be done.
#'
#' @param args_cmd Named list of values passed from
#' command line.
#' @param args_dots Named list of values specified
#' via the dots argument of [cmd_assign()].
#'
#' @return Revised version of `args_cmd`.
#'
#' @noRd
coerce_to_dots_class <- function(args_cmd, args_dots) {
  nms_cmd <- names(args_cmd)
  nms_dots <- names(args_dots)
  ans <- .mapply(coerce_arg_cmd,
                 dots = list(arg_cmd = args_cmd,
                             arg_dots = args_dots,
                             nm_cmd = nms_cmd,
                             nm_dots = nms_dots),
                 MoreArgs = list())
  names(ans) <- nms_dots
  ans
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
#' @param file File path from Makefile to script with R code
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
#' Get command line arguments
#'
#' Use function 'commandArgs' to get
#' command line arguments. Assumes
#' that current session is not interactive.
#'
#' @return A named list.
#'
#' @noRd
get_args_cmd <- function() {
  p <- "^-{1,2}(.*)=(.*)$"
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0L)
    return(list())
  is_named <- grepl(p, args)
  args_named <- args[is_named]
  nms_named <- sub(p, "\\1", args_named)
  vals_named <- sub(p, "\\2", args_named)
  ans <- as.list(args)
  nms_ans <- rep("", times = length(ans))
  nms_ans[is_named] <- nms_named
  names(ans) <- nms_ans
  ans[is_named] <- vals_named
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


## HAS_TESTS
#' Check that 'nm' is a Valid Name for an Object in R
#'
#' @param nm A string
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_varname_valid <- function(nm) {
  if (grepl("?", nm, fixed = TRUE)) ## can be interpreted below as call to help
    return(FALSE) 
  text <- paste(nm, "<- 0")
  val <- tryCatch(eval(parse(text = text)),
                  error = function(e) e)
  !inherits(val, "error")
}


## HAS_TESTS
#' Make Shell Commands for a Shell Script
#'
#' Loop through files in 'files', making
#' commands for ones that contain calls to
#' `cmd_assign()`.
#'
#' Assume that 'files' exists and is valid
#'
#' @param files Path from 'dir_make' to
#' directory where R code files exists
#' @param dir_make Location of Makefile
#'
#' @returns A list of strings
#'
#' @noRd
make_commands <- function(files, dir_shell) {
  files <- fs::path(dir_shell, files)
  files <- fs::dir_ls(files)
  files <- fs::path_rel(files, start = dir_shell)
  ans <- .mapply(cmd_shell_if_possible,
                 dots = list(file = files),
                 MoreArgs = list(dir_shell = dir_shell))
  ans <- Filter(Negate(is.null), ans)
  ans <- unlist(ans)
  ans
}



## HAS_TESTS
#' Make Rules for a Makefile
#'
#' Loop through files in 'files', making
#' rules for ones that contain calls to
#' `cmd_assign()`.
#'
#' Assume that 'files' exists and is valid
#'
#' @param files Path from 'dir_make' to
#' directory where R code files exists
#' @param dir_make Location of Makefile
#'
#' @returns A list of strings
#'
#' @noRd
make_rules <- function(files, dir_make) {
  files <- fs::path(dir_make, files)
  files <- fs::dir_ls(files)
  files <- fs::path_rel(files, start = dir_make)
  ans <- .mapply(cmd_make_if_possible,
                 dots = list(file = files),
                 MoreArgs = list(dir_make = dir_make))
  ans <- Filter(Negate(is.null), ans)
  ans <- unlist(ans)
  ans
}

