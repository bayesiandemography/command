
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
    nm <- sprintf("`%s`", nm)
    nm <- cli::col_blue(nm)
    msg <- "{.fun cmd_assign} creating object {nm} with value {.val {arg}} and class {.val {class(arg)}}"
    cli::cli_alert_success(msg)
  }
  invisible(args)
}    


## HAS_TESTS
#' Check Values Passed at Command Line
#'
#' Check that any named argument passed
#' at command line are also found in 'dots',
#' and that the number of arguments passed
#' at the command line matches the number
#' present in 'dots'.
#'
#' @param args_cmd A list, possibly with names.
#' @param args_dots A named list.
#' 
#' @return TRUE, invisibly.
#'
#' @noRd
check_args_cmd <- function(args_cmd, args_dots) {
  nms_cmd <- names(args_cmd)
  nms_dots <- names(args_dots)
  nms_cmd_nzchar <- nms_cmd[nzchar(nms_cmd)]
  ## any named arguments used in the command line
  ## should be present in dots
  for (nm in nms_cmd) {
    if (nzchar(nm) && !(nm %in% nms_dots)) {
      cli::cli_abort(c("Problem with argument {.arg {nm}}.",
                       i = paste("Named argument {.arg {nm}} passed at command line",
                                 "but not specified with {.fun cmd_assign}."),
                       i = "Argument{?s} specified with {.fun cmd_assign}: {.arg {nms_dots}}."))
    }
  }
  ## number of arguments passed at command line should
  ## match number of arguments specified in dots
  n_cmd <- length(args_cmd)
  n_dots <- length(args_dots)
  if (n_cmd != n_dots) {
    n_cmd_nzchar <- length(nms_cmd_nzchar)
    n_cmd_zchar <- n_cmd - n_cmd_nzchar
    msg <- c(paste("Mismatch between arguments passed at command line",
                   "and arguments specified with {.fun cmd_assign}."),
             i = paste("{.val {n_cmd_nzchar}} named argument{?s} and",
                       "{.val {n_cmd_zchar}} unnamed argument{?s}",
                       "passed at command line."),
             i = "{.val {n_dots}} argument{?s} specified with {.fun cmd_assign}.")
    if (n_cmd_nzchar > 0L)
      msg <- c(msg,
               i = "Named argument{?s} passed at command line: {.arg {nms_cmd_nzchar}}.")
    msg <- c(msg,
             i = "Argument{?s} specified with {.fun cmd_assign}: {.arg {nms_dots}}.")
    cli::cli_abort(msg)
  }
  ## no problems found
  invisible(TRUE)
}


## HAS_TESTS
#' Check Arguments Supplied to 'cmd_assign'
#' Through the Dots
#'
#' Check that names and values supplied
#' to [cmd_assign()] via the dots
#' argument are valid.
#'
#' Valid classes: character, integer, numeric, logical,
#'                Date, POSIXct, POSIXlt
#'
#' @param args_dots Dots argument from
#' [cmd_assign()].
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_args_dots <- function(args_dots) {
  n <- length(args_dots)
  nms <- names(args_dots)
  check_names_args(nms)
  is_character <- vapply(args_dots, is.character, NA, USE.NAMES = FALSE)
  is_integer <- vapply(args_dots, is.integer, NA, USE.NAMES = FALSE)
  is_numeric <- vapply(args_dots, is.numeric, NA, USE.NAMES = FALSE)
  is_logical <- vapply(args_dots, is.logical, NA, USE.NAMES = FALSE)
  is.Date <- function(x) methods::is(x, "Date")
  is.POSIXct <- function(x) methods::is(x, "POSIXct")
  is.POSIXlt <- function(x) methods::is(x, "POSIXlt")
  is_Date <- vapply(args_dots, is.Date, NA, USE.NAMES = FALSE)
  is_POSIXct <- vapply(args_dots, is.POSIXct, NA, USE.NAMES = FALSE)
  is_POSIXlt <- vapply(args_dots, is.POSIXlt, NA, USE.NAMES = FALSE)
  is_null <- vapply(args_dots, is.null, NA, USE.NAMES = FALSE)
  is_valid <- (is_character | is_logical | is_numeric | is_logical
    | is_Date | is_POSIXct | is_POSIXlt | is_null)
  i_invalid <- match(FALSE, is_valid, nomatch = 0L)
  if (i_invalid > 0L) {
    nm_invalid <- nms[[i_invalid]]
    val_invalid <- args_dots[[i_invalid]]
    cls_invalid <- class(val_invalid)
    valid_classes <- c("character", "integer", "numeric", "logical",
                       "Date", "POSIXct", "POSIXlt", "NULL")
    cli::cli_abort(c("Can't process argument {.arg {nm_invalid}} in call to {.fun cmd_assign}.",
                     i = "{.arg {nm_invalid}} has class {.val {cls_invalid}}.",
                     i = paste("{.fun cmd_assign} can only process arguments with classes",
                               "{.val {valid_classes}}.")))
  }
  lengths <- lengths(args_dots)
  is_valid_length <- ifelse(is_null, lengths == 0L, lengths == 1L)
  i_invalid_length <- match(FALSE, is_valid_length, nomatch = 0L)
  if (i_invalid_length > 0L) {
    nm_invalid_length <- nms[[i_invalid_length]]
    invalid_length <- lengths[[i_invalid_length]]
    cli::cli_abort(paste("Argument {.arg {nm_invalid_length}} in call to {.fun cmd_assign}",
                         "has length {.val {invalid_length}}."))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Argument Names are Valid
#'
#' Check that names are not NULL, NA, blank,
#' or duplicated. Also check that they are
#' valid names for R objects
#'
#' @param nms A character vector
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_names_args <- function(nms) {
  if (is.null(nms))
    cli::cli_abort("Arguments do not have names.")
  if (anyNA(nms))
    cli::cli_abort(c("Argument names include {.val {NA}}.",
                     i = "Argument names: {.val {nms}}."))
  if (!all(nzchar(nms)))
    cli::cli_abort(c("Argument name with length 0.",
                     i = "Argument names: {.val {nms}}."))
  i_dup <- match(TRUE, duplicated(nms), nomatch = 0L)
  if (i_dup > 0L)
    cli::cli_abort(c("More than one argument named {.val {nms[[i_dup]]}}.",
                     i = "Argument names: {.val {nms}}."))
  is_valid <- vapply(nms, is_varname_valid, TRUE)
  i_invalid <- match(FALSE, is_valid, nomatch = 0L)
  if (i_invalid > 0L)
    cli::cli_abort(c("Argument name not a valid name for an R object.",
                     i = "Invalid name: {.val {nms[[i_invalid]]}}.",
                     i = "Argument names: {.val {nms}}."))
  invisible(TRUE)
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
#' Check that 'nm' is a Valid Name for an Object in R
#'
#' @param nm A string
#'
#' @returns TRUE or FALSE
#'
#' noRd
is_varname_valid <- function(nm) {
  if (grepl("?", nm, fixed = TRUE)) ## can be interpreted below as call to help
    return(FALSE) 
  text <- paste(nm, "<- 0")
  val <- tryCatch(eval(parse(text = text)),
                  error = function(e) e)
  !inherits(val, "error")
}


