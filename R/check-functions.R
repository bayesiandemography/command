
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
#' Check Name used for Makefile or Shell Script
#'
#' @param name String, valid as filename
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_dir <- function(dir, nm) {
  if (!identical(length(dir), 1L))
    cli::cli_abort("{.arg {nm}} does not have length 1.")
  if (!fs::dir_exists(dir))
    cli::cli_abort(c("Problem with {.arg {nm}}.",
                     i = "Directory {.file {dir}} does exist."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check File Exists, with Helpful Error Messages
#'
#' Check that file exists, with path starting at 'dir'.
#' Give helpful messages, since relative vs absolute
#' paths etc are potentially confusing.
#'
#' @param File Path from Makefile or shell script
#' to file with R code
#' @param dir Directory where Makefile or shell script exists
#' (typically the project directory)
#' @param nm_dir_arg Name of argument used
#' for directory. "dir_make" or "dir_shell"
#' @param has_dir_arg Whether the user supplied
#' an explicit argument for the Makefile or shell script
#' directory. (If not, default to current working directory.)
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_path_file_valid <- function(path_file, dir, nm_dir_arg, has_dir_arg) {
  if (fs::is_absolute_path(path_file))
    cli::cli_abort(c("{.arg path_file} is an absolute path.",
                     i = "{.arg path_file}: {.path {path_file}}.",
                     i = "{.arg path_file} must be a relative path."))
  path_file_comb <- fs::path(dir, path_file)
  if (!fs::file_exists(path_file_comb)) {
    msg1 <- "Can't find R script."
    if (has_dir_arg)
      msg2 <- paste("Path to R script constructed from",
                    "{.arg path_file} and {.arg {nm_dir_arg}}.")
    else
      msg2 <- paste("No value for {.arg {nm_dir_arg}} supplied, so path to",
                    "R script assumed to start from current working directory.")
    msg3 <- "Path: {.path {path_file_comb}}"
    msg <- c(msg1, i = msg2, i = msg3)
    cli::cli_abort(msg)
  }
  invisible(TRUE)
}  


## HAS_TESTS
#' Check Directory Specified by 'path_files' and 'dir' Exists,
#' with Helpful Error Messages
#'
#' Check that directory implied by 'dir' and
#' 'path_files' exists.
#' Give helpful messages, since relative vs absolute
#' paths etc are potentially confusing.
#'
#' @param path_files Path from Makefile or shell script
#' to directory with files with R code
#' @param dir Directory where Makefile or shell script exists
#' (typically the project directory)
#' @param nm_dirt_arg Name of argument used
#' for directory ("dir_make" or "dir_shell")
#' @param has_dir_arg Whether the user supplied
#' an explicit argument for the Makefile or shell script
#' directory. (If not, default to current working directory.)
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_path_files_valid <- function(path_files, dir, nm_dir_arg, has_dir_arg) {
  if (fs::is_absolute_path(path_files))
    cli::cli_abort(c("{.arg path_files} is an absolute path.",
                     i = "{.arg path_files}: {.path {path_files}}.",
                     i = "{.arg path_files} must be a relative path."))
  path_files_comb <- fs::path(dir, path_files)
  if (!fs::dir_exists(path_files_comb)) {
    msg1 <- "Can't find directory containing R scripts."
    if (has_dir_arg)
      msg2 <- paste("Path to directory constructed from",
                    "{.arg path_files} and {.arg {nm_dir_arg}}.")
    else
      msg2 <- paste("No value for {.arg {nm_dir_arg}} supplied, so path to",
                    "{.arg path_files} directory assumed to start",
                    "from current working directory.")
    msg3 <- "Path: {.path {path_files_comb}}"
    msg <- c(msg1, i = msg2, i = msg3)
    cli::cli_abort(msg)
  }
  invisible(TRUE)
}



## HAS_TESTS
#' Check a Logical Flag
#'
#' @param x TRUE or FALSE
#' @param nm Name for 'x' to use in error messages.
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_flag <- function(x, nm) {
  if (!identical(length(x), 1L))
    cli::cli_abort(c("{.arg {nm}} does not have length 1",
                     i = "{.arg {nm}} has length {length(x)}."))
  if (!is.logical(x))
    cli::cli_abort(c("{.arg {nm}} does not have class {.cls logical}.",
                     i = "{.arg {nm}} has class {.cls {class(x)}}"))
  if (is.na(x))
    cli::cli_abort("{.arg {nm}} is {.val {NA}}")
  invisible(TRUE)
}


## HAS_TESTS
#' Check that a File Contains R Code
#'
#' @param file Path to R code file
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
#' Check Whether a String Could be a Valid Filename
#'
#' @param x String
#' @param nm Name of string to be used
#' in error messages.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_valid_filename <- function(x, nm) {
  if (!is.character(x))
    cli::cli_abort("{.arg {nm}} is not a character string.")
  if (length(x) != 1L)
    cli::cli_abort("{.arg {nm}} does not have length 1.")
  if (!nzchar(x))
    cli::cli_abort("{.arg {nm}} is blank.")
  if (grepl("[/\\:*?\"<>|]", x))
    cli::cli_abort("{.arg {nm}} contains invalid character.")
  invisible(TRUE)
}
