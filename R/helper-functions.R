
## HAS_TESTS
#' Join Path Components with Forward Slashes
#'
#' Base-R stand-in for `fs::path()`, using
#' forward slashes so paths are suitable for
#' Makefiles and shell scripts on all platforms.
#' Backslashes already inside a component are
#' rewritten as forward slashes. On Windows,
#' [tempfile()] inserts `\` even when `tmpdir`
#' uses `/`, and a backslash is an escape in a
#' shell command, so [shell_quote()] would wrap
#' the path.
#' Components that are `"."` are dropped so that
#' joining with the current directory does not
#' produce a redundant `"./"` prefix.
#'
#' @param ... Path components
#'
#' @returns A character vector of length 1
#'
#' @noRd
path_join <- function(...) {
  parts <- unlist(list(...), use.names = FALSE)
  parts <- chartr("\\", "/", parts)
  parts <- parts[nzchar(parts) & parts != "."]
  if (!length(parts))
    return(".")
  paste(parts, collapse = "/")
}


## HAS_TESTS
#' Test Whether a Path is Absolute
#'
#' Base-R stand-in for `fs::is_absolute_path()`.
#' Recognises Unix paths and Windows drive / UNC paths.
#'
#' @param path A length-1 character string
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:([/\\\\]|$)|\\\\\\\\)", path)
}


## HAS_TESTS
#' Make Paths Relative to a Start Directory
#'
#' Base-R stand-in for `fs::path_rel()`.
#'
#' @param path Character vector of paths
#' @param start Directory to make paths relative to
#'
#' @returns A character vector the same length as `path`
#'
#' @noRd
path_rel <- function(path, start = ".") {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  start <- normalizePath(start, winslash = "/", mustWork = TRUE)
  prefix <- paste0(start, "/")
  ## Windows paths are case-insensitive; normalizePath() may still
  ## disagree on drive-letter case (e.g. d: vs D: on WinBuilder).
  fold <- if (.Platform$OS.type == "windows") tolower else identity
  ans <- character(length(path))
  for (i in seq_along(path)) {
    p <- path[[i]]
    if (identical(fold(p), fold(start)))
      ans[[i]] <- "."
    else if (startsWith(fold(p), fold(prefix)))
      ans[[i]] <- substring(p, nchar(prefix) + 1L)
    else
      cli::cli_abort("Can't make {.path {path[[i]]}} relative to {.path {start}}.")
  }
  ans
}


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
#' checked via `check_arg_dots()` and
#' `args_cmd` has been checked via
#' `check_args_cmd()`.
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
#' Put Unnamed Command Line Arguments Into the One Leftover Argument
#'
#' Named elements of `args_cmd` are matched to `args_dots`
#' by name. The single argument in `args_dots` that was
#' not named on the command line receives every unnamed
#' value, as a character vector.
#'
#' Assume that `check_args_cmd_dots()` has already succeeded.
#'
#' @param args_cmd A list, possibly with names.
#' @param args_dots A named list.
#'
#' @return A named list with the same names as `args_dots`.
#'
#' @noRd
align_cmd_dots <- function(args_cmd, args_dots) {
  n_cmd <- length(args_cmd)
  nms_cmd <- names(args_cmd)
  if (is.null(nms_cmd))
    nms_cmd <- rep("", times = n_cmd)
  is_named <- nzchar(nms_cmd)
  nms_dots <- names(args_dots)
  nms_cmd_named <- nms_cmd[is_named]
  args_cmd_named <- args_cmd[is_named]
  unmatched <- !(nms_dots %in% nms_cmd_named)
  nm_vec <- nms_dots[unmatched][[1L]]
  unnamed <- unlist(args_cmd[!is_named], use.names = FALSE)
  if (is.null(unnamed))
    unnamed <- character()
  else
    unnamed <- as.character(unnamed)
  ans <- vector(mode = "list", length = length(args_dots))
  for (i in seq_along(nms_dots)) {
    nm <- nms_dots[[i]]
    if (identical(nm, nm_vec))
      ans[[i]] <- unnamed
    else
      ans[[i]] <- args_cmd_named[[match(nm, nms_cmd_named)]]
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
#' @param quiet Flag. If `TRUE`, suppress
#' success message.
#'
#' @return Returns 'args' invisibly,
#' and creates objects as a side effect.
#'
#' @noRd
assign_args <- function(args, envir, quiet) {
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
    empty <- if (identical(arg, "")) " (an empty string)" else ""
    if (!quiet) {
      cli::cli_alert_success("{assigned} {nm} {value} {.val {arg}}{empty} {class} {.val {class(arg)}}.")
    }
  }
  invisible(args)
}    


## HAS_TESTS
#' Extract One Shell Command and Its File Paths
#'
#' @param file Path to R file from 'dir_shell'
#' @param dir_shell Directory where the
#' shell script is
#' @param quiet Flag. Success message
#' only printed if 'quiet' is FALSE
#'
#' @returns A list with `file`, `inputs`, `output`, and `command`,
#' or NULL when the file has no call to extract.
#'
#' @noRd
extract_shell_step <- function(file, dir_shell, quiet) {
  path_file <- path_join(dir_shell, file)
  ext <- tools::file_ext(path_file)
  if (!ext %in% c("r", "R"))
    return(NULL)
  call <- extract_cmd_call(path_file)
  if (is.null(call))
    return(NULL)
  nm_fun <- call$name
  variadic <- is_collect_call(nm_fun)
  tryCatch(
    check_args_dots(call$args, variadic = variadic),
    error = function(e)
      cli::cli_abort(c(paste("Problem extracting call to {.fun {nm_fun}}",
                             "in file {.file {file}}."),
                       i = "Call to {.fun {nm_fun}} malformed?",
                       i = e$message))
  )
  io <- file_arg_io(call$args)
  command <- format_args_shell(file = file, args = call$args)
  if (!quiet)
    cli::cli_alert_success("Extracted call to {.fun {nm_fun}} in {.file {file}}.")
  list(file = file,
       inputs = io$inputs,
       output = io$output,
       command = command)
}


## HAS_TESTS
#' Internal Version of 'extract_shell' that Returns
#' Text Where Possible and NULL Otherwise
#'
#' @param file Path to R file from 'dir_shell'
#' @param dir_shell Directory where the
#' shell script is
#' @param quiet Flag. Success message
#' only printed if 'quiet' is FALSE
#'
#' @returns A text string or NULL
#'
#' @noRd
extract_shell_if_possible <- function(file, dir_shell, quiet) {
  step <- extract_shell_step(file = file,
                             dir_shell = dir_shell,
                             quiet = quiet)
  if (is.null(step))
    NULL
  else
    step$command
}


## HAS_TESTS
#' Internal Version of 'extract_make' that Returns
#' Text Where Possible and NULL Otherwise
#'
#' @param file Path to R file from 'dir_make'
#' @param dir_make Directory where the
#' Makefile is
#' @param quiet Flag. Success message
#' only printed if 'quiet' is FALSE
#'
#' @returns A text string or NULL
#'
#' @noRd
extract_make_if_possible <- function(file, dir_make, quiet) {
  path_file <- path_join(dir_make, file)
  ext <- tools::file_ext(path_file)
  if (!ext %in% c("r", "R"))
    return(NULL)
  call <- extract_cmd_call(path_file)
  if (is.null(call))
    return(NULL)
  nm_fun <- call$name
  variadic <- is_collect_call(nm_fun)
  tryCatch(
    check_args_dots(call$args, variadic = variadic),
    error = function(e)
      cli::cli_abort(c(paste("Problem extracting call to {.fun {nm_fun}}",
                             "in file {.file {file}}."),
                       i = "Call to {.fun {nm_fun}} malformed?",
                       i = e$message))
  )
  ans <- format_args_make(file = file, args = call$args)
  if (!quiet)
    cli::cli_alert_success("Extracted call to {.fun {nm_fun}} in {.file {file}}.")
  ans
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
  ## Empty values (e.g. Make expands --v=$(V) to --v= when V is
  ## undefined) are allowed for character arguments, but not for
  ## other classes, where they are almost always a mistake.
  if (!is.character(arg_dots) && identical(arg_cmd, "")) {
    cli::cli_abort(c("Empty value passed at command line for argument {.arg {nm_dots}}.",
                     i = "Value specified by {.fun cmd_assign}: {.val {arg_dots}}.",
                     i = "Class specified by {.fun cmd_assign}: {.cls {class(arg_dots)}}.",
                     i = paste("Empty values are only allowed when {.fun cmd_assign}",
                               "specifies a character argument."),
                     i = paste("If this came from a Makefile, check that the Make",
                               "variable in {.code --{nm_dots}=$(...)} is defined.")))
  }
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
  call <- extract_cmd_call(file)
  if (is.null(call)) {
    cli::cli_alert_warning(paste("No call to {.fn cmd_assign} or",
                                 "{.fn cmd_assign_collect} found."))
    return(NULL)
  }
  call$args
}


## HAS_TESTS
#' Whether a Call Name Collects Unnamed Arguments
#'
#' @param nm_fun Name of the function in the extracted call.
#'
#' @returns `TRUE` or `FALSE`
#'
#' @noRd
is_collect_call <- function(nm_fun) {
  nm_fun %in% c("cmd_assign_collect", "cmd_assign_collect_quiet")
}


## HAS_TESTS
#' Name of a cmd_assign Call, or NULL
#'
#' Accepts [cmd_assign()], [cmd_assign_quiet()],
#' [cmd_assign_collect()], and [cmd_assign_collect_quiet()],
#' with or without a `command::` or `command:::` prefix.
#'
#' @param expr A language object.
#'
#' @returns A string, or `NULL`.
#'
#' @noRd
cmd_call_name <- function(expr) {
  if (!is.call(expr))
    return(NULL)
  head <- expr[[1L]]
  nm <- NULL
  if (is.name(head)) {
    nm <- as.character(head)
  } else if (is.call(head) &&
             length(head) == 3L &&
             (identical(head[[1L]], as.name("::")) ||
              identical(head[[1L]], as.name(":::"))) &&
             identical(head[[2L]], as.name("command")) &&
             is.name(head[[3L]])) {
    nm <- as.character(head[[3L]])
  }
  allowed <- c("cmd_assign", "cmd_assign_quiet",
               "cmd_assign_collect", "cmd_assign_collect_quiet")
  if (!is.null(nm) && nm %in% allowed)
    nm
  else
    NULL
}


## HAS_TESTS
#' First cmd_assign Call in an Expression
#'
#' @param expr A language object, symbol, or constant.
#'
#' @returns A list with elements `name` and `expr`, or `NULL`.
#'
#' @noRd
find_cmd_call <- function(expr) {
  nm <- cmd_call_name(expr)
  if (!is.null(nm))
    return(list(name = nm, expr = expr))
  if (!is.call(expr))
    return(NULL)
  for (i in seq_along(expr)[-1L]) {
    found <- find_cmd_call(expr[[i]])
    if (!is.null(found))
      return(found)
  }
  NULL
}


## HAS_TESTS
#' Read One Literal Argument From a cmd_assign Call
#'
#' Accepts constants, `c(...)` of those values, and the class
#' constructors [as.integer()], [as.numeric()], [as.logical()],
#' [as.Date()], [as.POSIXct()], and [as.POSIXlt()]. A name or
#' any other call is an error. The constructors are taken from
#' [baseenv()], so a masked function in the global environment
#' is ignored.
#'
#' @param expr One argument from the parsed call.
#' @param nm Argument name, or `""` when the argument is unnamed.
#' @param file Path to the script, used in the error message.
#'
#' @returns The value of the literal.
#'
#' @noRd
read_cmd_arg <- function(expr, nm, file) {
  reject <- function() {
    text <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")
    header <- if (nzchar(nm))
      "Can't read argument {.arg {nm}} in {.file {file}}."
    else
      "Can't read an unnamed argument in {.file {file}}."
    cli::cli_abort(c(header,
                     i = "Expression: {.code {text}}.",
                     i = paste("Use a constant, {.fun c}, {.fun as.integer},",
                               "{.fun as.numeric}, {.fun as.logical}, {.fun as.Date},",
                               "{.fun as.POSIXct}, or {.fun as.POSIXlt}.")))
  }
  read_literal <- function(x) {
    if (is.null(x))
      return(NULL)
    if (is.atomic(x) && identical(length(x), 1L))
      return(x)
    if (is.symbol(x)) {
      nm_sym <- as.character(x)
      if (nm_sym %in% c("TRUE", "T"))
        return(TRUE)
      if (nm_sym %in% c("FALSE", "F"))
        return(FALSE)
      if (nm_sym == "NULL")
        return(NULL)
      if (nm_sym == "NA")
        return(NA)
      if (nm_sym == "NA_integer_")
        return(NA_integer_)
      if (nm_sym == "NA_real_")
        return(NA_real_)
      if (nm_sym == "NA_character_")
        return(NA_character_)
      if (nm_sym == "Inf")
        return(Inf)
      if (nm_sym == "NaN")
        return(NaN)
      reject()
    }
    if (!is.call(x))
      reject()
    head <- x[[1L]]
    if (!is.symbol(head))
      reject()
    fn <- as.character(head)
    args <- as.list(x)[-1L]
    if (fn == "(" && identical(length(args), 1L))
      return(read_literal(args[[1L]]))
    if (fn == "-" && identical(length(args), 1L)) {
      val <- read_literal(args[[1L]])
      if (is.numeric(val) && !is.logical(val) && identical(length(val), 1L))
        return(base::`-`(val))
      reject()
    }
    vals <- lapply(args, read_literal)
    if (fn == "c")
      return(do.call(base::c, vals))
    constructors <- c("as.integer", "as.numeric", "as.logical",
                      "as.Date", "as.POSIXct", "as.POSIXlt")
    if (!(fn %in% constructors))
      reject()
    nms <- names(vals)
    if (is.null(nms))
      nms <- rep("", length(vals))
    named <- nzchar(nms)
    if (sum(!named) != 1L)
      reject()
    value <- vals[!named][[1L]]
    optional <- vals[named]
    allowed <- switch(fn,
                      as.Date = "format",
                      as.POSIXct = c("format", "tz"),
                      as.POSIXlt = c("format", "tz"),
                      character())
    opt_nms <- names(optional)
    if (is.null(opt_nms))
      opt_nms <- character()
    if (any(!(opt_nms %in% allowed)))
      reject()
    ok_opt <- vapply(optional,
                     function(val) is.character(val) && identical(length(val), 1L),
                     TRUE)
    if (length(ok_opt) > 0L && !all(ok_opt))
      reject()
    fun <- get(fn, envir = baseenv(), mode = "function")
    do.call(fun, c(list(value), optional))
  }
  read_literal(expr)
}


## HAS_TESTS
#' Extract a cmd_assign or cmd_assign_collect Call From a File
#'
#' Finds the first call to [cmd_assign()], [cmd_assign_quiet()],
#' [cmd_assign_collect()], or [cmd_assign_collect_quiet()],
#' including a call nested inside another expression and a call
#' written as `command::cmd_assign()`. Argument values are read
#' by `read_cmd_arg()` and are not evaluated.
#'
#' @param file Path to an R script.
#'
#' @returns A list with elements `name` and `args`, or NULL
#' when the file contains neither call.
#'
#' @noRd
extract_cmd_call <- function(file) {
  code <- paste(readLines(file), collapse = "\n")
  exprs <- parse(text = code)
  for (expr in exprs) {
    found <- find_cmd_call(expr)
    if (is.null(found))
      next
    args <- as.list(found$expr)[-1L]
    nms <- names(args)
    if (is.null(nms))
      nms <- rep("", length(args))
    read <- lapply(seq_along(args), function(i) {
      read_cmd_arg(expr = args[[i]], nm = nms[[i]], file = file)
    })
    if (!is.null(names(args)))
      names(read) <- names(args)
    return(list(name = found$name, args = read))
  }
  NULL
}


## HAS_TESTS
#' Quote a Shell Word When It Is Not a Plain Token
#'
#' A plain token, such as `data/mydata.csv`, is left unchanged.
#' A value that contains a space or a character that is special
#' to the shell is wrapped with [shQuote()] using `type = "sh"`.
#'
#' @param x A length-1 character string.
#'
#' @returns A length-1 character string.
#'
#' @noRd
shell_quote <- function(x) {
  plain <- grepl("^[[:alnum:]_@%+=:,./][[:alnum:]_@%+=:,./-]*$", x)
  if (isTRUE(plain))
    x
  else
    shQuote(x, type = "sh")
}


## HAS_TESTS
#' Format One Named Command-Line Argument
#'
#' `NULL` is written as `--name=NULL`, which [cmd_assign()]
#' turns back into `NULL`. Other values must have length 1.
#'
#' @param nm Argument name.
#' @param value Argument value.
#'
#' @returns A length-1 character string.
#'
#' @noRd
format_named_arg <- function(nm, value) {
  if (is.null(value))
    return(paste0("--", nm, "=NULL"))
  if (!identical(length(value), 1L))
    cli::cli_abort(c("Can't format argument {.arg {nm}} for the command line.",
                     i = "{.arg {nm}} has length {.val {length(value)}}.",
                     i = "A named value must have length 1, or be {.val NULL}."))
  paste0("--", nm, "=", shell_quote(as.character(value)))
}


## HAS_TESTS
#' Format Named Command-Line Arguments
#'
#' @param args A named list of values that are not file paths.
#'
#' @returns A character vector, one `--name=value` string per argument.
#'
#' @noRd
format_named_args <- function(args) {
  nms <- names(args)
  vapply(seq_along(args),
         function(i) format_named_arg(nms[[i]], args[[i]]),
         FUN.VALUE = "")
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
  is_file <- is_file_arg(args)
  file_paths <- file_arg_paths(args = args, is_file = is_file)
  n_file_path <- length(file_paths)
  if (identical(n_file_path, 0L))
    cli::cli_abort(c("Can't find any file arguments.",
                     i = "Nothing to use as a target in Makefile rule."))
  target <- file_paths[[n_file_path]]
  ans <- sprintf("%s: %s", target, file)
  if (n_file_path > 1L) {
    prereqs <- file_paths[-n_file_path]
    space_left <- strrep(" ", times = n_space)
    prereqs <- paste0(space_left, prereqs)
    prereqs <- paste0(" \\\n", prereqs)
    ans_is_file <- paste(prereqs, collapse = "")
    ans <- paste0(ans, ans_is_file)
  }
  ans_recipe <- "\n\tRscript $^ $@"
  ans <- paste0(ans, ans_recipe)
  if (any(!is_file)) {
    args_not_file <- format_named_args(args[!is_file])
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
  ans <- paste("Rscript", shell_quote(file))
  if (length(args) > 0L) {
    space_left <- strrep(" ", times = n_space)
    is_file <- is_file_arg(args)
    if (any(is_file)) {
      file_paths <- file_arg_paths(args = args, is_file = is_file)
      if (length(file_paths) > 0L) {
        args_is_file <- paste0(space_left, vapply(file_paths, shell_quote, ""))
        args_is_file <- paste0(" \\\n", args_is_file)
        ans_is_file <- paste(args_is_file, collapse = "")
        ans <- paste0(ans, ans_is_file)
      }
    }
    if (any(!is_file)) {
      args_not_file <- paste0(space_left, format_named_args(args[!is_file]))
      args_not_file <- paste0(" \\\n", args_not_file)
      ans_not_file <- paste(args_not_file, collapse = "")
      ans <- paste0(ans, ans_not_file)
    }
  }
  ans <- paste0(ans, "\n")
  ans
}


## HAS_TESTS
#' Character Paths From Arguments Marked as Files
#'
#' A file argument that is a character vector contributes
#' one path per element, in order.
#'
#' @param args Named list of arguments.
#' @param is_file Logical vector, the same length as `args`.
#'
#' @returns A character vector, possibly of length 0.
#'
#' @noRd
file_arg_paths <- function(args, is_file) {
  unlist(lapply(args[is_file], as.character), use.names = FALSE)
}


## HAS_TESTS
#' Inputs and Output From File Arguments
#'
#' The last file path is the output, matching a Makefile rule.
#' Earlier file paths are inputs. A call with no file paths
#' has no output.
#'
#' @param args Named list of arguments from a call to
#' `cmd_assign()` or `cmd_assign_collect()`.
#'
#' @returns A list with elements `inputs` and `output`.
#' `output` is `NULL` when there are no file paths.
#'
#' @noRd
file_arg_io <- function(args) {
  is_file <- is_file_arg(args)
  paths <- if (any(is_file))
    file_arg_paths(args = args, is_file = is_file)
  else
    character()
  n_path <- length(paths)
  if (identical(n_path, 0L))
    list(inputs = character(), output = NULL)
  else
    list(inputs = paths[-n_path], output = paths[[n_path]])
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
  ## Split on the first '=', so a value may itself contain '='.
  p <- "^-{1,2}([^=]*)=(.*)$"
  is_littler <- (exists("argv", inherits = FALSE)
    || identical(commandArgs()[[1L]], "littler"))
  if (is_littler)
    args <- argv
  else
    args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0L)
    return(list())
  is_named <- grepl(p, args)
  args_named <- args[is_named]
  nms_named <- sub(p, "\\1", args_named)
  vals_named <- sub(p, "\\2", args_named)
  i_blank <- match(TRUE, !nzchar(nms_named), nomatch = 0L)
  if (i_blank > 0L)
    cli::cli_abort(c("Named command-line argument is missing a name.",
                     i = "Argument: {.val {args_named[[i_blank]]}}."))
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
                  FUN = is_file_value,
                  FUN.VALUE = TRUE,
                  USE.NAMES = FALSE)
  ans
}


## HAS_TESTS
#' Guess Whether One Argument Value is a File Path
#'
#' A character vector with length other than 1 is a file
#' when every element is. An empty character vector is not.
#'
#' @param arg One element of the list passed to `is_file_arg()`.
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_file_value <- function(arg) {
  if (is.character(arg) && !identical(length(arg), 1L)) {
    if (length(arg) == 0L)
      return(FALSE)
    all(vapply(arg,
               FUN = is_actual_or_potential_file_path,
               FUN.VALUE = TRUE,
               USE.NAMES = FALSE))
  } else {
    is_actual_or_potential_file_path(arg)
  }
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
#' Loop through files, making
#' commands for ones that contain calls to
#' `cmd_assign()`. Order the commands so that
#' a script runs after the scripts that create
#' its inputs.
#'
#' Assume that path constructed from 'dir_shell'
#' and 'path_files' exists and is valid
#'
#' @param path_files Path from 'dir_shell' to
#' directory where R code files exists
#' @param dir_shell Location of the shell script
#' @param quiet Whether to suppress
#' progress messages. 
#'
#' @returns A character vector of shell commands
#'
#' @noRd
make_commands <- function(path_files,
                          dir_shell,
                          quiet) {
  path_files_comb <- path_join(dir_shell, path_files)
  file <- list.files(path_files_comb, full.names = TRUE)
  file <- path_rel(file, start = dir_shell)
  steps <- lapply(file, extract_shell_step,
                  dir_shell = dir_shell,
                  quiet = quiet)
  steps <- Filter(Negate(is.null), steps)
  steps <- order_commands(steps)
  vapply(steps, function(step) step$command, FUN.VALUE = "")
}


## HAS_TESTS
#' Order Shell Commands by File Dependencies
#'
#' A script runs after every other script whose output
#' path is one of its inputs. The match is on the path
#' strings as written. Scripts that are free to run are
#' taken in path order.
#'
#' @param steps A list of records, each with `file`,
#' `inputs`, `output`, and `command`.
#'
#' @returns `steps` in execution order.
#'
#' @noRd
order_commands <- function(steps) {
  n_step <- length(steps)
  if (n_step == 0L)
    return(steps)
  files <- vapply(steps, function(step) step$file, FUN.VALUE = "")
  out_vals <- vapply(steps, function(step) {
    if (is.null(step$output))
      NA_character_
    else
      step$output
  }, FUN.VALUE = "")
  has_out <- !is.na(out_vals)
  dup_vals <- unique(out_vals[has_out][duplicated(out_vals[has_out])])
  if (length(dup_vals) > 0L) {
    bullets <- vapply(dup_vals, function(val) {
      who <- files[has_out & out_vals == val]
      as.character(cli::format_inline("{.file {who}} write {.file {val}}."))
    }, FUN.VALUE = "")
    names(bullets) <- rep("*", length(bullets))
    cli::cli_abort(c("Can't order the shell commands.",
                     i = "Two or more scripts write the same file.",
                     bullets))
  }
  preds <- lapply(seq_len(n_step), function(i) {
    inputs <- steps[[i]]$inputs
    if (length(inputs) == 0L)
      return(integer())
    producers <- match(inputs, out_vals)
    unique(producers[!is.na(producers)])
  })
  dependents <- rep(list(integer()), n_step)
  for (i in seq_len(n_step)) {
    for (p in preds[[i]]) {
      if (p != i)
        dependents[[p]] <- c(dependents[[p]], i)
    }
  }
  indeg <- lengths(preds)
  ready <- which(indeg == 0L)
  ready <- ready[order(files[ready])]
  ordered <- integer()
  while (length(ready) > 0L) {
    i <- ready[[1L]]
    ready <- ready[-1L]
    ordered <- c(ordered, i)
    for (j in dependents[[i]]) {
      indeg[[j]] <- indeg[[j]] - 1L
      if (indeg[[j]] == 0L) {
        ready <- c(ready, j)
        ready <- ready[order(files[ready])]
      }
    }
  }
  if (length(ordered) < n_step) {
    cycle <- find_command_cycle(nodes = setdiff(seq_len(n_step), ordered),
                                preds = preds)
    bullets <- vapply(seq_len(length(cycle) - 1L), function(k) {
      consumer <- steps[[cycle[[k]]]]
      producer <- steps[[cycle[[k + 1L]]]]
      as.character(cli::format_inline(paste("{.file {producer$file}} writes {.file {producer$output}},",
                                            "which {.file {consumer$file}} reads.")))
    }, FUN.VALUE = "")
    names(bullets) <- rep("*", length(bullets))
    cli::cli_abort(c("Can't order the shell commands.",
                     i = "These scripts depend on each other.",
                     bullets))
  }
  steps[ordered]
}


## HAS_TESTS
#' Find One Cycle in a Shell-Command Dependency Graph
#'
#' `preds[[i]]` lists the scripts that script `i` must follow.
#' The search follows those edges and returns the first cycle
#' it finds, with the first script repeated at the end.
#'
#' @param nodes Indices still unordered.
#' @param preds List of integer vectors, one per script.
#'
#' @returns An integer vector.
#'
#' @noRd
find_command_cycle <- function(nodes, preds) {
  n_pred <- length(preds)
  state <- rep(0L, n_pred)
  in_set <- rep(FALSE, n_pred)
  in_set[nodes] <- TRUE
  path <- integer()
  cycle <- NULL
  dfs <- function(i) {
    state[[i]] <<- 1L
    path <<- c(path, i)
    for (p in preds[[i]]) {
      if (!isTRUE(in_set[[p]]))
        next
      if (identical(state[[p]], 1L)) {
        at <- match(p, path)
        cycle <<- c(path[at:length(path)], p)
        return()
      }
      if (identical(state[[p]], 0L))
        dfs(p)
      if (!is.null(cycle))
        return()
    }
    path <<- path[-length(path)]
    state[[i]] <<- 2L
  }
  for (i in nodes) {
    if (identical(state[[i]], 0L))
      dfs(i)
    if (!is.null(cycle))
      break
  }
  cycle
}


## HAS_TESTS
#' Make Rules for a Makefile
#'
#' Loop through files, making
#' rules for ones that contain calls to
#' `cmd_assign()`.
#'
#' Assume that path constructed from 'dir_make'
#' and 'path_files' exists and is valid
#'
#' @param path_files Path from 'dir_make' to
#' directory where R code files exists
#' @param dir_make Location of Makefile
#'
#' @returns A list of strings
#' @param quiet Whether to suppress
#' progress messages. 
#'
#' @noRd
make_rules <- function(path_files,
                       dir_make,
                       quiet) {
  path_files_comb <- path_join(dir_make, path_files)
  file <- list.files(path_files_comb, full.names = TRUE)
  file <- path_rel(file, start = dir_make)
  ans <- .mapply(extract_make_if_possible,
                 dots = list(file = file),
                 MoreArgs = list(dir_make = dir_make,
                                 quiet = quiet))
  ans <- Filter(Negate(is.null), ans)
  ans <- unlist(ans)
  ans
}

