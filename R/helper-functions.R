
## HAS_TESTS
#' Reorder 'args_cmd', and add names, so that
#' it aligns with 'args_dots'
#'
#' Use 'args_dots' to name and order elements
#' of 'args_cmd'. The process mimics argument
#' matching in function calls in R, in that
#' elements of 'args_cmd' are matched by name
#' where possible and by position where not.
#'
#' Assume that \code{args_dots} has been
#' checked via \code{check_arg_dots} and
#' \code{args_cmd} has been checked via
#' \code{check_args_cmd}.
#' 
#' @param args_cmd A list, possibly with names.
#' @param args_dots A named list, the same
#' length as \code{arg_dots}.
#'
#' @return A list with the same length
#' and names as \code{arg_dots}.
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
#' Create objects in the specified environment.
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
        x <- nms[[i]]
        value <- args[[i]]
        assign(x = x,
               value = value,
               envir = envir)
    }
    invisible(args)
}    


## HAS_TESTS
#' Check values passed at command line
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
    for (nm in nms_cmd) {
        if (nzchar(nm) && !(nm %in% nms_dots)) {
            msg <- gettextf(paste("argument named \"%s\" passed at command line",
                                  "but no argument named \"%s\" specified in call to '%s'"),
                            nm,
                            nm,
                            "file_args")
            stop(msg, call. = FALSE)
        }
    }
    ## number of arguments passed at command line
    ## should match naumber of arguments specified in dots
    n_cmd <- length(args_cmd)
    n_dots <- length(args_dots)
    if (n_cmd != n_dots) {
        msg <- gettextf(paste("number of arguments passed at command line [%d]",
                              "not equal to number of arguments specified in",
                              "call to '%s' [%d]"),
                        n_cmd,
                        "file_args",
                        n_dots)
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check dots arguments
#'
#' Check that names and values supplied
#' to \code{\link{file_args}}
#' via the dots argument are valid.
#'
#' @param args_dots Dots argument from
#' \code{\link{file_args}}.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_args_dots <- function(args_dots) {
    n <- length(args_dots)
    nms <- names(args_dots)
    val_chk_nms <- checkmate::check_names(nms, type = "strict")
    if (!isTRUE(val_chk_nms)) {
        msg <- gettextf("names in call to '%s' invalid : %s",
                        "file_args",
                        val_chk_nms)
        stop(msg, call. = FALSE)
    }
    is_character <- vapply(args_dots, is.character, NA, USE.NAMES = FALSE)
    is_numeric <- vapply(args_dots, is.numeric, NA, USE.NAMES = FALSE)
    is_logical <- vapply(args_dots, is.logical, NA, USE.NAMES = FALSE)
    is_valid <- is_character | is_numeric | is_logical
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L) {
        msg <- gettextf("value in call to '%s' invalid : '%s' has class \"%s\"",
                        "file_args",
                        nms[[i_invalid]],
                        class(args_dots[[i_invalid]]))
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Coerce values supplied at command line
#' to classes used in dots
#'
#' Coerce each element of 'args_cmd' to have
#' the same class as the corresponding
#' element of 'args_dots'. Raise an error
#' if the cannot be done.
#'
#' @param args_cmd Named list of values passed from
#' command line.
#' @param args_dots Named list of values specified
#' via the dots argument of \code{\link{file_args}}.
#'
#' @return Revised version of \code{args_cmd}.
#'
#' @noRd
coerce_to_dots_class <- function(args_cmd, args_dots) {
    nms <- names(args_cmd)
    for (i in seq_along(args_cmd)) {
        val_cmd_old <- args_cmd[[i]]
        val_dots <- args_dots[[i]]
        class_dots <- class(val_dots)
        val_cmd_new <- suppressWarnings(methods::as(val_cmd_old, class_dots))
        cannot_coerce <- !identical(val_cmd_new, val_cmd_old) && is.na(val_cmd_new)
        if (cannot_coerce) {
            val_quoted <- if (is.character(val_cmd_old)) sprintf('"%s"', val_cmd_old) else val_cmd_old
            msg <- gettextf(paste("value for '%s' in call to '%s' has class \"%s\",",
                                  "but value for '%s' passed at command line [%s]",
                                  "cannot be coerced to class \"%s\""),
                            nms[[i]],                            
                            "file_args",
                            class_dots,
                            nms[[i]],
                            val_quoted,
                            class_dots)
            stop(msg, call. = FALSE)
        }
        args_cmd[[i]] <- val_cmd_new
    }
    args_cmd
}            


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
    p <- "^-{1,2}([A-z_.]+)=(.*)$"
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


#' Test whether 'x' is a filename starting with
#' "p_".
#'
#' @param A character or numeric vector of length 1.
#'
#' @return TRUE or FALSE.
#'
#' @noRd
is_p_filename <- function(x) {
    if (!is.character(x))
        return(FALSE)
    basename <- basename(x)
    grepl("^p_|^p\\.|^p[A-Z]", basename)
}


## HAS_TESTS
#' Test whether 'x' is the name of an .rds file
#'
#' @param A character or numeric vector of length 1.
#'
#' @return TRUE or FALSE.
#'
#' @noRd
is_rds_filename <- function(x) {
    if (!is.character(x))
        return(FALSE)
    x <- tolower(x)
    ext <- tools::file_ext(x)
    identical(ext, "rds")
}
    

## HAS_TESTS
#' Replace file paths for .rds files with
#' the contents of those files
#'
#' Identify elements of 'args' that are file
#' paths for .rds files, call \code{readRDS}
#' on those paths, and replace the paths
#' with the objects obtained.
#'
#' @param args A named list.
#'
#' @return A modified version of \code{args}.
#'
#' @noRd
replace_rds_with_obj <- function(args) {
    for (i in seq_along(args)) {
        value_old <- args[[i]]
        is_replace <- is_rds_filename(value_old) && !is_p_filename(value_old)
        if (is_replace) {
            value_new <- tryCatch(suppressWarnings(readRDS(value_old)),
                                  error = function(e) e)
            if (methods::is(value_new, "error")) {
                msg <- gettextf(paste("problem with function '%s' :",
                                      "attempt to call '%s'",
                                      "with argument \"%s\" failed : %s"),
                                "file_args",
                                "readRDS",
                                value_old,
                                value_new$message)
                stop(msg, call. = FALSE)
            }
            args[[i]] <- value_new
        }
    }
    args
}
