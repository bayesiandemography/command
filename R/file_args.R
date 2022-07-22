
#' Assign values based on unnamed command line arguments
#'
#' Use names in \dots, combined with unnamed values passed
#' at the command line, or default values in \dots, to 
#' create objects in the current environment.
#' 
#' If the script is being run from the command line,
#' then \code{assign_unnamed} locates all unnamed values
#' passed at the command line, and assigns names to them,
#' drawing on \dots. If the number of unnamed values
#' passed at the command line is greater than the number
#' of items in \dots, then an error is raised.
#' If the number of unnamed values is less than the
#' number of items in \dots, then any unused name-value
#' pairs in \dots are turned into objects, and any
#' names without values are ignored.
#'
#' If script is being run interactively (as defined by function
#' \code{\link[base]{interactive}}, then name-value
#' pairs in \dots are turned into objects, and
#' names without values are ignored.
#'
#' \code{assign_unnamed} is designed to be run mainly from
#' the command line. The interactive mode is for developing
#' new code, or interactively testing existing code.
#' 
#' @param \dots Names for objects, optionally with default values.
#' 
#' @return \code{assign_unnamed} returns a named list of objects
#' invisbly, but is normally called for its side effect,
#' which is to create objects in the current environment.
#'
#' @seealso \code{assign_named}
#'
#' @examples
#' ## use at command line - single argument ---------------
#'
#' ## create script
#' script_name <- tempfile()
#' cat("library(fileargs)\n
#'      assign_unnamed(a)\n
#'      print(ls())\n
#'      print(a)",
#'     file = script_name)
#' ## run script from command line, passing one
#' ## unnammed value
#' cmd <- sprintf("Rscript %s 500", script_name)
#' system(cmd)
#' file.remove(script_name)
#'
#'
#' ## use at command line - multiple arguments ------------
#'
#' ## create script
#' script_name <- tempfile()
#' cat("library(fileargs)\n
#'      assign_unnamed(a = 1, b, c = "hello", d)\n
#'      print(ls())\n
#'      print(a); print(b); print(c)",
#'     file = script_name)
#' ## run script from command line, passing
#' ## two unnamed values
#' cmd <- sprintf("Rscript %s 500 bonjour", script_name)
#' system(cmd)
#' file.remove(script_name)
#' 
#' ## use interactively -----------------------------------
#' assign_unnamed(a = 1, b, c = "hello", d)
#' ls()
#' print(a); print(c)
#' @export
assign_unnamed <- function(...) {
    args_dots <- get_args_dots(...)
    if (interactive())
        assign_args(arg_dots)
    else {
        args_cmd <- get_args_cmd_unnamed()
        args_comb <- make_args_comb_(args_dots = args_dots,
                                     args_cmd = args_cmd)
        assign_args(args_comb)
    }
}



#' Assign values based on named command line arguments
#'
#' Use named values passed at the command line,
#' or default values in \dots, to
#' create objects in the current environment.
#' 
#' If the script is being run from the command line,
#' then \code{assign_named} locates all named values
#' passed at the command line, and uses these to
#' create objects in the current environment. It then locates
#' any names in \dots that did not receive values via
#' the command line, and that have default values,
#' and uses the defaults to create objects. A named value
#' passed at the command line is
#' one that has the format \code{-<single letter>=<value>},
#' such as \code{-n=100} or \code{-p=YES}, or the
#' format \code{--<name>=<value>},
#' such as \code{--niter=100} or \code{--print=YES}.
#' (Note that there are no spaces around the equals.)
#' Passing a named value that is not included in \code{...}
#' is an error.
#'
#' If script is being run interactively (as defined by function
#' \code{\link[base]{interactive}}, then name-value
#' pairs in \dots are turned into objects, and
#' names without values are ignored.
#'
#' \code{assign_named} is designed to be run mainly from
#' the command line. The interactive mode is for developing
#' new code, or interactively testing existing code.
#' 
#' @param \dots Names for objects, optionally with default values.
#' 
#' @return \code{assign_named} returns a named list of objects
#' invisbly, but is normally called for its side effect,
#' which is to create objects in the current environment.
#'
#' @seealso \code{assign_unnamed}
#'
#' @examples
#' ## use at command line - single argument ---------------
#'
#' ## create script
#' script_name <- tempfile()
#' cat("library(fileargs)\n
#'      assign_named(iter = 100)\n
#'      print(ls())\n
#'      print(a)",
#'     file = script_name)
#' ## run script from command line, passing one
#' ## unnammed value
#' cmd <- sprintf("Rscript %s --iter=500", script_name)
#' system(cmd)
#' file.remove(script_name)
#'
#'
#' ## use at command line - multiple arguments ------------
#'
#' ## create script
#' script_name <- tempfile()
#' cat("library(fileargs)\n
#'      assign_named(iter = 100, b, print = TRUE, d)\n
#'      print(ls())\n
#'      print(a); print(b); print(c)",
#'     file = script_name)
#' ## run script from command line, passing
#' ## two unnamed values
#' cmd <- sprintf("Rscript %s --iter=500 bonjour", script_name)
#' system(cmd)
#' file.remove(script_name)
#' 
#' ## use interactively -----------------------------------
#' assign_unnamed(a = 1, b, c = "hello", d)
#' ls()
#' print(a); print(c)
#' @export
assign_named <- function(...) {
    args_dots <- get_args_dots(..., fn = "assign_named")
    if (interactive())
        assign_args(arg_dots)
    else {
        args_cmd <- get_args_cmd_named(args_dots)
        args_comb <- make_args_comb(args_dots = args_dots,
                                    args_cmd = args_cmd)
        assign_args(args_comb)
    }
}


get_args_dots <- function(..., fn) {
    dots <- substitute(as.list(...))
    n_dots <- length(dots)
    ans <- vector(mode = "list", length = n_dots - 1L)
    nms_dots <- names(dots)
    has_nm_supplied <- nzchar(nms_dots)
    nms_supplied <- nms_dots[[has_nm_supplied]]
    is_dup_nm <- duplicated(nms_supplied)
    i_dup_nm <- match(TRUE, is_dup_nm, nomatch = 0L)
    if (i_dup_nm > 0L) {
        msg <- paste("problem with function '%s' :",
                     "'...' has duplicated name [\"%s\"]")
        msg <- gettextf(msg,
                        fn,
                        nms_supplied[[i_dup_nm]])
        stop(msg, call = FALSE)
    }
    nms_ans <- nms_dots
    if (n_dots > 1L) {
        for (i in seq.int(from = 2L, to = n_dots)) {
            val <- dots[[i]]
            if (has_nm_supplied[[i]]) {
                is_valid_val <- is.numeric(val) || is.character(val)
                if (!is_valid_val) {
                    msg <- paste("problem with function '%s' :",
                                 "'%s' in '...' is has class \"%s\"")
                    msg <- gettextf(msg,
                                    fn,
                                    nms_dots[[i]],
                                    class(val))
                    stop(msg, call. = FALSE)
                }
                ans[[i]] <- val
            }
            else {
                if (is.name(val)) {
                    nms_ans <- as.character(val)
                    ans[i] <- list(NULL)
                }
                else {
                    msg <- paste("problem with function '%s' :",
                                 "item %d in '...' has class \"%s\"")
                    msg <- gettextf(msg,
                                    fn,
                                    class(val))
                    stop(msg, call. = FALSE)
                }
            }
        }
        names(ans) <- nms_ans
    }
    ans
}

is_named_arg <- function(x) {
    grepl("^-{1,2}[^-]+=.+$", x)
}
            
## assume this session is not interactive
get_args_cmd_unnamed <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    is_unnamed <- !is_named_arg(args)
    args[is_unnamed]
}

## assume this session is not interactive
get_args_cmd_named <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    is_named <- is_named_arg(args)
    args_named <- args[is_named]
    args_split <- strsplit(args_named, split = "=", fixed = TRUE)
    names <- vapply(args_split,
                    function(x) x[[1L]],
                    character(1L),
                    USE.NAMES = FALSE)
    values <- vapply(args_split,
                    function(x) x[[2L]],
                    character(1L),
                    USE.NAMES = FALSE)
    fsargs::
    
    
    args[is_unnamed]
}



make_args_comb_unnamed <- function(args_dots, args_cmd) {
    n_dots <- length(args_dots)
    n_cmd <- length(args_cmd)
    nms_dots <- names(dots)
    if (n_cmd > n_dots) {
        msg <- paste("problem with function '%s' :",
                     "number of names supplied via '...' [%d] less than",
                     "number of unnamed arguments passed at command line [%d]")
        msg <- gettextf(msg,
                        "assign_unnamed",
                        n_dots,
                        n_cmd)
        stop(msg, call. = FALSE)
    }
    ans <- args_dots
    nms_ans <- names(ans)
    s_cmd <- seq_len(n_cmd)
    ans[s_cmd] <- args_cmd[s_cmd]
    for (i in seq_along(ans)) {
        value <- ans[[i]]
        if (!is.null(value)) {
            x <- nms_ans[[i]]
            assign(x = x, value = value)
        }
    }
    ans <- ans[!is.null(ans)]
    invisible(ans)
}


make_args_comb_named <- function(args_dots, args_cmd) {
    n_dots <- length(args_dots)
    n_cmd <- length(args_cmd)
    nms_dots <- names(args_dots)
    nms_cmd <- names(args_cmd)
    is_full <- grepl("^--[^-].*$", nms_cmd)
    msg_invalid <- paste("problem with function '%s' :",
                         "could not find unique match in '...' for",
                         "named command-line argument '%s'")
    ans <- args_dots
    ## process command line arguments with '--name' format
    nms_full <- sub("^--", "", nms_cmd[is_full])
    i_full <- match(nms_full, nms_dots, nomatch = 0L)
    i_invalid_full <- match(0, i_full, nomatch = 0L)
    if (i_invalid_full > 0L) {
        msg <- gettextf(msg,
                        "assign_named",
                        nms_full[[i_invalid_full]])
        stop(msg, call. = FALSE)
    }
    ans[i_full] <- args_cmd[is_full]
    ## process command line args with '-n' format
    nms_short <- sub("^-", "", nms_cmd[[!is_full]])
    i_short <- pmatch(nms_short, nms_dots[!is_full], nomatch = 0L)
    i_invalid_short <- match(0L, i_short, nomatch = 0L)
    if (i_invalid_short > 0L) {
        msg <- gettextf(msg,
                        "assign_named",
                        nms_short[[i_invalid_short]])
        stop(msg, call. = FALSE)
    }
    ans[!is_full][is_short] <- args_cmd[!is_full][i_short]
    ## assign values
    for (i in seq_along(ans)) {
        value <- ans[[i]]
        if (!is.null(value)) {
            x <- nms_ans[[i]]
            assign(x = x, value = value)
        }
    }
    ans <- ans[!is.null(ans)]
    invisible(ans)
}
                    
