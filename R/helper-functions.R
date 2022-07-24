
## HAS_TESTS
#' Create objects in current environment
#'
#' Use the names and values in 'args' to
#' create objects in the current environment.
#'
#' @param args A named list.
#'
#' @return Returns 'args' invisibly,
#' and creates objects as a side effect.
#'
#' @noRd
assign_args <- function(args) {
    nms <- names(args)
    envir <- parent.frame()
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
#' Check dots arguments
#'
#' Check that names and values supplied
#' 'assign_named' or 'assign_unnamed'
#' via the dots argument are valid.
#'
#' @param args_dots Dots argument from 'assign_named' or
#' 'assign_unnamed' turned into a list.
#' @param fun_name "assign_named" or "assign_unnamed"
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_args_dots <- function(args_dots, fun_name) {
    n <- length(args_dots)
    nms <- names(args_dots)
    val_chk_nms <- checkmate::check_names(nms, type = "strict")
    if (!isTRUE(val_chk_nms)) {
        msg <- gettextf("problem with names in '...' argument to '%s' : %s",
                        fun_name,
                        val_chk_nms)
        stop(msg, call. = FALSE)
    }
    is_character <- vapply(args_dots, is.character, NA, USE.NAMES = FALSE)
    is_numeric <- vapply(args_dots, is.numeric, NA, USE.NAMES = FALSE)
    is_logical <- vapply(args_dots, is.logical, NA, USE.NAMES = FALSE)
    is_valid <- is_character | is_numeric | is_logical
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L) {
        msg1 <- gettextf("value for '%s' has class \"%s\"",
                         nms[[i_invalid]],
                         class(args_dots[[i_invalid]]))
        msg <- gettextf("problem with values in '...' argument to '%s' : %s",
                        fun_name,
                        msg1)
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Coerce values to classes implied by templates
#'
#' Coerce the elements of 'vals' to have the same
#' classes as the elements of 'templates',
#' raising an error if this cannot be done.
#'
#' @param vals A list of values.
#' @param names The names of the elements of 'vals'/
#' @param templates A list of values.
#' @param fun_name "assign_named" or "assign_unnamed".
#'
#' @return A list of values.
#'
#' @noRd
coerce_to_template <- function(vals, names, templates, fun_name) {
    for (i in seq_along(vals)) {
        name <- names[[i]]
        val <- vals[[i]]
        template <- templates[[i]]
        class_template <- class(template)
        val_new <- suppressWarnings(methods::as(val, class_template))
        cannot_coerce <- is.na(val_new)
        if (cannot_coerce) {
            msg1 <- gettextf("value \"%s\" passed at command line cannot be coerced to class \"%s\"",
                             val,
                             class_template)
            msg <- gettextf("function '%s' unable to create object '%s' : %s",
                            fun_name,
                            name,
                            msg1)
            stop(msg, call. = FALSE)
        }
        vals[[i]] <- val_new
    }
    vals
}
            

## HAS_TESTS
#' Get named command line arguments
#'
#' Use function 'commandArgs' to get
#' named command line arguments. Assumes
#' that current session is not interactive.
#'
#' @return A named list.
#'
#' @noRd
get_args_cmd_named <- function() {
    p <- "^-{1,2}([A-z_.]+)=(.*)$"
    args <- commandArgs(trailingOnly = TRUE)
    is_named <- is_named_arg(args)
    args_named <- args[is_named]
    ans <- sub(p, "\\2", args_named)
    ans <- as.list(ans)
    nms <- sub(p, "\\1", args_named)
    names(ans) <- nms
    ans
}


## HAS_TESTS
#' Get unnamed command line arguments
#'
#' Use function 'commandArgs' to get
#' unnamed command line arguments. Assumes
#' that current session is not interactive.
#'
#' @return An unnamed list.
#'
#' @noRd
get_args_cmd_unnamed <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    is_unnamed <- !is_named_arg(args)
    ans <- args[is_unnamed]
    as.list(ans)
}


## HAS_TESTS
#' Identify named command line arguments
#'
#' Identify command line arguments that have
#' the format -n=value or --name=value.
#'
#' @param x Command line arguments - output from
#'   \code{\link{commandArgs}}.
#'
#' @return A logical vector.
#'
#' @noRd
is_named_arg <- function(x) {
    is_short <- grepl("^-[A-z]=.+$", x)
    is_full <- grepl("^--[A-z_.]+=.+$", x)
    is_short | is_full
}


## HAS_TESTS
#' Combine values from dots with unnamed command line arguments
#'
#' Combine names and classes from 'dots' with values
#' obtained from command line arguments.
#'
#' @param args_dots Named list.
#' @param args_cmd Unnamed list.
#'
#' @return Named list
#'
#' @noRd
make_args_comb_unnamed <- function(args_dots, args_cmd) {
    n_dots <- length(args_dots)
    n_cmd <- length(args_cmd)
    nms <- names(args_dots)
    if (n_dots != n_cmd) {
        msg1 <- sprintf(ngettext(n_dots,
                                 "%d name-value pair supplied in '...'",
                                 "%d name-value pairs supplied in '...'"),
                        n_dots)
        msg2 <- sprintf(ngettext(n_cmd,
                                 "%d unnamed argument passed at command line",
                                 "%d unnamed arguments passed at command line"),
                        n_cmd)
        msg <- gettextf("problem with function '%s' : %s and %s",
                        "assign_unnamed",
                        msg1,
                        msg2)
        stop(msg, call. = FALSE)
    }
    ans <- coerce_to_template(vals = args_cmd,
                              names = nms,
                              templates = args_dots,
                              fun_name = "assign_unnamed")
    names(ans) <- nms
    ans
}

    
## HAS_TESTS
#' Combine values from dots with named command line arguments
#'
#' Combine names and classes from 'dots' with values
#' obtained from command line arguments.
#'
#' @param args_dots Named list.
#' @param args_cmd Named list.
#'
#' @return Named list
#'
#' @noRd
make_args_comb_named <- function(args_dots, args_cmd) {
    n_dots <- length(args_dots)
    n_cmd <- length(args_cmd)
    nms_dots <- names(args_dots)
    nms_cmd <- names(args_cmd)
    not_in_cmd <- setdiff(nms_dots, nms_cmd)
    if (length(not_in_cmd) >= 1L) {
        first_not_in_cmd <- not_in_cmd[[1L]]
        msg1 <- gettextf("argument named '%s' supplied in '...'",
                         first_not_in_cmd)
        msg2 <- gettextf("argument named '%s' passed at command line",
                         first_not_in_cmd)
        msg <- gettextf("problem with function '%s' : have %s but do not have %s",
                        "assign_named",
                        msg1,
                        msg2)
        stop(msg, call. = FALSE)
    }
    not_in_dots <- setdiff(nms_cmd, nms_dots)
    if (length(not_in_dots) >= 1L) {
        first_not_in_dots <- not_in_dots[[1L]]
        msg1 <- gettextf("argument named '%s' passed at command line",
                         first_not_in_dots)
        msg2 <- gettextf("argument named '%s' supplied in '...'",
                         first_not_in_dots)
        msg <- gettextf("problem with function '%s' : have %s but do not have %s",
                        "assign_named",
                        msg1,
                        msg2)
        stop(msg, call. = FALSE)
    }
    ans <- args_dots
    for (i in seq_along(ans)) {
        i_cmd <- match(nms_dots[[i]], nms_cmd)
        ans[[i]] <- args_cmd[[i_cmd]]
    }
    ans <- coerce_to_template(vals = ans,
                              names = nms_dots,
                              templates = args_dots,
                              fun_name = "assign_named")
    ans
}

    
                    
