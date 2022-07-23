
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
    val_chk_nms <- check_names(nms, type = "strict")
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
        msg1 <- gettextf("value [%s] for '%s' has class \"%s\"",
                         args_dots[[i_invalid]],
                         nms[[i_invalid]],
                         class(args_dots[[i_invalid]]))
        msg <- gettextf("problem with value in '...' argument to '%s' : %s"
                        fun_name,
                        msg)
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}

is_named_arg <- function(x) {
    is_short <- grepl("^-[A-z]=.+$", x)
    is_full <- grepl("^--[A-z_.]+=.+$", x)
    is_short | is_full
}

            
## assume this session is not interactive
get_args_cmd_unnamed <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    is_unnamed <- !is_named_arg(args)
    args[is_unnamed]
}

## assume this session is not interactive
get_args_cmd_named <- function() {
    p <- "^-{1,2}([A-z_.]+)=(.*)$"
    args <- commandArgs(trailingOnly = TRUE)
    is_named <- is_named_arg(args)
    args_named <- args[is_named]
    ans <- sub(p, "\\2", args_named)
    nms <- sub(p, "\\1", args_named)
    name(ans) <- nms
    ans
}

coerce_to_template <- function(vals, names, templates, fun_name) {
    for (i in seq_along(vals)) {
        name <- names[[i]]
        val <- vals[[i]]
        template <- templates[[i]]
        class_template <- class(template)
        val_new <- as(val, class_template)
        cannot_coerce <- is.na(val_new)
        if (cannot_coerce) {
            msg1 <- gettextf("value '%s' passed at command line cannot be coerced to class \"%s\"",
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
    ans <- coerce_to_templates(vals = ans_cmd,
                               names = nms,
                               templates = args_dots,
                               fun_name = "assign_unnamed")
    names(ans) <- nms
    ans
}

    
make_args_comb_named <- function(args_dots, args_cmd) {
    n_dots <- length(args_dots)
    n_cmd <- length(args_cmd)
    nms_dots <- names(args_dots)
    nms_cmd <- names(args_cmd)
    in_dots_not_in_cmd <- setdiff(nms_dots, nms_cmd)
    if (length(in_dots_not_in_cmd) >= 1L) {
        msg1 <- gettextf("argument named '%s' supplied via '...' but passed at command line",
                         in_dots_not_in_cmd[[1L]])
        msg <- gettextf("problem with function '%s' : %s",
                        "assign_named",
                        msg1)
        stop(msg, call. = FALSE)
    }
    in_cmd_not_in_dots <- setdiff(nms_cmd, nms_dots)
    if (length(in_cmd_not_in_dots) >= 1L) {
        msg1 <- gettextf("argument named '%s' passed at command line but not supplied via '...'",
                         in_cmd_not_in_dots[[1L]])
        msg <- gettextf("problem with function '%s' : %s",
                        "assign_named",
                        msg1)
        stop(msg, call. = FALSE)
    }
    ans <- args_dots
    for (i in seq_along(args_dots)) {
        i_cmd <- match(nms_dots[[i]], nms_cmd)
        ans[[i]] <- args_cmd[[i]]
    }
    ans <- coerce_to_templates(vals = args_dots,
                               names = nms_dots,
                               templates = args_dots,
                               fun_name = "assign_named")
    ans
}

    
                    
assign_args <- function(args) {
    nms <- names(args)
    for (i in seq_along(args)) {
        x <- nms[[i]]
        value <- args[[i]]
        assign(x = x, value = value)
    }
    invisible(args)
}    
