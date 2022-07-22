
#' Create
#'
#' @param \dots Arguments
#' @return Returns list invisibly, but invoked for side effect
#' of creating objects in current environment.
#'
#' @export
file_args <- function(...) {
    ## pattern used to identify options passed at command line
    p_opt <- "^-{1,2}(.*)=(.*)$"
    ## capture and characterise dots
    dots <- list(...)
    check_dots(dots)
    nms_dots <- names(dots)
    n_dots <- length(dots)
    ## extract all args, and check if this is an iteractive session
    args_all <- commandArgs()
    is_interactive <- !any(grepl("^--file=", args_all))
    ## if this is an interactive session, ignore 'args_all'
    ## and finish up, based on 'dots'
    if (is_interactive) {
        for (i in seq_along(dots))
            assign(x = nms_dots[[i]], value = dots[[i]])
        return(invisible(dots))
    }
    ## otherwise check whether any args were passed at command line
    i_dash_args <- match("--args", args_all, nomatch = 0L)
    args_passed <- i_dash_args > 0L
    ## handle case where no args were passed at command line
    if (!args_passed) {
        if (n_dots > 0L)
            stop("no command line arguments passed", call. = FALSE)
        else
            return(invisible(list()))
    }
    ## otherwise, extract args passed and check that they are valid
    n_args_all <- length(args_all)
    n_args <- n_args_all - i_dash_args
    args <- tail(args_all, n = n_args)
    check_args(args)
    check_dots_args_same_length(dots = dots, args = args)
    ## identify options in args
    is_opt <- grepl(p_opt, args)
    ## extract names of options
    nms_opts <- sub(p_opt, "\\1", args[is_opt])
    ## find corresponding values in dots, raising error if values not found
    i_nms_opts <- match(nms_opts, nms_dots, nomatch = 0L)
    i_nm_opt_not_found <- match(0L, i_nms_opts, nomatch = 0L)
    if (i_nm_opt_not_found > 0L) {
        nm_opt_not_found <- nms_opts[[i_nm_opt_not_found]]
        msg <- sprintf(paste("option called \"%s\" passed at command line,",
                             "but no argument called \"%s\" supplied to",
                             "function 'file_args'"),
                       nm_opt_not_found,
                       nm_opt_not_found)
        stop(msg, call. = FALSE)
    }
    ## match remaining cmd line args based on order
    i_dots <- integer(n_dots)
    i_dots[is_opt] <- i_nms_opts
    i_dots[!is_opt] <- setdiff(seq_len(n_dots), i_nms_opts)
    ## rewritten to here
}

check_dots <- function(dots) {
    ## check names valid
    checkmate::check_named(dots, type = "strict")
    nms <- names(dots)
    ## check that values are numeric or character
    is_num <- vapply(dots, is.numeric, TRUE)
    is_char <- vapply(dots, is.character, TRUE)
    is_valid <- is_num | is_char
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L) {
        msg <- sprintf(paste("problem with argument supplied to function 'file_args' :",
                             "'%s' has class \"%s\"",
                             nms[[i_invalid]],
                             class(dots[[i_invalid]])))
        stop(msg, call. = FALSE)
    }
    ## return
    invisible(TRUE)
}



check_args <- function(args, p_opt) {
    ## find options
    is_opt <- grepl(p_opt, args)
    opts <- args[is_opt]
    nms_opts <- sub(p_opt, "\\1", opts)
    ## check that names of options are not blank
    is_blank <- !nzchar(nms_opts)
    i_blank <- match(TRUE, is_blank, nomatch = 0L)
    if (i_blank > 0L) {
        msg <- sprintf(paste("problem with option passed at command line :",
                             "option \"%s\" does not have a name"),
                       args_opt[[i_blank]])
        stop(msg, call. = FALSE)
    }
    ## check that names of options are not duplicated
    is_dup <- duplicated(nms_opts)
    i_dup <- match(TRUE, is_dup, nomatch = 0L)
    if (i_dup > 0L) {
        msg <- sprintf(paste("problem with options passed at command line :",
                             "two options have the same name [\"%s\"]"),
                       args[[i_dup]])
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}


check_dots_args_same_length <- function(dots, args) {
    n_dots <- length(dots)
    n_args <- length(args)
    if (n_args != n_dots) {
        msg <- sprintf(paste("number of arguments supplied to function",
                             "'file_args' [%d] different from number of",
                             "arguments passed at command line [%d]\n",
                             "supplied to 'file_args' : %s\n",
                             "passed at command line : %s"),
                       n_dots,
                       n_args,
                       paste(paste(names(dots), dots, sep = "="),
                             collapse = ", "),
                       paste(args, collapse = ", "))
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}





## check_dots <- function(dots) {
##     ## check names valid
##     checkmate::check_named(dots, type = "strict")
##     nms_dots <- names(dots)
##     ## check that values are numeric or character
##     is_dots_num <- vapply(dots, is.numeric, TRUE)
##     is_dots_char <- vapply(dots, is.character, TRUE)
##     is_dots_valid <- is_dots_num | is_dots_char
##     i_dots_invalid <- match(FALSE, is_dots_valid, nomatch = 0L)
##     if (i_dots_invalid > 0L) {
##         msg <- sprintf(paste("problem with argument supplied to 'file_args' :",
##                              "'%s' has class \"%s\"",
##                              nms_dots[[i_dots_invalid]],
##                              class(dots[[i_dots_invalid]])))
##         stop(msg, call. = FALSE)
##     }
##     ## check that paths come before options
##     is_path_correct_order <- diff(is_dots_path) >= 0L
##     i_path_wrong_order <- match(FALSE, is_path_correct_order, nomatch = 0L)
##     if (i_path_wrong_order > 0L) {
##         msg <- sprintf(paste("problem with arguments supplied to 'file_args' :",
##                              "option '%s' [%s] comes before path '%s' [%s]"),
##                        nms_dots[[i_path_wrong_order]],
##                        dots[[i_path_wrong_order]],
##                        nms_dots[[i_path_wrong_order + 1L]],
##                        dots[[i_path_wrong_order + 1L]])
##         stop(msg, call. = FALSE)
##     }
##     ## check that paths are non-numeric
##     is_num_path <- is_dots_num & is_dots_path
##     i_num_path <- match(TRUE, is_num_path, nomatch = 0L)
##     if (i_num_path > 0L) {
##         msg <- sprintf(paste("problem with argument supplied to 'file_args' :",
##                              "path '%s' [%s] is numeric"),
##                        nms_dots[[i_num_path]],
##                        dots[[i_num_path]])
##         stop(msg, call. = FALSE)
##     }
##     invisible(TRUE)
## }



## check_args <- function(args) {
##     p_opt <- "^-{1,2}(.*)=.*$"
##     ## find options
##     is_opt <- grepl(p_opt, args)
##     opts <- args[is_opt]
##     nms_opts <- sub(p_opt, "\\1", opts)
##     ## check that options come after paths
##     is_opt_correct_order <- diff(is_args_opt) <= 0L
##     i_opt_wrong_order <- match(FALSE, is_opt_correct_order, nomatch = 0L)
##     if (i_opt_wrong_order > 0L) {
##         msg <- sprintf(paste("problem with arguments passed at command line :",
##                              "option \"%s\" comes before path \"%s\""),
##                        args[[i_opt_wrong_order]],
##                        args[[i_opt_wrong_order]])
##         stop(msg, call. = FALSE)
##     }
##     ## check that names of options in 'args' are not blank
##     is_opt_blank <- !nzchar(nms_args_opts)
##     i_opt_blank <- match(TRUE, is_opt_blank, nomatch = 0L)
##     if (i_opt_blank > 0L) {
##         msg <- sprintf(paste("problem with argument passed at commend line :",
##                              "argument \"%s\" does not have a name"),
##                        args_opt[[i_opt_blank]])
##         stop(msg, call. = FALSE)
##     }
##     ## check that names of options in 'args' are not duplicated
##     is_opt_dup <- duplicated(nms_args_opts)
##     i_opt_dup <- match(TRUE, is_opt_dup, nomatch = 0L)
##     if (i_opt_dup > 0L) {
##         msg <- sprintf(paste("problem with argument passed at commend line :",
##                              "two arguments named \"%s\""),
##                        args_opt[[i_opt_dup]])
##         stop(msg, call. = FALSE)
##     }
##     invisible(TRUE)
## }
