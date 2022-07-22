

get_args_dots <- function(..., fun_name) {
    ans function<- list(...)
    n <- length(ans)
    nms <- names(dots)
    val_check_nms <- check_names(nms, type = "strict")
    if (!isTRUE(value)) {
        msg <- gettextf("problem with name-value pairs supplied to '%s' : %s",
                        fun_name,
                        val_check_nms)
        stop(msg, call. = FALSE)
    }
    is_logical <- vapply(ans, is.logical, TRUE, USE.NAMES = FALSE)
    is_numeric <- vapply(ans, is.numeric, TRUE, USE.NAMES = FALSE)
    is_character <- vapply(ans, is.character, TRUE, USE.NAMES = FALSE)
    is_valid <- is_logical | is_numeric | is_character
    i_invalid <- match(FALSE, is_valid, nomatch = 0L)
    if (i_invalid > 0L) {
        msg_inner <- gettextf("does not have type '%s', '%s', '%s', or '%s'",
                              "logical",
                              "integer",
                              "numeric",
                              "character")
        msg_outer <- gettextf("problem with name-value pairs supplied to '%s' : %s"
                              fun_name,
                              msg_inner)
        stop(msg, call. = FALSE)
    }
    ans
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

coerce_vals <- function(args, names, vals, templates, fun_name) {
    for (i in seq_along(vals)) {
        arg <- args[[i]]
        nm <- names[[i]]
        val_old <- vals[[i]]
        template <- templates[[i]]
        val_new <- utils::type.convert(val_old, as.is = TRUE)
        if (is.logical(template)) {
            if (is.logical(val_new))
                vals[[i]] <- val_new
            else if (is.numeric(val_new))
                vals[[i]] <- as.logical(val_new)
            else {
                msg1 <- gettextf("command line argument '%s'",
                                 arg)
                msg2 <- gettextf("cannot be coerced to type implied by 
                

                                          cannot be coerced to type \"%s\"",
                                 nm,
                                 val_old,
                                 class(template))
                msg_outer <- gettextxf("problem with function '%s' : %s",
                                       fun_name,
                                       msg_inner)
                stop(msg_outer, call. = FALSE)
            }
                
            
        if (is.character(
        if (!is.character(template)) {
            val_new <- 
            if (is.logical(template))
                
        }
        vals[[i]] <- val_new
        }
        vals
        }
            
make_args_comb_unnamed <- function(args_dots, args_cmd) {
    n_dots <- length(args_dots)
    n_cmd <- length(args_cmd)
    if (n_dots != n_cmd) {
        msg1 <- gettextf("problem with function '%s' :",
                         "assign_unnamed")
        msg2 <- sprintf(ngettext(n_dots,
                                 "%d name-value pair supplied in '...'",
                                 "%d name-value pairs supplied in '...'"),
                        n_dots)
        msg3 <- sprintf(ngettext(n_cmd,
                                 "and %d unnamed argument passed at command line",
                                 "and %d unnamed arguments passed at command line"),
                        n_cmd)
        stop(msg1, msg2, msg3, call. = FALSE)
    }
    ans <- args_cmd
    names(ans) <- names(args_dots)
    
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
                    
