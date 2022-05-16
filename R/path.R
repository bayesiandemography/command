#' Test 'default' argument used by functions
#' 'path_prereq' and 'path_target'
#'
#' @param default The default if function called interactively.
#'
#' @return TRUE or an error.
#'
#' @noRd
assert_default_path <- function(default) {
    length_default <- length(default)
    if (length_default != 1L)
        stop("'default' has length ", length_default, call. = FALSE)
    if (!nzchar(default))
        stop("'default' is blank", call. = FALSE)
    TRUE
}

## path_prereq <- function(default = NULL) {
##     p_prereq <- "^--file="
##     p_interact <- "^--interactive$"
##     args <- commandArgs()
##     i_prereq <- grep(p_prereq, args)
##     has_prereq <- length(i_prereq) > 0L
##     if (has_prereq) {
##         i_prereq <- i_prereq[[1L]]
##         ans <- args[[i_prereq]]
##         ans <- sub(p_prereq, "", ans)
##     }
##     else {
##         has_default <- !is.null(default)
##         if (has_default) {
##             assert_default_path(default)
##             ans <- as.character(default)
##             message("'path_prereq' using default value [\"", ans, "\"]")
##         }
##         else {
##             msg <- "could not find path to first prerequisite"
##             is_interact <- any(grepl(p_interact, args))
##             if (is_interact)
##                 msg <- paste0(msg, " : appears to be an interactive session")
##             stop(msg, call. = FALSE)
##         }
##     }
##     ans
## }



get_target <- function(default = NULL) {
    p_args <- "^--args$"
    p_interact <- "^--interactive$"
    args <- commandArgs()
    i_args <- grep(p_args, args)
    has_target <- length(i_args) > 0L
    if (has_target) {
        n_args <- length(args)
        i_args <- i_args[[1L]]
        too_many_args <- n_args > i_args + 1L
        if (too_many_args) {
            s_passed <- seq(from = i_args + 1L, to = n_args)
            args_passed <- args[s_passed]
            args_passed <- sprintf("%s", args_passed)
            args_passed <- paste(args_passed, collapse = ", ")
            stop("more than one command line argument passed : ",
                 args_passed,
                 call. = FALSE)
        }
        ans <- args[[n_args]]
    }
    else {
        has_default <- !is.null(default)
        if (has_default) {
            assert_string(default, min.chars = 1L)
            ans <- default
            message("'get_target' using default value [\"", ans, "\"]")
        }
        else {
            msg <- "could not find path to target"
            is_interact <- any(grepl(p_interact, args))
            if (is_interact)
                msg <- paste0(msg, " : appears to be an interactive session")
            stop(msg, call. = FALSE)
        }
    }
    ans
}


get_suffix_target <- function(target, which = 1, sep = "-", remove_ext = TRUE) {
    checkmate::assert_string(target)
    checkmate::assert_int(which, lower = 1)
    checkmate::assert_flag(remove_ext)
    target_trimmed <- basename(target)
    if (remove_ext) {
        base_ext <- strsplit(target_trimmed, split = ".", fixed = TRUE)[[1L]]
        n_base_ext <- length(base_ext)
        if (n_base_ext > 1L)
            target_trimmed <- paste0(base_ext[seq_len(n_base_ext - 1L)])
    }
    head_suffixes <- strsplit(target_trimmed, split = sep)[[1L]]
    n_suffix <- length(head_suffixes) - 1L
    if (which > n_suffix) {
        if (n_suffix == 0L)
            msg <- sprintf("target '%s' has no suffixes",
                           target)
        else
            msg <- sprintf("'which' equals %s but target '%s' only has %d suffixes",
                           which, target, n_suffix)
        stop(msg, call. = FALSE)
    }
    head_suffixes[[which + 1L]]
}

## suffix_target("out/tgt-1-low.rds", 1)

