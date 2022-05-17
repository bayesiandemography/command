
#' Extract a suffix from a target
#'
#' Extract a suffix from the path to a target file.  

variant <- function(target,
                    which = 1,
                    sep = "-",
                    choices = NULL,
                    convert = TRUE,
                    has_ext = TRUE) {
    checkmate::assert_string(target)
    checkmate::assert_int(which, lower = 1)
    checkmate::assert_string(sep)
    if (!is.null(choices))
        checkmate::assert_vector(choices, min.len = 1, unique = TRUE)
    checkmate::assert_convert(convert)
    checkmate::assert_flag(has_ext)
    target_trimmed <- basename(target)
    if (has_ext) {
        pieces <- strsplit(target_trimmed, split = ".", fixed = TRUE)[[1L]]
        n_pieces <- length(pieces)
        if (n_pieces > 1L)
            target_trimmed <- paste(pieces[-n_pieces], collapse = ".")
    }
    head_variants <- strsplit(target_trimmed, split = sep)[[1L]]
    n_variant <- length(head_variants) - 1L
    if (which > n_variant) {
        if (n_variant == 0L)
            msg <- sprintf("target '%s' has no variants",
                           target)
        else
            msg <- sprintf("'which' equals %s but target '%s' only has %d variants",
                           which, target, n_variant)
        stop(msg, call. = FALSE)
    }
    ans <- head_variants[[which + 1L]]
    if (convert)
        ans <- utils::type.convert(ans, as.is = TRUE)
    ans
}

## suffix_target("out/tgt-1-low.rds", 1)

