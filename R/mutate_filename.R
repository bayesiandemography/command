#' @export
mutate_filename <- function(filename, dir = NULL, base = NULL, ext = NULL,
                            sep = "-", fixed = TRUE) {
    checkmate::assert_string(filename)
    has_dir <- !is.null(dir)
    if (has_dir)
        checkmate::assert_string(dir)
    has_base <- !is.null(base)
    if (has_base)
        checkmate::assert_string(base)
    has_ext <- !is.null(ext)
    if (has_ext)
        checkmate::assert_string(ext)
    checkmate::assert_string(sep)
    checkmate::assert_flag(fixed)
    dirname <- dirname(filename)
    basename <- basename(filename)
    if (has_dir)
        dirname <- dir
    if (has_base) {
        pieces_basename <- strsplit(basename, split = sep, fixed = fixed)[[1L]]
        pieces_basename[[1L]] <- base
        basename <- paste(pieces_basename, collapse = sep)
    }
    if (has_ext) {
        pieces_ext <- strsplit(basename, split = ".", fixed = TRUE)[[1L]]
        n_pieces_ext <- length(pieces_ext)
        if (n_pieces_ext == 0L) {
            msg <- sprintf("filename \"%s\" does not appear to have a file extension",
                           filename)
            stop(msg, call. = FALSE)
        }
        pieces_ext[[n_pieces_ext]] <- ext
        basename <- paste(pieces_ext, collapse = ".")
    }
    file.path(dirname, basename)
}

#' @export
mutate_opt <- function(filename, which, value, sep = "-", fixed = TRUE,
                       has_ext = TRUE) {
    checkmate::assert_string(filename, min.chars = 1L)
    checkmate::assert_int(which, lower = 1L)
    if (length(value) != 1L)
        stop(sprintf("'value' has length %d", length(value)),
             call. = FALSE)
    checkmate::assert_string(sep, min.chars = 1L)
    checkmate::assert_flag(fixed)
    checkmate::assert_flag(has_ext)
    dirname <- dirname(filename)
    basename <- basename(filename)
    if (has_ext) {
        pieces_ext <- strsplit(basename, split = ".", fixed = TRUE)[[1L]]
        n_pieces_ext <- length(pieces_ext)
        if (n_pieces_ext < 2L) {
            msg <- sprintf(paste("'has_ext' is TRUE but filename \"%s\"",
                                "does not appear to have an extension"),
                          filename)
            stop(msg, call. = FALSE)
        }
        basename <- paste(pieces_ext[-n_pieces_ext], collapse = ".")
    }
    pos_sep <- gregexpr(sep, text = basename, fixed = fixed)[[1L]]
    n_sep <- length(pos_sep)
    if (n_sep == 0L) {
        msg <- sprintf("filename \"%s\" does not contain any options",
                       filename)
        stop(msg, call. = FALSE)
    }
    if (which > n_sep) {
        if (n_sep == 1L)
            msg <- sprintf("'which' equals %s but filename \"%s\" only has one option",
                           which, filename)
        else
            msg <- sprintf("'which' equals %s but filename \"%s\" only has %d options",
                           which, filename, n_sep - 1L)
        stop(msg, call. = FALSE)
    }
    match_length <- attr(pos_sep, "match.length")
    stop_head <- pos_sep[[which]] + match_length[[which]] - 1L
    head <- substr(basename, start = 1L, stop = stop_head)
    has_tail <- which < n_sep
    if (has_tail) {
        start_tail <- pos_sep[[which + 1L]]
        stop_tail <- nchar(basename)
        tail <- substr(basename, start = start_tail, stop_tail)
    }
    else
        tail <- NULL
    basename <- paste0(head, value, tail)
    if (has_ext)
        basename <- paste(basename, pieces_ext[[n_pieces_ext]], sep = ".")
    file.path(dirname, basename)
}

