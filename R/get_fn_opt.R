
## HAS_TESTS
#' Extract options from a file name or path
#'
#' Extract options from a file name or path, based
#' on conventions for constructing filenames.
#' Options include choices for inputs, parameters, or outputs.
#' For instance, a regression might be run several times with
#' different combinations of dataset and model specification.
#'
#' \code{filename} can include directories, and
#' is assumed to have the form
#'
#' \code{<dir>/<base><option><sep>...<sep><option><ext>}
#'
#' For instance, if a model is run separately on females and males,
#' then \code{filename} might be
#'
#' \code{out/model-female.rds}
#'
#' where \code{"-"} is being used to separate the option (\code{"female"})
#' from the base (\code{"model"}).
#'
#' If graphs are being produced for "high" and "low" variants
#' of a forecast, with various choices of a numeric parameter,
#' a target might be
#'
#' \code{project/figures/fig_forecast-high-3.pdf}
#'
#' @param filename File name or path.
#' @param which A number. Which option to extract,
#' starting from the left-most.
#' @param sep The string used to separate the base
#' from the options, and to separate different options.
#' @param fixed If \code{TRUE} (the default), match \code{sep} exactly.
#' If \code{FALSE}, interpret \code{sep} as a regular expression.
#' @param choices Valid values for the option. If \code{NULL}
#' (the default), any values are allowed.
#' @param labels Replacements for the values specified in
#' \code{choices}.
#' @param convert Whether to convert numeric options to numbers
#' (via \code{\link[utils]{type.convert}}). Defaults to \code{TRUE}.
#' @param has_ext Whether \code{filename} includes a file extension
#' (e.g. \code{.rds}). Defaults to \code{TRUE}.
#'
#' @return A character or (if \code{convert} is \code{TRUE})
#' possibly a numeric vector of length 1.
#'
#' @seealso Function \code{\link{get_cmd_args}} can be used
#' to extract file name and paths.
#'
#' @examples
#' \dontrun{
#' filename_opt("out/model-female.rds")
#'
#' filename_opt("project/figures/fig_forecast-high-3.pdf")
#' filename_opt("project/figures/fig_forecast-high-3.pdf",
#'              which = 2)
#' ## leave "3" as character
#' filename_opt("project/figures/fig_forecast-high-3.pdf",
#'              which = 2,
#'              convert = FALSE)
#'
#' ## non-default value for 'sep'
#' filename_opt("output.urban.2022.female.csv", sep = ".")
#'
#' ## include validity check
#' filename_opt("output-high.rda", choices = c("low", "med", "high"))
#' \dontrun{
#' filename_opt("output-mid.rda", choices = c("low", "med", "high"))
#' }
#'
#' ## filename does not include an extension
#' filename_opt("results/output.low.55",
#'            which = 2,
#'            sep = ".",
#'            has_ext = FALSE)
#' }
#' @export
get_fn_opt <- function(filename,
                       which = 1,
                       sep = "-",
                       fixed = TRUE,
                       choices = NULL,
                       labels = NULL,
                       convert = TRUE,
                       has_ext = TRUE) {
    checkmate::assert_string(filename, min.chars = 1L)
    checkmate::assert_int(which, lower = 1L)
    checkmate::assert_string(sep, min.chars = 1L)
    checkmate::assert_flag(fixed)
    has_choices <- !is.null(choices)
    if (has_choices)
        checkmate::assert_vector(choices, min.len = 1L, unique = TRUE)
    has_labels <- !is.null(labels)
    if (has_labels) {
        if (!has_choices)
            stop("value supplied for 'labels' but not for 'choices'", call. = FALSE)
        checkmate:::assert_vector(labels, min.len = 1L)
        if (length(labels) != length(choices)) {
            msg <- sprintf("'labels' and 'choices' have different lengths (%d vs %d)",
                           length(labels), length(choices))
            stop(msg, call. = FALSE)
        }
    }
    checkmate::assert_flag(convert)
    checkmate::assert_flag(has_ext)
    filename_base <- basename(filename)
    if (has_ext) {
        pieces_dot <- strsplit(filename_base, split = ".", fixed = TRUE)[[1L]]
        n_pieces_dot <- length(pieces_dot)
        if (n_pieces_dot > 1L)
            filename_base <- paste(pieces_dot[-n_pieces_dot], collapse = ".")
    }
    pieces_split <- strsplit(filename_base, split = sep, fixed = fixed)[[1L]]
    n_pieces_split <- length(pieces_split)
    if (n_pieces_split == 1L)
        stop(sprintf("filename '%s' has no options", filename),
             call. = FALSE)
    n_options <- n_pieces_split - 1L
    if (which > n_options) {
        if (n_options == 1L)
            msg <- sprintf("'which' equals %s but filename '%s' only has one option",
                           which, filename)
        else
            msg <- sprintf("'which' equals %s but filename '%s' only has %d options",
                           which, filename, n_options)
        stop(msg, call. = FALSE)
    }
    ans <- pieces_split[[which + 1L]]
    if (has_choices) {
        i_choice <- match(ans, choices, nomatch = 0L)
        if (i_choice == 0L)
            stop(sprintf("value for option [\"%s\"] not included in 'choices'",
                         ans),
                 call. = FALSE)
        if (has_labels)
            ans <- labels[[i_choice]]
    }
    if (convert)
        ans <- utils::type.convert(ans, as.is = TRUE)
    ans
}

