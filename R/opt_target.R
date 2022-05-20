
## HAS_TESTS
#' Extract options from a target
#'
#' Infer option used in creating a target from the name of that target.
#'
#' Options include choices for inputs, parameters, or outputs.
#' For instance, a regression might be run several times with
#' different combinations of dataset and model specification.
#'
#' The file name \code{target} is assumed to have the form
#'
#' \code{<dir>/<base><option><sep>...<sep><option><ext>}
#'
#' For instance, if a model is run separately on females and males,
#' then \code{target} might be
#'
#' \code{out/model-female.rds}
#'
#' where \code{"-"} is being used to separate the option (\code{"female"})
#' from the base (\code{"model"}).
#'
#' Or if graphs are being produced for "high" and "low" variants
#' of a forecast, with various choices of a numeric parameter,
#' a target might be
#'
#' \code{project/figures/fig_forecast-high-3.pdf}
#'
#' @param target File path to the output being created.
#' @param which A number. Which option to extract,
#' starting from the left-most.
#' @param sep The string used to separate the base
#' from the options, and to separate different options.
#' @param fixed If \code{TRUE} (the default), match \code{sep} exactly.
#' If \code{FALSE}, interpret \code{sep} as a regular expression.
#' @param choices Valid values for the option. If \code{NULL}
#' (the default), any values are allowed.
#' @param convert Whether to convert numeric options to numbers
#' (via \code{\link[utils]{type.convert}}). Defaults to \code{TRUE}.
#' @param has_ext Whether \code{target} includes a file extension
#' (e.g. \code{.rds}). Defaults to \code{TRUE}.
#'
#' @return A character or (if \code{convert} is \code{TRUE})
#' possibly a numeric vector of length 1.
#'
#' @seealso The value for \code{target} is typically obtained
#' using function \code{\link{target}}.
#'
#' @examples
#' opt_target("out/model-female.rds")
#'
#' opt_target("project/figures/fig_forecast-high-3.pdf")
#' opt_target("project/figures/fig_forecast-high-3.pdf",
#'            which = 2)
#' ## leave "3" as character
#' opt_target("project/figures/fig_forecast-high-3.pdf",
#'            which = 2,
#'            convert = FALSE)
#'
#' ## non-default value for 'sep'
#' opt_target("output.urban.2022.female.csv", sep = ".")
#'
#' ## include validity check
#' opt_target("output-high.rda", choices = c("low", "med", "high"))
#' \dontrun{
#' opt_target("output-mid.rda", choices = c("low", "med", "high"))
#' }
#'
#' ## target does not include an extension
#' opt_target("results/output.low.55",
#'            which = 2,
#'            sep = ".",
#'            has_ext = FALSE)
#' @export
opt_target <- function(target,
                       which = 1,
                       sep = "-",
                       fixed = TRUE,
                       choices = NULL,
                       convert = TRUE,
                       has_ext = TRUE) {
    checkmate::assert_string(target, min.chars = 1L)
    checkmate::assert_int(which, lower = 1L)
    checkmate::assert_string(sep, min.chars = 1L)
    checkmate::assert_flag(fixed)
    has_choices <- !is.null(choices)
    if (has_choices)
        checkmate::assert_vector(choices, min.len = 1L, unique = TRUE)
    checkmate::assert_flag(convert)
    checkmate::assert_flag(has_ext)
    target_base <- basename(target)
    if (has_ext) {
        pieces_dot <- strsplit(target_base, split = ".", fixed = TRUE)[[1L]]
        n_pieces_dot <- length(pieces_dot)
        if (n_pieces_dot > 1L)
            target_base <- paste(pieces_dot[-n_pieces_dot], collapse = ".")
    }
    pieces_split <- strsplit(target_base, split = sep, fixed = fixed)[[1L]]
    n_pieces_split <- length(pieces_split)
    if (n_pieces_split == 1L)
        stop(sprintf("target '%s' has no options", target),
             call. = FALSE)
    n_options <- n_pieces_split - 1L
    if (which > n_options) {
        if (n_options == 1L)
            msg <- sprintf("'which' equals %s but target '%s' only has one option",
                           which, target)
        else
            msg <- sprintf("'which' equals %s but target '%s' only has %d options",
                           which, target, n_options)
        stop(msg, call. = FALSE)
    }
    ans <- pieces_split[[which + 1L]]
    if (convert)
        ans <- utils::type.convert(ans, as.is = TRUE)
    if (has_choices) {
        i_choice <- match(ans, choices, nomatch = 0L)
        if (i_choice == 0L)
            stop(sprintf("value for option [\"%s\"] not included in 'choices'",
                         ans),
                 call. = FALSE)
    }
    ans
}

