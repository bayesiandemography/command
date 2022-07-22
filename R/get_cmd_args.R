
## HAS_TESTS
#' Get an argument that was passed from the command line
#'
#' ## NEED TO REVISE DESCRIPTION
#'
#' Get the value for an argument that was supplied
#' when the file was run from the command line.
#' e.g. via \code{\link[utils]{Rscript}}.
#'
#' \code{which} is used to select among command
#' line arguments if more than one was supplied.
#' \code{which} can be a number the name of an argument
#' if the argument was named, eg \code{--sex=Female}
#' or \code{-s=F}.
#'
#' DEFAULT ONLY USED IN INTERACTIVE SESSIONS
#'
#' If \code{convert} is \code{TRUE} (the default), then
#' \code{get_cmd_arg} converts results to numeric where
#' it can, via function \code{\link[utils]{type.convert}}.
#'
#' @param which A number or a string. Defaults to \code{1}.
#' @param choices Vector of values that the argument
#' is allowed to take. If \code{NULL} (the default)
#' any value is allowed.
#' @param convert Whether to coerce numeric strings to
#' numeric. Defaults to \code{TRUE}.
#' @param interactive Value to be used in interactive sessions.
#' Optional.
#'
#' @return A number or string.
#'
#' @seealso \code{\link{mutate_filename}},
#' \code{\link{extract_filename_opt}},
#' \code{\link{get_fn_opt}}
#'
#' \code{get_cmd_args} calls \code{\link[base]{commandArgs}}
#' to extract the command line arguments. 
#'
#' @examples 
#' ## -- Run from the command line ------------------------
#'
#' \dontrun{
#' ## a file called 'foo.R' contains the lines
#' library(makr)
#' product <- 6 * 7
#' path_target <- path_target()
#' saveRDS(product, file = path_target)
#'
#' ## we have a shell script that contains the line
#' Rscript foo.R bar.rds
#'
#' ## running the shell script creates a
#' ## file 'bar.rds' consisting of the number 42
#'
#' ## (see the article 'makr' for more complete examples)
#' 
#'
#' ## -- Use interactively --------------------------------
#' 
#' path_target(interactive = "product-high.rds")
#' }
#' @export
get_cmd_arg <- function(which = 1,
                        choices = NULL,
                        interactive = NULL,
                        convert = TRUE) {
    ## string constants
    p_named <- "^-{1,2}([^-]+)=(.+)$"
    p_file <- "^--file="
    ## check arguments
    if (is.numeric(which))
        which <- checkmate::assert_int(which, lower = 1L, coerce = TRUE)
    else if (is.character(which))
        checkmate::assert_string(which, min.chars = 1L)
    else
        stop("'which' is not numeric or character", call. = FALSE)
    has_choices <- !is.null(choices)
    if (has_choices)
        checkmate::assert_atomic(choices, any.missing = FALSE, min.len = 1L, unique = TRUE)
    has_interactive <- !is.null(interactive)
    if (has_interactive) {
        if (is.numeric(interactive))
            checkmate::assert_number(interactive)
        else if (is.character(interactive))
            checkmate::assert_string(interactive, min.chars = 1L)
        else
            stop("'interactive' is not numeric or character", call. = FALSE)
    }
    checkmate::assert_flag(convert)
    ## extract and characterise args
    args <- commandArgs()
    i_dash_args <- match("--args", args, nomatch = 0L)
    has_args <- i_dash_args > 0L
    ## case 1: command line arguments supplied
    if (has_args) {
        n_args <- length(args) - i_dash_args
        if (is.numeric(which)) {
            if (which > n_args) {
                msg <- sprintf(paste("value for 'which' [%d] greater than",
                                     "number of command line arguments [%d]"),
                               which, n_args)
                stop(msg, call. = FALSE)
            }
            ans <- args[[i_dash_args + which]]
            ans <- sub(p_named, "\\2", ans)
        }
        else {
            s_passed <- seq.int(from = i_dash_args + 1L, to = length(args))
            args_passed <- args[s_passed]
            args_named <- grep(p_named, args_passed, value = TRUE)
            n_args_named <- length(args_named)
            if (n_args_named == 0L)
                stop("'which' is a name, but there are no named command line arguments",
                     call. = FALSE)
            names_args <- sub(p_named, "\\1", args_named)
            i_which <- match(which, names_args, nomatch = 0L)
            if (i_which == 0L) {
                msg1 <- "can't find named command line argument matching 'which'"
                msg2 <- sprintf(" : 'which' is \"%s\" : ", which)
                if (n_args_named == 1L)
                    msg3 <- sprintf("named command line argument is \"%s\"",
                                    args_named)
                else
                    msg3 <- sprintf("named command line arguments are %s",
                                    paste(sprintf("%s", args_named), collapse = ", "))
                stop(msg1, msg2, msg3, call. = FALSE)
            }                
            ans <- args_named[[i_which]]
            ans <- sub(p_named, "\\2", ans)
        }
    }
    ## case 2: command line arguments not supplied
    else {
        is_interactive_session <- !any(grepl(p_file, args))
        if (has_interactive && is_interactive_session)
            ans <- interactive
        else
            stop("no command line arguments supplied",
                 call. = FALSE)
    }
    ## convert and return
    if (convert)
        ans <- utils::type.convert(ans, as.is = TRUE)
    ans
}
    
