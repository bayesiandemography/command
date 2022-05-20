
## HAS_TESTS
#' Get the file path to the target
#'
#' Get the path to the file that holds output from the current file.
#'
#' When called with no arguments, \code{path_target} assumes that (i) the
#' current file is being run from the command line,
#' and that the name of the target was passed
#' as a command line argument. See the examples below
#' and the vignette for this package.
#'
#' If the current file is not being run from the command line,
#' and if a \code{default} argument has been supplied, then
#' \code{path_target} will use that instead. Defaults are
#' helpful when developing code, but can disguise problems,
#' so should be removed when the code has matured.
#'
#' @param default A value to return if the current
#' file is not being run from the command line.
#'
#' @return A character vector of length 1,
#' giving the path to a file.
#'
#' @seealso \code{\link{filename_opt}}
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
#' }
#'
#' ## -- Use interactively --------------------------------
#' 
#' path_target(default = "product-high.rds")
#' @export
path_target <- function(default = NULL) {
    p_args <- "^--args$"
    p_file <- "^--file=.*$"
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
            checkmate::assert_string(default, min.chars = 1L)
            ans <- default
        }
        else {
            msg <- "could not find target"
            has_file_arg <- any(grepl(p_file, args))
            if (!has_file_arg)
                msg <- paste0(msg, " : perhaps because file not run from command line?")
            stop(msg, call. = FALSE)
        }
    }
    ans
}
