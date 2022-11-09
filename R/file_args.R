
#' Process command line arguments
#'
#' Create objects in the current environment
#' based on values passed at the command line,
#' or on values supplied via \dots.
#'
#' The behaviour of \code{file_args} depends the
#' on the type of R session it is called from:
#' an interactive session, or a session
#' invoked from the command line, e.g
#' via \code{\link[utils]{Rscript}}. Typically,
#' \code{file_args} is used iteractively when
#' developing code and via the command line
#' when the code is mature.
#'
#' In an interactive session, \code{file_args}
#' takes the names and values for the objects
#' to be created from \dots. In a session
#' invoked from the command
#' line, \code{file_args} takes the values
#' from the command line, and the names from
#' names \dots. The assigning of names to values
#' passed at the command line works much
#' like the assigning of argument names to
#' values in a standard R function call.
#' \code{file_args} first assigns any named arguments
#' passed at the command line to names specified
#' in dots. It then assigns any unnamed arguments
#' passed at the command line to remaining names
#' specified in dots.
#'
#' \code{file_args} assumes named arguments passed
#' at the command line have the form
#' \itemize{
#'   \item \code{-<single-letter>=<value>}
#'      e.g. \code{-n=10} or \code{-f=params.rds}, or
#'   \item \code{--<name>=<value>}
#'      e.g. \code{--nchains=10} or
#'        \code{--filename=params.rds},
#' }
#' or some combination of the two. (Note that there are
#' no spaces around the \code{=} sign.) \code{file_args}
#' processes both the same way.
#'
#' If \code{.load.rds} is \code{TRUE} (the default),
#' then \code{file_args} treats filenames with
#' \code{.rds} specially. Rather than simply
#' assigning the filename in the current,
#' it actually loads the file, using function
#' \code{\link[base]{readRDS}}.
#'
#' To prevent a particular \code{.rds} file from being
#' automatically loaded, give the file a name
#' in \dots that starts with \code{p_}. For instance,
#' when called interactively,
#'
#' \code{file_args(out = "myfile.rds")}
#'
#' is equivalent to
#'
#' \code{out <- readRDS("myfile.rds")}
#'
#' while
#'
#' \code{file_args(out = "myfile.rds")}
#'
#' is equivalent to
#'
#' \code{p_out <- "myfile.rds"}.
#'
#' @param \dots Names and values for arguments.
#' @param .load.rds Whether to automatically load
#' files with \code{.rds} extensions. Defaults to \code{TRUE}.
#' 
#' @return \code{file_args} returns a named list of objects
#' invisibly, but is normally called for its side effect,
#' which is to create objects in the current environment.
#'
#' @seealso \code{file_args} uses function
#' \code{\link[base]{interactive}} to decide
#' whether a function is interactive. It uses
#' function \code{\link[base]{commandArgs}}
#' to access command line arguments.
#'
#' @examples
#' \dontrun{
#' file_args(p_inputs = "data/inputs.csv",
#'           fitted_values = "out/fitted_values.rds",
#'           variant = "low",
#'           size = 12)
#' }
#' @export
file_args <- function(..., .load.rds = TRUE) {
    envir <- parent.frame()
    args_dots <- list(...)
    check_args_dots(args_dots)
    checkmate::assert_flag(.load.rds)
    if (interactive())
        args <- args_dots
    else {
        args_cmd <- get_args_cmd()
        check_args_cmd(args_cmd = args_cmd,
                       args_dots = args_dots)
        args_cmd <- align_cmd_to_dots(args_cmd = args_cmd,
                                      args_dots = args_dots)
        args_cmd <- coerce_to_dots_class(args_cmd = args_cmd,
                                         args_dots = args_dots)
        args <- args_cmd
    }
    if (.load.rds)
        args <- replace_rds_with_obj(args)
    assign_args(args = args,
                envir = envir)
}
