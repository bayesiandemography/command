
#' Process command line arguments
#'
#' Create objects in the current environment
#' based on values passed at the command line,
#' or on values supplied via \dots.
#'
#' The behaviour of \code{file_args} depends the
#' on the type of R session it is called from:
#' (i) an interactive session, or (ii) a session
#' invoked from the command line, e.g
#' via \code{\link[utils]{Rscript}}. The typical
#' pattern is to use interactive sessions
#' when first developing an R script, and
#' then to call use the command line when the
#' code is mature.
#'
#' In an interactive session, \code{file_args}
#' creates values based on the default values
#' specified in \dots. In a session
#' invoked from the command
#' line, \code{file_args} creates values based on
#' arguments passed at the command line.
#' When processing arguments passed at the command
#' line, \code{file_args} assigns names to values
#' in much the same way that R does when it
#' processes a function call. If an argument
#' is named when it is passed at the command line,
#' then it keeps that name. If an argument is
#' unnamed, then it gets the first unassigned
#' name in \dots.
#'
#' \code{file_args} assumes that named arguments passed
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
#' processes arguments with one dash and arguments
#' with two dashes the same way.
#'
#' \code{file_args} treats filenames with an
#' \code{.rds} extension differently from other filenames.
#' If \code{file_args} sees that a filename has
#' a \code{.rds} extension, it checks whether the
#' argunment name in \dots starts with \code{"p_"},
#' \code{"p."} or \code{"p<capital letter>"}.
#' If it does, then \code{file_args}
#' proceeds as normal, and assigns the filename
#' in the the name in the current environment. If
#' it does not, then \code{file_args} actually
#' loads the file, using function
#' \code{\link[base]{readRDS}}.For instance,
#' in an interactive session,
#'
#' \code{file_args(p_out = "myfile.rds")}
#'
#' is equivalent to
#'
#' \code{p_out <- "myfile.rds"}
#'
#' while
#'
#' \code{file_args(out = "myfile.rds")}
#'
#' is equivalent to
#'
#' \code{out <- readRDS("myfile.rds")}.
#'
#' (The "p" is short for "path" or "pointer".)
#'
#' @param \dots Names and values for arguments.
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
#'           size = 12,
#'           p_outfile = "out/model_summary.rds")
#' }
#' @export
file_args <- function(...) {
    envir <- parent.frame()
    args_dots <- list(...)
    check_args_dots(args_dots)
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
    args <- replace_rds_with_obj(args)
    assign_args(args = args,
                envir = envir)
}
