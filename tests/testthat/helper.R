
littler_available <- function() {
  os <- Sys.info()[["sysname"]]
  if (os == "Windows")
    return(FALSE)
  cmd <- if (os == "Darwin") "lr" else "r"
  path <- Sys.which(cmd)
  if (!nzchar(path))
    return(FALSE)
  out <- suppressWarnings(system2(cmd, "--version", stdout = TRUE, stderr = TRUE))
  if (!length(out))
    out <- suppressWarnings(system2(cmd, "-h", stdout = TRUE, stderr = TRUE))
  any(grepl("\\blittler\\b", out, ignore.case = TRUE))
}

skip_if_no_littler_available <- function() {
  if (!littler_available())
    testthat::skip("littler not callable via system()")
}
