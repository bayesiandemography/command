
littler_available <- function() {
  os <- Sys.info()[["sysname"]]
  if (os == "Windows")
    return(FALSE)
  cmd <- if (os == "Darwin") "lr" else "r"
  path <- Sys.which(cmd)
  if (!nzchar(path))
    return(FALSE)
  ## Confirm the binary identifies as littler (not some other 'r'/'lr').
  out <- suppressWarnings(system2(cmd, "--version", stdout = TRUE, stderr = TRUE))
  if (!length(out))
    out <- suppressWarnings(system2(cmd, "-h", stdout = TRUE, stderr = TRUE))
  if (!any(grepl("\\blittler\\b", out, ignore.case = TRUE)))
    return(FALSE)
  ## Also confirm it can actually execute R code. A stale binary linked
  ## against an older R can still answer --version but segfault on use.
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines("invisible(NULL)", script)
  status <- suppressWarnings(
    system2(cmd, script, stdout = FALSE, stderr = FALSE)
  )
  identical(as.integer(status), 0L)
}

skip_if_no_littler_available <- function() {
  if (!littler_available())
    testthat::skip("littler not callable via system()")
}

## Probe scripts for subprocess tests of script_dir(). Under R CMD check
## the package is installed; pass the parent .libPaths() so the child
## finds that install without needing devtools::load_all().
write_script_dir_probe <- function(script, out) {
  libs <- paste(encodeString(.libPaths(), quote = '"'), collapse = ", ")
  writeLines(c(
    sprintf(".libPaths(c(%s))", libs),
    "library(command)",
    sprintf('saveRDS(command:::script_dir(), "%s")',
            gsub("\\\\", "/", out))
  ), script)
}
