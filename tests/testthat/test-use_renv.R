
## 'script_dir' ---------------------------------------------------------------

test_that("'script_dir' returns NULL outside Rscript --file and littler", {
  args <- commandArgs(trailingOnly = FALSE)
  if (any(grepl("^--file=", args)))
    skip("Running with --file=")
  if (length(args) >= 1L && identical(args[[1L]], "littler"))
    skip("Running under littler")
  expect_null(script_dir())
})

test_that("'script_dir' finds script directory under Rscript from other cwd", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "src"))
  dir_other <- tempfile()
  dir.create(dir_other)
  script <- file.path(dir_tmp, "src", "probe.R")
  out <- file.path(dir_tmp, "result.rds")
  write_script_dir_probe(script, out)
  on.exit({
    setwd(dir_curr)
    unlink(dir_tmp, recursive = TRUE)
    unlink(dir_other, recursive = TRUE)
  })
  setwd(dir_other)
  status <- system2(file.path(R.home("bin"), "Rscript"), script,
                    stdout = FALSE, stderr = FALSE)
  expect_identical(as.integer(status), 0L)
  expect_identical(readRDS(out),
                   normalizePath(file.path(dir_tmp, "src"), winslash = "/"))
})

test_that("'script_dir' finds script directory under littler from other cwd", {
  skip_if_no_littler_available()
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "src"))
  dir_other <- tempfile()
  dir.create(dir_other)
  script <- file.path(dir_tmp, "src", "probe.R")
  out <- file.path(dir_tmp, "result.rds")
  write_script_dir_probe(script, out)
  on.exit({
    setwd(dir_curr)
    unlink(dir_tmp, recursive = TRUE)
    unlink(dir_other, recursive = TRUE)
  })
  setwd(dir_other)
  cmd <- if (Sys.info()[["sysname"]] == "Darwin") "lr" else "r"
  status <- system2(cmd, script, stdout = FALSE, stderr = FALSE)
  expect_identical(as.integer(status), 0L)
  expect_identical(readRDS(out),
                   normalizePath(file.path(dir_tmp, "src"), winslash = "/"))
})


## 'find_renv_root' -----------------------------------------------------------

test_that("'find_renv_root' finds root via renv/activate.R", {
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "renv"))
  writeLines("# stub", file.path(dir_tmp, "renv", "activate.R"))
  dir.create(file.path(dir_tmp, "src"))
  ans <- find_renv_root(file.path(dir_tmp, "src"))
  expect_identical(ans, normalizePath(dir_tmp, winslash = "/"))
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'find_renv_root' finds root via renv.lock", {
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  writeLines("{}", file.path(dir_tmp, "renv.lock"))
  ans <- find_renv_root(dir_tmp)
  expect_identical(ans, normalizePath(dir_tmp, winslash = "/"))
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'find_renv_root' returns NULL when no project", {
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  expect_null(find_renv_root(dir_tmp))
  unlink(dir_tmp, recursive = TRUE)
})


## 'renv_active_for' ----------------------------------------------------------

test_that("'renv_active_for' is FALSE when RENV_PROJECT unset", {
  old <- Sys.getenv("RENV_PROJECT", unset = NA_character_)
  Sys.unsetenv("RENV_PROJECT")
  on.exit({
    if (is.na(old)) Sys.unsetenv("RENV_PROJECT") else Sys.setenv(RENV_PROJECT = old)
  })
  expect_false(renv_active_for(getwd()))
})

test_that("'renv_active_for' is TRUE when RENV_PROJECT matches", {
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  root <- normalizePath(dir_tmp, winslash = "/")
  old <- Sys.getenv("RENV_PROJECT", unset = NA_character_)
  Sys.setenv(RENV_PROJECT = root)
  on.exit({
    if (is.na(old)) Sys.unsetenv("RENV_PROJECT") else Sys.setenv(RENV_PROJECT = old)
    unlink(dir_tmp, recursive = TRUE)
  })
  expect_true(renv_active_for(root))
})


## 'use_renv' -----------------------------------------------------------------

test_that("'use_renv' returns NULL when no project", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  old <- Sys.getenv("RENV_PROJECT", unset = NA_character_)
  Sys.unsetenv("RENV_PROJECT")
  setwd(dir_tmp)
  on.exit({
    setwd(dir_curr)
    if (is.na(old)) Sys.unsetenv("RENV_PROJECT") else Sys.setenv(RENV_PROJECT = old)
    unlink(dir_tmp, recursive = TRUE)
  })
  expect_null(use_renv())
})

test_that("'use_renv' sources activate.R for explicit project", {
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "renv"))
  marker <- file.path(dir_tmp, "activated.txt")
  writeLines(sprintf('cat("ok", file = "%s")',
                     gsub("\\\\", "/", marker)),
             file.path(dir_tmp, "renv", "activate.R"))
  old <- Sys.getenv("RENV_PROJECT", unset = NA_character_)
  Sys.unsetenv("RENV_PROJECT")
  on.exit({
    if (is.na(old)) Sys.unsetenv("RENV_PROJECT") else Sys.setenv(RENV_PROJECT = old)
    unlink(dir_tmp, recursive = TRUE)
  })
  ans <- use_renv(project = dir_tmp)
  expect_identical(ans, normalizePath(dir_tmp, winslash = "/"))
  expect_true(file.exists(marker))
})

test_that("'use_renv' is idempotent when RENV_PROJECT already set", {
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "renv"))
  root <- normalizePath(dir_tmp, winslash = "/")
  writeLines('stop("should not source activate.R")',
             file.path(dir_tmp, "renv", "activate.R"))
  old <- Sys.getenv("RENV_PROJECT", unset = NA_character_)
  Sys.setenv(RENV_PROJECT = root)
  on.exit({
    if (is.na(old)) Sys.unsetenv("RENV_PROJECT") else Sys.setenv(RENV_PROJECT = old)
    unlink(dir_tmp, recursive = TRUE)
  })
  expect_identical(use_renv(project = dir_tmp), root)
})

test_that("'use_renv' with only renv.lock warns and returns NULL", {
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  writeLines("{}", file.path(dir_tmp, "renv.lock"))
  old <- Sys.getenv("RENV_PROJECT", unset = NA_character_)
  Sys.unsetenv("RENV_PROJECT")
  on.exit({
    if (is.na(old)) Sys.unsetenv("RENV_PROJECT") else Sys.setenv(RENV_PROJECT = old)
    unlink(dir_tmp, recursive = TRUE)
  })
  expect_warning(
    ans <- use_renv(project = dir_tmp),
    "renv.lock"
  )
  expect_null(ans)
})

test_that("'use_renv' errors when project path missing", {
  expect_error(use_renv(project = tempfile()),
               "Can't find project directory")
})

test_that("'use_renv' discovers project by walking up from getwd()", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "renv"))
  dir.create(file.path(dir_tmp, "src"))
  marker <- file.path(dir_tmp, "activated.txt")
  writeLines(sprintf('cat("ok", file = "%s")',
                     gsub("\\\\", "/", marker)),
             file.path(dir_tmp, "renv", "activate.R"))
  setwd(file.path(dir_tmp, "src"))
  old <- Sys.getenv("RENV_PROJECT", unset = NA_character_)
  Sys.unsetenv("RENV_PROJECT")
  on.exit({
    setwd(dir_curr)
    if (is.na(old)) Sys.unsetenv("RENV_PROJECT") else Sys.setenv(RENV_PROJECT = old)
    unlink(dir_tmp, recursive = TRUE)
  })
  ans <- use_renv()
  expect_identical(ans, normalizePath(dir_tmp, winslash = "/"))
  expect_true(file.exists(marker))
})
