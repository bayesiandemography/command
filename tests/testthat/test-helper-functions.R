
## 'path_join' ----------------------------------------------------------------

test_that("'path_join' joins with forward slashes", {
  expect_identical(path_join("a", "b", "c"), "a/b/c")
  expect_identical(path_join(".", "src/script.R"), "src/script.R")
  expect_identical(path_join("."), ".")
  expect_identical(path_join("src", ".", "script.R"), "src/script.R")
})

test_that("'path_join' rewrites backslashes as forward slashes", {
  ## tempfile(tmpdir = getwd()) on Windows inserts '\\' after a
  ## directory that already uses '/'. That backslash must not
  ## survive into a shell command.
  expect_identical(path_join("D:/a/testthat\\file123", "script.R"),
                   "D:/a/testthat/file123/script.R")
  expect_identical(path_join("C:\\proj", "src\\script.R"),
                   "C:/proj/src/script.R")
})


## 'is_absolute_path' ---------------------------------------------------------

test_that("'is_absolute_path' recognises absolute paths", {
  expect_true(is_absolute_path("/tmp"))
  expect_true(is_absolute_path("C:/"))
  expect_true(is_absolute_path("C:\\Windows"))
  expect_true(is_absolute_path("\\\\server\\share"))
  expect_false(is_absolute_path("src/script.R"))
  expect_false(is_absolute_path("./script.R"))
  expect_false(is_absolute_path("~/script.R"))
})


## 'path_rel' -----------------------------------------------------------------

test_that("'path_rel' makes paths relative to start", {
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "src"))
  writeLines("1", con = file.path(dir_tmp, "src/script.R"))
  expect_identical(path_rel(file.path(dir_tmp, "src/script.R"), start = dir_tmp),
                   "src/script.R")
  expect_identical(path_rel(dir_tmp, start = dir_tmp), ".")
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'path_rel' works when tempfile is under getwd() on Windows", {
  skip_if_not(.Platform$OS.type == "windows")
  ## tempfile()/getwd() can disagree on drive-letter case after
  ## normalizePath() (e.g. d: vs D:), which broke extract_make() on WinBuilder.
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  writeLines("1", con = file.path(dir_tmp, "script.R"))
  rel <- path_rel(file.path(dir_tmp, "script.R"), start = getwd())
  expect_false(is_absolute_path(rel))
  expect_match(rel, "script\\.R$")
})

test_that("'path_rel' errors when path is not under start", {
  dir_a <- tempfile()
  dir_b <- tempfile()
  dir.create(dir_a)
  dir.create(dir_b)
  on.exit({
    unlink(dir_a, recursive = TRUE)
    unlink(dir_b, recursive = TRUE)
  }, add = TRUE)
  expect_error(path_rel(dir_a, start = dir_b), "Can't make")
})


## 'align_cmd_to_dots' --------------------------------------------------------

test_that("'align_cmd_to_dots' works on typical inputs supplied by makefile", {
  args_cmd <- list("file1", "file2", n_iter = 100, variant = "low")
  args_dots <- list(f1 = "f1", f2 = "f2", variant = "med", n_iter = 10)
  ans_obtained <- align_cmd_to_dots(args_cmd = args_cmd,
                                    args_dots = args_dots)
  ans_expected <- list(f1 = "file1", f2 = "file2", variant = "low", n_iter = 100)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'align_cmd_to_dots' works when named argument comes before unnamed", {
  args_cmd <- list("file1", variant = "low", "file2", n_iter = 100)
  args_dots <- list(f1 = "f1", f2 = "f2", variant = "med", n_iter = 10)
  ans_obtained <- align_cmd_to_dots(args_cmd = args_cmd,
                                    args_dots = args_dots)
  ans_expected <- list(f1 = "file1", f2 = "file2", variant = "low", n_iter = 100)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'align_cmd_to_dots' works when single unnamed argument", {
  args_cmd <- list("file1")
  args_dots <- list(f1 = "f1")
  ans_obtained <- align_cmd_to_dots(args_cmd = args_cmd,
                                    args_dots = args_dots)
  ans_expected <- list(f1 = "file1")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'align_cmd_to_dots' works 'args_cmd' and 'args_dots' both length 0", {
  args_cmd <- list()
  args_dots <- list()
  ans_obtained <- align_cmd_to_dots(args_cmd = args_cmd,
                                    args_dots = args_dots)
  ans_expected <- list()
  expect_identical(ans_obtained, ans_expected)
})


## 'align_cmd_dots' -----------------------------------------------------------

test_that("'align_cmd_dots' puts every unnamed value into the leftover argument", {
  args_cmd <- list("data/a.csv", "data/c.csv", "out/combined.rds",
                   use_log = "TRUE")
  args_dots <- list(files = c("data/a.csv", "out/combined.rds"),
                    use_log = TRUE)
  ans_obtained <- align_cmd_dots(args_cmd = args_cmd,
                                 args_dots = args_dots)
  ans_expected <- list(files = c("data/a.csv", "data/c.csv", "out/combined.rds"),
                       use_log = "TRUE")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'align_cmd_dots' uses an empty character vector when nothing is unnamed", {
  args_cmd <- list(use_log = "FALSE")
  args_dots <- list(files = c("data/a.csv", "out/combined.rds"),
                    use_log = TRUE)
  ans_obtained <- align_cmd_dots(args_cmd = args_cmd,
                                 args_dots = args_dots)
  ans_expected <- list(files = character(),
                       use_log = "FALSE")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'align_cmd_dots' treats a nameless command-line list as unnamed", {
  args_cmd <- list("data/a.csv", "out/combined.rds")
  args_dots <- list(files = "out/combined.rds")
  ans_obtained <- align_cmd_dots(args_cmd = args_cmd,
                                 args_dots = args_dots)
  ans_expected <- list(files = c("data/a.csv", "out/combined.rds"))
  expect_identical(ans_obtained, ans_expected)
})


## 'assign_args' --------------------------------------------------------------

test_that("'assign_args' creates correct objects in the parent frame, and returns as list", {
  args <- list(xx = 1, yy = "hello", zz = TRUE)
  envir <- new.env()
  ans_obtained <- assign_args(args = args, envir = envir, quiet = TRUE)
  expect_identical(envir$xx, 1)
  expect_identical(envir$yy, "hello")
  expect_identical(envir$zz, TRUE)
  expect_identical(ans_obtained, args)
})

test_that("'assign_args' creates expected message", {
  args <- list(xx = 1, yy = "hello", zz = TRUE)
  envir <- new.env()
  suppressMessages(
    expect_message(
      assign_args(args = args, envir = envir, quiet = FALSE),
      "Assigned object"
    )
  )
})

test_that("'assign_args' notes empty string in message", {
  args <- list(v = "")
  envir <- new.env()
  suppressMessages(
    expect_message(
      assign_args(args = args, envir = envir, quiet = FALSE),
      "an empty string"
    )
  )
})

test_that("'assign_args' works with empty args", {
  args <- list()
  envir <- new.env()
  ans_obtained <- assign_args(args = args, envir = envir, quiet = TRUE)
  expect_identical(length(envir), 0L)
  expect_identical(ans_obtained, args)
})
    

## 'extract_cmd_call' ---------------------------------------------------------

test_that("'extract_cmd_call' finds a qualified quiet call inside if", {
  dir_tmp <- tempfile(tmpdir = getwd())
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  dir.create(dir_tmp)
  path <- file.path(dir_tmp, "script.R")
  writeLines(c("if (interactive()) {",
               "  command::cmd_assign_quiet(.out = 'out/a.rds', n = 1L)",
               "}",
               "cmd_assign(.out = 'out/b.rds', n = 2L)"),
             path)
  ans <- extract_cmd_call(path)
  expect_identical(ans$name, "cmd_assign_quiet")
  expect_identical(ans$args$.out, "out/a.rds")
  expect_identical(ans$args$n, 1L)
})

test_that("'extract_cmd_call' finds a collect call inside a function", {
  dir_tmp <- tempfile(tmpdir = getwd())
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  dir.create(dir_tmp)
  path <- file.path(dir_tmp, "script.R")
  writeLines(c("f <- function() {",
               "  command:::cmd_assign_collect_quiet(",
               "    files = c('data/a.csv', 'out/combined.rds'),",
               "    use_log = TRUE)",
               "}"),
             path)
  ans <- extract_cmd_call(path)
  expect_identical(ans$name, "cmd_assign_collect_quiet")
  expect_identical(ans$args$files, c("data/a.csv", "out/combined.rds"))
  expect_identical(ans$args$use_log, TRUE)
})

test_that("'extract_cmd_call' reads a call with no argument names", {
  dir_tmp <- tempfile(tmpdir = getwd())
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  dir.create(dir_tmp)
  path <- file.path(dir_tmp, "script.R")
  writeLines("cmd_assign('data/a.csv', 'out/b.rds')", path)
  ans <- extract_cmd_call(path)
  expect_identical(ans$name, "cmd_assign")
  expect_null(names(ans$args))
  expect_identical(ans$args, list("data/a.csv", "out/b.rds"))
})

test_that("'extract_cmd_call' does not evaluate names or other calls", {
  dir_tmp <- tempfile(tmpdir = getwd())
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  dir.create(dir_tmp)
  i <- 99
  path_name <- file.path(dir_tmp, "name.R")
  writeLines(c("n <- 7",
               "cmd_assign(i = i)"),
             path_name)
  expect_error(extract_cmd_call(path_name),
               "Can't read argument `i`")
  surprise <- file.path(dir_tmp, "surprise.txt")
  path_side <- file.path(dir_tmp, "side.R")
  writeLines(sprintf("cmd_assign(.out = {writeLines('created', '%s'); 'out/x.rds'})",
                     surprise),
             path_side)
  expect_error(extract_cmd_call(path_side),
               "Can't read argument")
  expect_false(file.exists(surprise))
})


## 'read_cmd_arg' -------------------------------------------------------------

test_that("'read_cmd_arg' reads constants, c(), and class constructors", {
  expect_identical(read_cmd_arg(quote(1L), "n", "script.R"), 1L)
  expect_identical(read_cmd_arg(quote(-1), "n", "script.R"), -1)
  expect_identical(read_cmd_arg(quote((-1L)), "n", "script.R"),
                   base::`-`(1L))
  expect_identical(read_cmd_arg(quote(NULL), "k", "script.R"), NULL)
  expect_identical(read_cmd_arg(quote(c("data/a.csv", "out/b.rds")), "files", "script.R"),
                   c("data/a.csv", "out/b.rds"))
  expect_identical(read_cmd_arg(quote(as.integer("3")), "n", "script.R"), 3L)
  expect_identical(read_cmd_arg(quote(as.numeric(1L)), "n", "script.R"), 1)
  expect_identical(read_cmd_arg(quote(as.logical("TRUE")), "flag", "script.R"), TRUE)
  expect_identical(read_cmd_arg(quote(as.Date("01-01-2025", format = "%d-%m-%Y")),
                                "date", "script.R"),
                   as.Date("2025-01-01"))
  expect_identical(read_cmd_arg(quote(as.POSIXct("2025-01-01", tz = "UTC")),
                                "date", "script.R"),
                   as.POSIXct("2025-01-01", tz = "UTC"))
})

test_that("'read_cmd_arg' reads logical symbols and missing-value symbols", {
  expect_identical(read_cmd_arg(quote(T), "flag", "script.R"), TRUE)
  expect_identical(read_cmd_arg(quote(F), "flag", "script.R"), FALSE)
  expect_identical(read_cmd_arg(as.symbol("TRUE"), "flag", "script.R"), TRUE)
  expect_identical(read_cmd_arg(as.symbol("FALSE"), "flag", "script.R"), FALSE)
  expect_identical(read_cmd_arg(as.symbol("NULL"), "k", "script.R"), NULL)
  expect_identical(read_cmd_arg(as.symbol("NA"), "k", "script.R"), NA)
  expect_identical(read_cmd_arg(as.symbol("NA_integer_"), "k", "script.R"),
                   NA_integer_)
  expect_identical(read_cmd_arg(as.symbol("NA_real_"), "k", "script.R"),
                   NA_real_)
  expect_identical(read_cmd_arg(as.symbol("NA_character_"), "k", "script.R"),
                   NA_character_)
  expect_identical(read_cmd_arg(as.symbol("Inf"), "k", "script.R"), Inf)
  expect_identical(read_cmd_arg(as.symbol("NaN"), "k", "script.R"), NaN)
})

test_that("'read_cmd_arg' rejects a name, file.path(), and paste()", {
  expect_error(read_cmd_arg(quote(n), "n", "script.R"),
               "Can't read argument `n`")
  expect_error(read_cmd_arg(quote(file.path("data", "x.csv")), ".data", "script.R"),
               "file.path")
  expect_error(read_cmd_arg(quote(paste0("out/", "x.rds")), ".out", "script.R"),
               "paste0")
  expect_error(read_cmd_arg(quote(file.path("data", "x.csv")), "", "script.R"),
               "unnamed argument")
})

test_that("'read_cmd_arg' rejects a non-call, a qualified call, and a bad minus", {
  expect_error(read_cmd_arg(list(1), "n", "script.R"),
               "Can't read argument `n`")
  expect_error(read_cmd_arg(quote(base::as.integer(1)), "n", "script.R"),
               "Can't read argument `n`")
  expect_error(read_cmd_arg(quote(-"a"), "n", "script.R"),
               "Can't read argument `n`")
})

test_that("'read_cmd_arg' rejects a constructor with the wrong arguments", {
  expect_error(read_cmd_arg(quote(as.integer()), "n", "script.R"),
               "Can't read argument `n`")
  expect_error(read_cmd_arg(quote(as.integer(1, 2)), "n", "script.R"),
               "Can't read argument `n`")
  expect_error(read_cmd_arg(quote(as.Date("2020-01-01", origin = "1970-01-01")),
                            "date", "script.R"),
               "Can't read argument `date`")
  expect_error(read_cmd_arg(quote(as.POSIXct("2020-01-01", tz = c("UTC", "GMT"))),
                            "date", "script.R"),
               "Can't read argument `date`")
  expect_error(read_cmd_arg(quote(as.POSIXlt("2020-01-01", format = 1)),
                            "date", "script.R"),
               "Can't read argument `date`")
})

test_that("'extract_shell_if_possible' accepts a quiet collect call", {
  dir_tmp <- tempfile(tmpdir = getwd())
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  dir.create(dir_tmp)
  file <- "script.R"
  writeLines(paste("command::cmd_assign_collect_quiet(",
                   "files = c('data/a.csv', 'out/combined.rds'),",
                   "use_log = TRUE)"),
             con = file.path(dir_tmp, file))
  ans_obtained <- extract_shell_if_possible(file = file,
                                           dir_shell = dir_tmp,
                                           quiet = TRUE)
  ans_expected <- paste0("Rscript script.R \\\n",
                         "  data/a.csv \\\n",
                         "  out/combined.rds \\\n",
                         "  --use_log=TRUE\n")
  expect_identical(ans_obtained, ans_expected)
})


## 'extract_shell_if_possible' ------------------------------------------------

test_that("'extract_shell_if_possible' works with R file with cmd_assign", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "script.R"
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = file.path(dir_tmp, file))
  ans_obtained <- extract_shell_if_possible(file = file,
                                            dir_shell = dir_tmp,
                                            quiet = TRUE)
  ans_expected <- paste0("Rscript script.R \\\n",
                         "  data/mydata.csv \\\n",
                         "  out/cleaned.rds \\\n",
                         "  --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_shell_if_possible' throws expected message", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "script.R"
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = file.path(dir_tmp, file))
  expect_message(
    extract_shell_if_possible(file = file,
                              dir_shell = dir_tmp,
                              quiet = FALSE),
    "Extracted call to `cmd_assign\\(\\)")
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_shell_if_possible' returns NULL with R file with no cmd_assign", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "script.R"
  writeLines("x <- 1",
             con = file.path(dir_tmp, "script.R"))
  ans_obtained <- extract_shell_if_possible(file = file,
                                            dir_shell = dir_tmp,
                                            quiet = TRUE)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_shell_if_possible' returns NULL with non-R file", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "data.csv"
  write.csv(data.frame(a = 1, b = 2), file = file.path(dir_tmp, "data.csv"))
  ans_obtained <- extract_shell_if_possible(file = file,
                                            dir_shell = dir_tmp,
                                            quiet = TRUE)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_shell_if_possible' throws error when cannot evaluate", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "script.r"
  writeLines("cmd_assign(.data = 'data/mydata.csv', FALSE, .out = 'out/cleaned.rds')",
             con = file.path(dir_tmp, file))
  expect_error(extract_shell_if_possible(file = file,
                                         dir_shell = dir_tmp,
                                         quiet = TRUE),
               "Problem extracting call to")
  unlink(dir_tmp, recursive = TRUE)
})


## 'extract_make_if_possible' -------------------------------------------------

test_that("'extract_make_if_possible' works with R file with cmd_assign", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "script.R"
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = file.path(dir_tmp, file))
  ans_obtained <- extract_make_if_possible(file = file,
                                           dir_make = dir_tmp,
                                           quiet = TRUE)
  ans_expected <- paste0("out/cleaned.rds: ", file, " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_make_if_possible' throws expected message", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "script.R"
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = file.path(dir_tmp, file))
  expect_message(
    extract_make_if_possible(file = file,
                             dir_make = dir_tmp,
                             quiet = FALSE),
    "Extracted call to")
  unlink(dir_tmp, recursive = TRUE)
})


test_that("'extract_make_if_possible' returns NULL with R file with no cmd_assign", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "script.R"
  writeLines("x <- 1",
             con = file.path(dir_tmp, "script.R"))
  ans_obtained <- extract_make_if_possible(file = file,
                                           dir_make = dir_tmp,
                                           quiet = TRUE)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_make_if_possible' returns NULL with non-R file", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "data.csv"
  write.csv(data.frame(a = 1, b = 2), file = file.path(dir_tmp, "data.csv"))
  ans_obtained <- extract_make_if_possible(file = file,
                                           dir_make = dir_tmp,
                                           quiet = TRUE)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_make_if_possible' throws error when cannot evaluate", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "script.r"
  writeLines("cmd_assign(.data = 'data/mydata.csv', FALSE, .out = 'out/cleaned.rds')",
             con = file.path(dir_tmp, file))
  expect_error(extract_make_if_possible(file = file,
                                        dir_make = dir_tmp,
                                        quiet = TRUE),
               "Problem extracting call to")
  unlink(dir_tmp, recursive = TRUE)
})


## 'coerce_arg_cmd' -----------------------------------------------------------

test_that("'coerce_arg_cmd' works with character", {
  ans_obtained <- coerce_arg_cmd(arg_cmd = "X",
                                 arg_dots = "Y",
                                 nm_cmd = "",
                                 nm_dots = "val")
  ans_expected <- "X"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'coerce_arg_cmd' allows empty string for character", {
  ans_obtained <- coerce_arg_cmd(arg_cmd = "",
                                 arg_dots = "a",
                                 nm_cmd = "v",
                                 nm_dots = "v")
  expect_identical(ans_obtained, "")
})

test_that("'coerce_arg_cmd' rejects empty string for non-character", {
  expect_error(coerce_arg_cmd(arg_cmd = "",
                             arg_dots = 1L,
                             nm_cmd = "v",
                             nm_dots = "v"),
               "Empty value passed at command line for argument `v`.")
  expect_error(coerce_arg_cmd(arg_cmd = "",
                             arg_dots = TRUE,
                             nm_cmd = "v",
                             nm_dots = "v"),
               "Empty value passed at command line for argument `v`.")
  expect_error(coerce_arg_cmd(arg_cmd = "",
                             arg_dots = as.Date("2000-01-01"),
                             nm_cmd = "v",
                             nm_dots = "v"),
               "Empty value passed at command line for argument `v`.")
})

test_that("'coerce_arg_cmd' works with integer", {
  ans_obtained <- coerce_arg_cmd(arg_cmd = "111",
                                 arg_dots = 1L,
                                 nm_cmd = "",
                                 nm_dots = "val")
  ans_expected <- 111L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'coerce_arg_cmd' works with numeric", {
  ans_obtained <- coerce_arg_cmd(arg_cmd = "111",
                                 arg_dots = 1.0,
                                 nm_cmd = "",
                                 nm_dots = "val")
  ans_expected <- 111.0
  expect_identical(ans_obtained, ans_expected)
})

test_that("'coerce_arg_cmd' works with logical", {
  ans_obtained <- coerce_arg_cmd(arg_cmd = "F",
                                 arg_dots = NA,
                                 nm_cmd = "",
                                 nm_dots = "val")
  ans_expected <- FALSE
  expect_identical(ans_obtained, ans_expected)
})

test_that("'coerce_arg_cmd' works with Date", {
  ans_obtained <- coerce_arg_cmd(arg_cmd = "2001-02-01",
                                 arg_dots = as.Date("2000-01-01"),
                                 nm_cmd = "",
                                 nm_dots = "val")
  ans_expected <- as.Date("2001-02-01")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'coerce_arg_cmd' works with POSIXct", {
  ans_obtained <- coerce_arg_cmd(arg_cmd = "2001-02-01 12:13:14",
                                 arg_dots = as.POSIXct("2000-01-01", tz = "EST"),
                                 nm_cmd = "",
                                 nm_dots = "val")
  ans_expected <- as.POSIXct("2001-02-01 12:13:14", tz = "EST")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'coerce_arg_cmd' works with POSIXlt", {
  ans_obtained <- coerce_arg_cmd(arg_cmd = "2001-02-01 12:13:14",
                                 arg_dots = as.POSIXlt("2000-01-01", tz = "EST"),
                                 nm_cmd = "",
                                 nm_dots = "val")
  ans_expected <- as.POSIXlt("2001-02-01 12:13:14", tz = "EST")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'coerce_arg_cmd' works with NULL", {
  ans_obtained <- coerce_arg_cmd(arg_cmd = "NULL",
                                 arg_dots = NULL,
                                 nm_cmd = "",
                                 nm_dots = "val")
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
})

test_that("'coerce_arg_cmd' throws expected error with unexpected class", {
  expect_error(coerce_arg_cmd(arg_cmd = "NULL",
                                 arg_dots = raw(),
                                 nm_cmd = "",
                              nm_dots = "val"),
               "Internal error: `arg_dots` has class")
})

test_that("'coerce_arg_cmd' throws expected error when can't coerce", {
  expect_error(coerce_arg_cmd(arg_cmd = "a",
                              arg_dots = 1L,
                              nm_cmd = "",
                              nm_dots = "val"),
               "Can't coerce value passed at command line to class specified by `cmd_assign\\(\\)")
})


## 'coerce_to_dots_class' -----------------------------------------------------

test_that("'coerce_to_dots_class' works with valid inputs", {
    args_cmd <- list(a = "TRUE", b = "1", c = "1", d = "1")
    args_dots <- list(a = NA, b = 1L, c = 1, d = "1")
    ans_obtained <- coerce_to_dots_class(args_cmd = args_cmd,
                                         args_dots = args_dots)
    ans_expected <- list(a = TRUE, b = 1L, c = 1, d = "1")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'coerce_to_dots_class' throws correct error when cannot coerce", {
    args_cmd <- list(a = "TRUE", b = "1", c = "a", d = "1")
    args_dots <- list(a = NA, b = 1L, c = 1, d = "1")
    expect_error(coerce_to_dots_class(args_cmd = args_cmd,
                                      args_dots = args_dots),
                 "Can't coerce value passed at command line")
})


## 'extract_args' -------------------------------------------------------------

test_that("'extract_args' works with valid inputs", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("x <- 1",
               "cmd_assign(a = 1L, b = as.Date('2020-01-01'), c = FALSE)",
               "y <- 2"),
             con = "script.R")
  ans_obtained <- extract_args("script.R")
  ans_expected <- list(a = 1L, b = as.Date("2020-01-01"), c = FALSE)
  expect_identical(ans_obtained, ans_expected)
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_args' gives warning when no call to cmd_assign() found", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("x <- 1",
               "y <- 2"),
             con = "script.R")
  expect_message(ans <- extract_args("script.R"),
                 "No call to `cmd_assign\\(\\)` or `cmd_assign_collect\\(\\)` found.")
  expect_identical(ans, NULL)
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})


## 'format_args_make' ---------------------------------------------------------

test_that("'format_args_make works with file and non-file args", {
  file <- "src/bla.R"
  args <- list(a = 1, b = "x.R", c = "out/bla.rds")
  ans_obtained <- format_args_make(file = file, args = args)
  ans_expected <- paste(c("out/bla.rds: src/bla.R \\",
                          "  x.R",
                          "\tRscript $^ $@ --a=1\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_make works with one file and one non-file args", {
  file <- "src/bla.R"
  args <- list(a = 1, c = "out/bla.rds")
  ans_obtained <- format_args_make(file = file, args = args)
  ans_expected <- paste(c("out/bla.rds: src/bla.R",
                          "\tRscript $^ $@ --a=1\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_make works with file and no non-file args", {
  file <- "src/bla.R"
  args <- list(b = "x.R", c = "out/bla.rds")
  ans_obtained <- format_args_make(file = file, args = args)
  ans_expected <- paste(c("out/bla.rds: src/bla.R \\",
                          "  x.R",
                          "\tRscript $^ $@\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_make works with one file and no non-file args", {
  file <- "src/bla.R"
  args <- list(c = "out/bla.rds")
  ans_obtained <- format_args_make(file = file, args = args)
  ans_expected <- paste(c("out/bla.rds: src/bla.R",
                          "\tRscript $^ $@\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_make throws error when no file arguments", {
  file <- "src/bla.R"
  args <- list(c = 1L)
  expect_error(format_args_make(file = file, args = args),
               "Can't find any file arguments.")
})

test_that("'format_args_make' expands a character vector of paths", {
  file <- "src/combine.R"
  args <- list(files = c("data/a.csv", "data/b.csv", "out/combined.rds"),
               use_log = TRUE)
  ans_obtained <- format_args_make(file = file, args = args)
  ans_expected <- paste(c("out/combined.rds: src/combine.R \\",
                          "  data/a.csv \\",
                          "  data/b.csv",
                          "\tRscript $^ $@ --use_log=TRUE\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_make' quotes a named value that is not a plain token", {
  file <- "src/fit.R"
  args <- list(.data = "my data.csv",
               title = "a b",
               keep = NULL,
               .out = "out/fit.rds")
  ans_obtained <- format_args_make(file = file, args = args)
  ans_expected <- paste(c("out/fit.rds: src/fit.R \\",
                          "  my data.csv",
                          "\tRscript $^ $@ --title='a b' \\",
                          "                      --keep=NULL\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_make' errors when a vector has no target path", {
  file <- "src/combine.R"
  args <- list(.files = character())
  expect_error(format_args_make(file = file, args = args),
               "Can't find any file arguments.")
})


## 'format_args_shell' --------------------------------------------------------

test_that("'format_args_shell works with file and non-file args", {
  file <- "src/myfile.R"
  args <- list(a = 1, b = "x.R")
  ans_obtained <- format_args_shell(file = file, args = args)
  ans_expected <- paste(c("Rscript src/myfile.R \\",
                          "  x.R \\",
                          "  --a=1\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_shell works with file arg only", {
  file <- "src/myfile.R"
  args <- list(b = "x.R")
  ans_obtained <- format_args_shell(file = file, args = args)
  ans_expected <- paste(c("Rscript src/myfile.R \\",
                          "  x.R\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_shell works with non-file arg only", {
  file <- "src/myfile.R"
  args <- list(a = 1)
  ans_obtained <- format_args_shell(file = file, args = args)
  ans_expected <- paste(c("Rscript src/myfile.R \\",
                          "  --a=1\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_shell' expands a character vector of paths", {
  file <- "src/combine.R"
  args <- list(files = c("data/a.csv", "data/b.csv", "out/combined.rds"),
               use_log = TRUE)
  ans_obtained <- format_args_shell(file = file, args = args)
  ans_expected <- paste(c("Rscript src/combine.R \\",
                          "  data/a.csv \\",
                          "  data/b.csv \\",
                          "  out/combined.rds \\",
                          "  --use_log=TRUE\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_shell' quotes values that are not plain tokens", {
  file <- "src/my file.R"
  args <- list(.data = "my data.csv",
               title = "a b",
               keep = NULL,
               .out = "out/fit.rds")
  ans_obtained <- format_args_shell(file = file, args = args)
  ans_expected <- paste(c("Rscript 'src/my file.R' \\",
                          "  'my data.csv' \\",
                          "  out/fit.rds \\",
                          "  --title='a b' \\",
                          "  --keep=NULL\n"),
                        collapse = "\n")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_args_shell' rejects a named value whose length is not 1", {
  file <- "src/fit.R"
  args <- list(labels = c("a", "b"),
               .out = "out/fit.rds")
  expect_error(format_args_shell(file = file, args = args),
               "has length 2")
})

test_that("'format_args_shell works no args", {
  file <- "src/myfile.R"
  args <- list()
  ans_obtained <- format_args_shell(file = file, args = args)
  ans_expected <- "Rscript src/myfile.R\n"
  expect_identical(ans_obtained, ans_expected)
})


## 'get_args_cmd' -------------------------------------------------------------

test_that("'get_args_cmd' works with valid inputs", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("args <- command:::get_args_cmd()",
               "saveRDS(args, file = 'args.rds')"),
             con = "script.R")
  cmd <- sprintf('%s/bin/Rscript script.R unnamed1 -n=1 unnamed2 --named1=hello --named2="kia ora"', R.home())
  system(cmd)
  ans_obtained <- readRDS("args.rds")
  ans_expected <- list("unnamed1", n = "1", "unnamed2", named1 = "hello", named2 = "kia ora")
  expect_identical(ans_obtained, ans_expected)
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'get_args_cmd' works when no arguments passed", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("args <- command:::get_args_cmd()",
               "saveRDS(args, file = 'args.rds')"),
             con = "script.R")
  cmd <- sprintf("%s/bin/Rscript script.R", R.home())
  system(cmd)
  args <- readRDS("args.rds")
  expect_identical(args, list())
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'get_args_cmd' keeps equals signs that are part of the value", {
  local_mocked_bindings(
    commandArgs = function(trailingOnly = FALSE) {
      if (isTRUE(trailingOnly)) "--path=a=b" else "Rscript"
    },
    .package = "base"
  )
  expect_identical(get_args_cmd(), list(path = "a=b"))
})

test_that("'get_args_cmd' rejects a named argument with an empty name", {
  local_mocked_bindings(
    commandArgs = function(trailingOnly = FALSE) {
      if (isTRUE(trailingOnly)) "--=value" else "Rscript"
    },
    .package = "base"
  )
  expect_error(get_args_cmd(), "missing a name")
})

## Empty value after '=' (e.g. Make expands --v=$(V) when V is undefined)
test_that("'get_args_cmd' treats --v= as named argument with empty string", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("args <- command:::get_args_cmd()",
               "saveRDS(args, file = 'args.rds')"),
             con = "script.R")
  cmd <- sprintf("%s/bin/Rscript script.R --v=", R.home())
  system(cmd)
  ans_obtained <- readRDS("args.rds")
  ans_expected <- list(v = "")
  expect_identical(ans_obtained, ans_expected)
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'get_args_cmd' works with littler", {
  skip_if_no_littler_available()
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("args <- command:::get_args_cmd()",
               "saveRDS(args, file = 'args.rds')"),
             con = "script.R")
  cmd <- 'lr script.R unnamed1 -n=1 unnamed2 --named1=hello --named2="kia ora"'
  system(cmd)
  ans_obtained <- readRDS("args.rds")
  ans_expected <- list("unnamed1", n = "1", "unnamed2", named1 = "hello", named2 = "kia ora")
  expect_identical(ans_obtained, ans_expected)
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})




## 'is_actual_or_potential_file_path' -----------------------------------------

test_that("'is_actual_or_potential_file_path' returns FALSE if is non-character or NA", {
  expect_false(is_actual_or_potential_file_path(1))
  expect_false(is_actual_or_potential_file_path(FALSE))
  expect_false(is_actual_or_potential_file_path(NA_character_))
})

test_that("'is_actual_or_potential_file_path' returns TRUE if file exists", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines("1",
             con = "mydata.csv")
  ans <- is_actual_or_potential_file_path("mydata.csv")
  expect_true(ans)
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'is_actual_or_potential_file_path' returns TRUE if file is directory", {
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  expect_true(is_actual_or_potential_file_path(dir_tmp))
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'is_actual_or_potential_file_path' returns TRUE if is potential file", {
  expect_true(is_actual_or_potential_file_path("mydir/myfile.r"))
  expect_false(is_actual_or_potential_file_path(".777!"))
  expect_false(is_actual_or_potential_file_path("*"))
})


## 'is_file_arg' --------------------------------------------------------------

test_that("'is_file_arg' works with dot arguments", {
  args <- list(a = 1, .b = "x.r", c = FALSE, .d = "x.c")
  ans_obtained <- is_file_arg(args)
  ans_expected <- c(FALSE, TRUE, FALSE, TRUE)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'is_file_arg' works with non-dot arguments", {
  args <- list(a = 1, b = "x.r", c = FALSE, d = "x.c")
  ans_obtained <- is_file_arg(args)
  ans_expected <- c(FALSE, TRUE, FALSE, TRUE)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'is_file_arg' treats a path vector as one file argument", {
  args <- list(files = c("data/a.csv", "out/combined.rds"),
               use_log = TRUE)
  ans_obtained <- is_file_arg(args)
  ans_expected <- c(TRUE, FALSE)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'is_file_arg' does not treat an empty character vector as a file", {
  args <- list(files = character(),
               label = "notes.csv")
  ans_obtained <- is_file_arg(args)
  ans_expected <- c(FALSE, TRUE)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'is_file_arg' lets a dotted name override a path-like string", {
  args <- list(.files = c("data/a.csv", "out/combined.rds"),
               label = "notes.csv")
  ans_obtained <- is_file_arg(args)
  ans_expected <- c(TRUE, FALSE)
  expect_identical(ans_obtained, ans_expected)
})


## 'file_arg_io' --------------------------------------------------------------

test_that("'file_arg_io' has no output when nothing is a file", {
  ans <- file_arg_io(list(n = 1, label = "notes"))
  expect_identical(ans, list(inputs = character(), output = NULL))
})


## 'is_varname_valid' ---------------------------------------------------------

test_that("'is_varname_valid' returns TRUE with valid names", {
  expect_true(is_varname_valid("x"))
  expect_true(is_varname_valid("....x"))
  expect_true(is_varname_valid("X_X_..3"))
})

test_that("'is_varname_valid' returns FALSE with invalid names", {
  expect_false(is_varname_valid("_x"))
  expect_false(is_varname_valid(" "))
  expect_false(is_varname_valid("NA"))
  expect_false(is_varname_valid("if"))
  expect_false(is_varname_valid("x x"))
  expect_false(is_varname_valid("?X_X_..3"))
})


## 'make_commands' ------------------------------------------------------------

test_that("'make_commands' works with valid input - path_files in same directory", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  f1 <- file.path(dir_tmp, "script.R")
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = f1)
  f2 <- file.path(dir_tmp, "data.R")
  writeLines("1",
             con = f2)
  ans_obtained <- make_commands(path_files = ".",
                                dir_shell = dir_tmp,
                                quiet = TRUE)
  ans_expected <- paste0("Rscript script.R \\\n",
                         "  data/mydata.csv \\\n",
                         "  out/cleaned.rds \\\n",
                         "  --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'make_commands' works with valid input - path_files in lower directory", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "src"))
  f1 <- file.path(dir_tmp, "src/script.R")
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = f1)
  f2 <- file.path(dir_tmp, "src/data.R")
  writeLines("1",
             con = f2)
  ans_obtained <- make_commands(path_files = "src",
                                dir_shell = dir_tmp,
                                quiet = TRUE)
  ans_expected <- paste0("Rscript src/script.R \\\n",
                         "  data/mydata.csv \\\n",
                         "  out/cleaned.rds \\\n",
                         "  --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'make_commands' runs a script after the script that creates its input", {
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "src"))
  writeLines("cmd_assign_collect(.files = c('data/a.csv', 'data/b.csv', 'out/combined.rds'))",
             con = file.path(dir_tmp, "src/z_combine.R"))
  writeLines("cmd_assign(.combined = 'out/combined.rds', .out = 'out/next.rds')",
             con = file.path(dir_tmp, "src/a_next.R"))
  ans_obtained <- make_commands(path_files = "src",
                                dir_shell = dir_tmp,
                                quiet = TRUE)
  ans_expected <- c(paste0("Rscript src/z_combine.R \\\n",
                           "  data/a.csv \\\n",
                           "  data/b.csv \\\n",
                           "  out/combined.rds\n"),
                    paste0("Rscript src/a_next.R \\\n",
                           "  out/combined.rds \\\n",
                           "  out/next.rds\n"))
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'make_commands' rejects two scripts that write the same file", {
  dir_tmp <- tempfile(tmpdir = getwd())
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "src"))
  writeLines("cmd_assign(.out = 'out/same.rds')",
             con = file.path(dir_tmp, "src/a.R"))
  writeLines("cmd_assign(.out = 'out/same.rds')",
             con = file.path(dir_tmp, "src/b.R"))
  expect_error(make_commands(path_files = "src",
                             dir_shell = dir_tmp,
                             quiet = TRUE),
               "write")
})

test_that("'make_commands' rejects scripts that depend on each other", {
  dir_tmp <- tempfile(tmpdir = getwd())
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "src"))
  writeLines("cmd_assign(.in = 'out/b.rds', .out = 'out/a.rds')",
             con = file.path(dir_tmp, "src/a.R"))
  writeLines("cmd_assign(.in = 'out/a.rds', .out = 'out/b.rds')",
             con = file.path(dir_tmp, "src/b.R"))
  expect_error(make_commands(path_files = "src",
                             dir_shell = dir_tmp,
                             quiet = TRUE),
               "depend on each other")
})


## 'order_commands' -----------------------------------------------------------

test_that("'order_commands' keeps independent scripts in path order", {
  steps <- list(list(file = "src/b.R",
                     inputs = "data/b.csv",
                     output = "out/b.rds",
                     command = "b"),
                list(file = "src/a.R",
                     inputs = "data/a.csv",
                     output = "out/a.rds",
                     command = "a"))
  ans <- order_commands(steps)
  expect_identical(vapply(ans, function(step) step$command, ""),
                   c("a", "b"))
})

test_that("'order_commands' waits for every input", {
  steps <- list(list(file = "src/a_model.R",
                     inputs = "out/clean.rds",
                     output = "out/model.rds",
                     command = "model"),
                list(file = "src/m_vals.R",
                     inputs = c("out/clean.rds", "out/model.rds"),
                     output = "out/vals.rds",
                     command = "vals"),
                list(file = "src/z_clean.R",
                     inputs = "data/raw.csv",
                     output = "out/clean.rds",
                     command = "clean"))
  ans <- order_commands(steps)
  expect_identical(vapply(ans, function(step) step$command, ""),
                   c("clean", "model", "vals"))
})

test_that("'order_commands' returns an empty list unchanged", {
  expect_identical(order_commands(list()), list())
})

test_that("'order_commands' accepts a script with no inputs", {
  steps <- list(list(file = "src/a.R",
                     inputs = character(),
                     output = "out/a.rds",
                     command = "a"))
  expect_identical(order_commands(steps), steps)
})

test_that("'order_commands' rejects a cycle that also reads an earlier script", {
  steps <- list(list(file = "src/a.R",
                     inputs = character(),
                     output = "out/a.rds",
                     command = "a"),
                list(file = "src/b.R",
                     inputs = c("out/a.rds", "out/c.rds"),
                     output = "out/b.rds",
                     command = "b"),
                list(file = "src/c.R",
                     inputs = "out/b.rds",
                     output = "out/c.rds",
                     command = "c"))
  expect_error(order_commands(steps),
               "depend on each other")
})

test_that("'find_command_cycle' reports a cycle after a script with no predecessors", {
  ans <- find_command_cycle(nodes = c(1L, 2L, 3L),
                            preds = list(integer(), 3L, 2L))
  expect_identical(ans, c(2L, 3L, 2L))
})

test_that("'order_commands' rejects a script that reads its own output", {
  steps <- list(list(file = "src/a.R",
                     inputs = "out/a.rds",
                     output = "out/a.rds",
                     command = "a"))
  expect_error(order_commands(steps),
               "src/a.R")
})


## 'make_rules' ---------------------------------------------------------------

test_that("'make_rules' works with valid input - path_files in same directory", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  f1 <- file.path(dir_tmp, "script.R")
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = f1)
  f2 <- file.path(dir_tmp, "data.R")
  writeLines("1",
             con = f2)
  ans_obtained <- make_rules(path_files = ".",
                             dir_make = dir_tmp,
                             quiet = TRUE)
  ans_expected <- paste0("out/cleaned.rds: ", "script.R", " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'make_rules' works with valid input - path_files in lower directory", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  dir.create(file.path(dir_tmp, "src"))
  f1 <- file.path(dir_tmp, "src/script.R")
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = f1)
  f2 <- file.path(dir_tmp, "src/data.R")
  writeLines("1",
             con = f2)
  ans_obtained <- make_rules(path_files = "src",
                             dir_make = dir_tmp,
                             quiet = TRUE)
  ans_expected <- paste0("out/cleaned.rds: ", "src/script.R", " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})


