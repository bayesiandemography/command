
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


## 'assign_args' --------------------------------------------------------------

test_that("'assign_args' creates correct objects in the parent frame, and returns as list", {
    args <- list(xx = 1, yy = "hello", zz = TRUE)
    envir <- new.env()
    ans_obtained <- suppressMessages(
        assign_args(args = args, envir = envir)
    )
    expect_identical(envir$xx, 1)
    expect_identical(envir$yy, "hello")
    expect_identical(envir$zz, TRUE)
    expect_identical(ans_obtained, args)
})

test_that("'assign_args' works with empty args", {
    args <- list()
    envir <- new.env()
    ans_obtained <- suppressMessages(
        assign_args(args = args, envir = envir)
    )
    expect_identical(length(envir), 0L)
    expect_identical(ans_obtained, args)
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
  ans_obtained <- suppressMessages(extract_shell_if_possible(file = file, dir_shell = dir_tmp))
  ans_expected <- paste0("Rscript script.R \\\n",
                         "  data/mydata.csv \\\n",
                         "  out/cleaned.rds \\\n",
                         "  --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
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
  ans_obtained <- extract_shell_if_possible(file = file, dir_shell = dir_tmp)
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
  ans_obtained <- extract_shell_if_possible(file = file, dir_shell = dir_tmp)
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
  expect_error(extract_shell_if_possible(file = file, dir_shell = dir_tmp),
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
  ans_obtained <- suppressMessages(extract_make_if_possible(file = file, dir_make = dir_tmp))
  ans_expected <- paste0("out/cleaned.rds: ", file, " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
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
  ans_obtained <- extract_make_if_possible(file = file, dir_make = dir_tmp)
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
  ans_obtained <- extract_make_if_possible(file = file, dir_make = dir_tmp)
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
  expect_error(extract_make_if_possible(file = file, dir_make = dir_tmp),
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
                 "No call to `cmd_assign\\(\\)` found.")
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
  ans_obtained <- suppressMessages(make_rules(path_files = ".", dir_make = dir_tmp))
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
  ans_obtained <- suppressMessages(make_rules(path_files = "src", dir_make = dir_tmp))
  ans_expected <- paste0("out/cleaned.rds: ", "src/script.R", " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})


