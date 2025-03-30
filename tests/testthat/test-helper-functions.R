

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
    

## 'check_args_cmd' ----------------------------------------------------------

test_that("'check_args_cmd' returns TRUE when 'args_cmd' valid", {
  args_cmd <- list("file1", "file2", n_iter = 3)
  args_dots  <- list(f1 = "f1", f2 = "f2", n_iter = 2)
  expect_true(check_args_cmd(args_cmd = args_cmd,
                             args_dots = args_dots))
  expect_true(check_args_cmd(args_cmd = args_cmd,
                             args_dots = rev(args_dots)))
})

test_that("'check_args_cmd' returns TRUE when 'args_cmd' empty", {
  args_cmd <- list()
  args_dots  <- list()
  expect_true(check_args_cmd(args_cmd = args_cmd,
                             args_dots = args_dots))
})

test_that("'check_args_cmd' throws expected error when named passed at command line not found in dots", {
  ## two dots arg
  args_cmd <- list("file1", wrong = 3)
  args_dots  <- list(f1 = "f1", n_iter = 10)
  expect_error(check_args_cmd(args_cmd = args_cmd,
                              args_dots = args_dots),
               "Problem with argument `wrong`.")
  ## one dots arg
  args_cmd <- list("file1", wrong = 3)
  args_dots  <- list(f1 = "f1")
  expect_error(check_args_cmd(args_cmd = args_cmd,
                              args_dots = args_dots),
               "Problem with argument `wrong`.")
})

test_that("'check_args_cmd' throws expected error lengths of 'args_cmd' 'args_dots' differ", {
  args_cmd <- list("file1", "file2", n_iter = 3)
  args_dots  <- list(f1 = "f1", n_iter = 10)
  expect_error(check_args_cmd(args_cmd = args_cmd,
                              args_dots = args_dots),
               paste("Mismatch between arguments passed at command line and arguments",
                     "specified with `cmd_assign\\(\\)`."))
})

test_that("'check_args_cmd' throws expected error lengths of 'args_cmd' 'args_dots' differ - no named args", {
  args_cmd <- list("file1", "file2")
  args_dots  <- list(f1 = "f1")
  expect_error(check_args_cmd(args_cmd = args_cmd,
                              args_dots = args_dots),
               paste("Mismatch between arguments passed at command line and arguments",
                     "specified with `cmd_assign\\(\\)`."))
})



## 'check_args_dots' ----------------------------------------------------------

test_that("'check_args_dots' returns TRUE when 'args_dots' valid", {
  expect_true(check_args_dots(args_dots = list(a = "1",
                                               b = 2L,
                                               c = 3.0,
                                               d = as.Date("2001-01-01"),
                                               e = as.POSIXct("2001-01-01 11:11:11"),
                                               f = as.POSIXlt("2001-01-01 11:11:11"),
                                               g = NULL)))
})

test_that("'check_args_dots' throws errors when values are invalid", {
    expect_error(check_args_dots(args_dots = list(a = raw(), b = 2)),
                 "Can't process argument `a` in call to `cmd_assign\\(\\)`.")
})


## 'check_names_args' ---------------------------------------------------------

test_that("'check_names_args' returns TRUE when 'nms' valid", {
    expect_true(check_names_args(nms = c("a", "b")))
})

test_that("'check_names_args' throws error when nms NULL", {
  expect_error(check_names_args(nms = NULL),
               "Arguments do not have names.")
})

test_that("'check_names_args' throws error when nms has NA", {
  expect_error(check_names_args(nms = c("a", NA, "b", NA)),
               "Argument names include NA.")
})

test_that("'check_names_args' throws error when nms has blank", {
  expect_error(check_names_args(nms = c("a", "", "b", "bb")),
               "Argument name with length 0.")
})

test_that("'check_names_args' throws error when nms has duplicate", {
  expect_error(check_names_args(nms = c("a", "bb", "b", "bb")),
               "More than one argument named \"bb\".")
})

test_that("'check_names_args' throws error when nms has invalid name", {
  expect_error(check_names_args(nms = c("a", "1", "b", "bb")),
               "Argument name not a valid name for an R object.")
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


## 'get_args_cmd' -------------------------------------------------------------

test_that("'get_args_cmd' works with valid inputs", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
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
  dir_tmp <- tempfile()
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





