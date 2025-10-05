

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

test_that("'check_args_dots' throws error when values are invalid", {
    expect_error(check_args_dots(args_dots = list(a = raw(), b = 2)),
                 "Can't process argument `a` in call to `cmd_assign\\(\\)`.")
})

test_that("'check_args_dots' throws error when values have wrong length", {
    expect_error(check_args_dots(args_dots = list(a = "x", b = 2:1)),
                 "Argument `b` in call to `cmd_assign\\(\\)` has length 2.")
    expect_error(check_args_dots(args_dots = list(a = "x", b = integer())),
                 "Argument `b` in call to `cmd_assign\\(\\)` has length 0.")
})


## 'check_dir' ----------------------------------------------------------------

test_that("'check_dir' works with valid inputs", {
  dir_tmp <- tempfile()
  if (!file.exists(dir_tmp))
    dir.create(dir_tmp)
  expect_true(check_dir(dir_tmp, nm = "x"))
})

test_that("'check_dir' throws error when dir length != 1", {
  expect_error(check_dir(c("a", "b"), nm = "x"),
               "`x` does not have length 1.")
  expect_error(check_dir(character(), nm = "x"),
               "`x` does not have length 1.")
})

test_that("'check_dir' throws error when dir does not exist", {
  if (!file.exists("wrong/wrong_again")) {
    expect_error(check_dir("wrong/wrong_again", nm = "x"),
                 "Problem with `x`.")
  }
})


## 'check_path_file_valid' ----------------------------------------------------

test_that("'check_path_file_valid' returns true with valid inputs, with dir specified", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  writeLines("x <- 1",
             con = fs::path(dir_tmp, "script.R"))
  expect_true(check_path_file_valid(path_file = "script.R",
                                    dir = dir_tmp,
                                    nm_dir_arg = "d",
                                    has_dir_arg = TRUE))
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_path_file_valid' returns true with valid inputs, with dir not specified", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  writeLines("x <- 1",
             con = fs::path(dir_tmp, "script.R"))
  expect_true(check_path_file_valid(path_file = fs::path(fs::path_rel(dir_tmp, getwd()), "script.R"),
                                    dir = getwd(),
                                    nm_dir_arg = "d",
                                    has_dir_arg = FALSE))
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_path_files_valid' throws correct error, with absolute path", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  expect_error(check_path_file_valid(path_file = "C:/",
                                     dir = dir_tmp,
                                     nm_dir_arg = "dir_make",
                                     has_dir_arg = TRUE),
               "`path_file` is an absolute path.")
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_path_file_valid' throws correct error, with dir specified", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  expect_error(check_path_file_valid(path_file = "wrong",
                                     dir = dir_tmp,
                                     nm_dir = "d",
                                     has_dir_arg = TRUE),
               "Can't find R script.")
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_path_file_valid' throws correct error, with dir not specified", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  expect_error(check_path_file_valid(path_file = "wrong",
                                     dir = getwd(),
                                     nm_dir = "d",
                                     has_dir_arg = FALSE),
               "Can't find R script.")
  unlink(dir_tmp, recursive = TRUE)
})


## 'check_path_files_valid' ---------------------------------------------------

test_that("'check_path_files_valid' returns true with valid inputs, with dir specified", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  dir.create(fs::path(dir_tmp, "src"))
  expect_true(check_path_files_valid(path_files = "src",
                                 dir = dir_tmp,
                                 nm_dir_arg = "dir_make",
                                 has_dir_arg = TRUE))
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_path_files_valid' returns true with valid inputs, with dir not specified", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  dir.create(fs::path(dir_tmp, "src"))
  expect_true(check_path_files_valid(path_files = fs::path(fs::path_rel(dir_tmp, getwd()), "src"),
                                 dir = getwd(),
                                 nm_dir_arg = "dir_make",
                                 has_dir_arg = FALSE))
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_path_files_valid' throws correct error, with absolute path", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  expect_error(check_path_files_valid(path_files = "C:/",
                                      dir = dir_tmp,
                                      nm_dir_arg = "dir_make",
                                      has_dir_arg = TRUE),
               "`path_files` is an absolute path.")
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_path_files_valid' throws correct error, with dir specified", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  expect_error(check_path_files_valid(path_files = "wrong",
                                  dir = dir_tmp,
                                  nm_dir_arg = "dir_make",
                                  has_dir_arg = TRUE),
               "Can't find directory containing R scripts.")
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_path_files_valid' throws correct error, with dir not specified", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  expect_error(check_path_files_valid(path_files = "wrong",
                                 dir = getwd(),
                                 nm_dir_arg = "dir_make",
                                 has_dir_arg = FALSE),
               "Can't find directory.")
  unlink(dir_tmp, recursive = TRUE)
})


## 'check_flag' ---------------------------------------------------------------

test_that("'check_flag' returns TRUE with valid inputs", {
  x <- TRUE
  expect_true(check_flag(x, nm = "x"))
  x <- FALSE
  expect_true(check_flag(x))
})

test_that("'check_flag' throws expected error non-length-1", {
  y <- logical()
  expect_error(check_flag(y, nm = "z"),
               "`z` does not have length 1")
  z <- c(TRUE, TRUE)
  expect_error(check_flag(z, nm = "z"),
               "`z` does not have length 1")
})

test_that("'check_flag' throws expected error non-logical", {
  x <- "hello"
  expect_error(check_flag(x, nm = "x"),
               "`x` does not have class <logical>")
})

test_that("'check_flag' throws expected error NA", {
  x <- NA
  expect_error(check_flag(x, nm = "xx"),
               "`xx` is NA")
})


## 'check_is_r_code' -------------------------------------------------------------

test_that("'check_is_r_code' works with valid inputs", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines("x <- 1",
             con = "script.R")
  ans <- check_is_r_code("script.R")
  expect_true(ans)
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_is_r_code' throws error when file does not exist", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  expect_error(check_is_r_code("wrong.R"),
                 "File 'wrong\\.R' does not exist.")
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_is_r_code' gives warning with no R extension", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines("x <- 1",
             con = "script.c")
  expect_message(check_is_r_code("script.c"),
                 "File 'script\\.c' does not have extension \"R\" or \"r\"\\.")
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'check_is_r_code' gives error when file invalid", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  dir.create("dd")
  writeLines("wrong wrong wrong",
             con = "dd/script.R")
  expect_error(check_is_r_code("dd/script.R"),
               "Can't parse file 'dd/script\\.R'.")
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
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


## 'check_valid_filename' -----------------------------------------------------

test_that("check_valid_filename works with valid inputs", {
  expect_invisible(result <- check_valid_filename("file.csv", nm = "x"))
  expect_true(result)
  expect_invisible(result <- check_valid_filename("valid_file-name.txt", nm = "x"))
  expect_true(result)
})

test_that("check_valid_filename fails with non-character input", {
  expect_error(check_valid_filename(42, nm = "x"), "`x` is not a character string")
  expect_error(check_valid_filename(TRUE, nm = "x"), "`x` is not a character string")
})

test_that("check_valid_filename failes with length != 1", {
  expect_error(check_valid_filename(character(0), nm = "x"), "`x` does not have length 1")
  expect_error(check_valid_filename(c("file1", "file2"), nm = "x"), "`x` does not have length 1")
})

test_that("check_valid_filename fails with blank", {
  expect_error(check_valid_filename("", nm = "x"), "`x` is blank")
})

test_that("check_valid_filename fails with invalid character", {
  expect_error(check_valid_filename("bad/name.txt", nm = "x"),
               "`x` contains invalid character.")
  expect_error(check_valid_filename("bad\\name.txt", nm = "x"),
               "`x` contains invalid character.")
  expect_error(check_valid_filename("bad:name.txt", nm = "x"),
               "`x` contains invalid character.")
  expect_error(check_valid_filename("bad*name.txt", nm = "x"),
               "`x` contains invalid character.")
  expect_error(check_valid_filename("bad?name.txt", nm = "x"),
               "`x` contains invalid character.")
  expect_error(check_valid_filename("bad\"name.txt", nm = "x"),
               "`x` contains invalid character.")
  expect_error(check_valid_filename("bad<name.txt", nm = "x"),
               "`x` contains invalid character.")
  expect_error(check_valid_filename("bad>name.txt", nm = "x"),
               "`x` contains invalid character.")
  expect_error(check_valid_filename("bad|name.txt", nm = "x"),
               "`x` contains invalid character.")
})
