
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


## 'extract_args' -------------------------------------------------------------

test_that("'extract_args' works with valid inputs", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
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
  dir_tmp <- tempfile()
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


## 'is_actual_or_potential_file_path' -----------------------------------------

test_that("'is_actual_or_potential_file_path' returns FALSE if is non-character or NA", {
  expect_false(is_actual_or_potential_file_path(1))
  expect_false(is_actual_or_potential_file_path(FALSE))
  expect_false(is_actual_or_potential_file_path(NA_character_))
})

test_that("'is_actual_or_potential_file_path' returns TRUE if file exists", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
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
  dir_tmp <- tempfile()
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




