
## 'makefile' -----------------------------------------------------------------

test_that("'makefile' works with has dir_make, no name_make", {
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  dir.create(fs::path(dir_tmp, "src"))
  file_path <- fs::path(path = file.path(dir_tmp, "src/script.R"))
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = file_path)
  suppressMessages(ans_obtained <- makefile(path_files = "src", dir_make = dir_tmp))
  ans_expected <- paste0("\n",
                         ".PHONY: all\n",
                         "all:\n",
                         "\n\n",
                         "out/cleaned.rds: ", "src/script.R", " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n",
                         "\n\n",
                         ".PHONY: clean\n",
                         "clean:\n",
                         "\trm -rf out\n",
                         "\tmkdir out\n\n")
  expect_identical(ans_obtained, ans_expected)
  expect_true(file.exists(fs::path(dir_tmp, "Makefile")))
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'makefile' works with no dir_make, has name_make", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  dir.create("src")
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = "src/script.R")
  suppressMessages(ans_obtained <- makefile(path_files = "src", name_make = "Make"))
  ans_expected <- paste0("\n",
                         ".PHONY: all\n",
                         "all:\n",
                         "\n\n",
                         "out/cleaned.rds: ", "src/script.R", " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n",
                         "\n\n",
                         ".PHONY: clean\n",
                         "clean:\n",
                         "\trm -rf out\n",
                         "\tmkdir out\n\n")
  expect_identical(ans_obtained, ans_expected)
  expect_true(file.exists("Make"))
  unlink(dir_tmp, recursive = TRUE)
  setwd(dir_curr)
})

test_that("'makefile' works with no dir_make, no path_files", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  dir.create("src")
  suppressMessages(ans_obtained <- makefile())
  ans_expected <- paste0("\n",
                         ".PHONY: all\n",
                         "all:\n\n\n",
                         ".PHONY: clean\n",
                         "clean:\n",
                         "\trm -rf out\n",
                         "\tmkdir out\n\n")
  expect_identical(ans_obtained, ans_expected)
  expect_true(file.exists(fs::path(dir_tmp, "Makefile")))
  unlink(dir_tmp, recursive = TRUE)
  setwd(dir_curr)
})

test_that("'makefile' throws appropriate error when file already exists", {
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  dir.create(fs::path(dir_tmp, "src"))
  file_path <- fs::path(dir_tmp, "src/script.R")
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = file_path)
  writeLines("bla",
             con = fs::path(dir_tmp, "Makefile"))
  expect_error(suppressMessages(makefile(path_files = "src", dir_make = dir_tmp)),
               "already contains a file called")
  suppressMessages(makefile(path_files = "src", dir_make = dir_tmp, overwrite = TRUE))
  ans <- readLines(fs::path(dir_tmp, "Makefile"))
  expect_identical(ans[[2]], ".PHONY: all")
  unlink(dir_tmp, recursive = TRUE)
})

