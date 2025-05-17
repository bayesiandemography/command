
## 'cmd_shell' ----------------------------------------------------------------

test_that("'cmd_shell' works with valid inputs - dir supplied", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "script.R"
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = fs::path(dir_tmp, file))
  capture.output(ans_obtained <- cmd_shell(file, dir_shell = dir_tmp))
  ans_expected <- paste0("Rscript ", file, " \\\n",
                         "  data/mydata.csv \\\n",
                         "  out/cleaned.rds \\\n",
                         "  --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'cmd_shell' works with valid inputs - dir not supplied", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- fs::path(fs::path_rel(dir_tmp, dir_curr), "script.R")
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = fs::path(dir_tmp, "script.R"))
  capture.output(ans_obtained <- cmd_shell(file))
  ans_expected <- paste0("Rscript ", file, " \\\n",
                         "  data/mydata.csv \\\n",
                         "  out/cleaned.rds \\\n",
                         "  --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})




