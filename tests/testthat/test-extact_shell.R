
## 'extract_shell' ----------------------------------------------------------------

test_that("'extract_shell' works with valid inputs - dir supplied", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  path_file <- "script.R"
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = fs::path(dir_tmp, path_file))
  capture.output(ans_obtained <- extract_shell(path_file, dir_shell = dir_tmp))
  path_file_comb <- fs::path(dir_tmp, path_file)
  ans_expected <- paste0("Rscript ", path_file_comb, " \\\n",
                         "  data/mydata.csv \\\n",
                         "  out/cleaned.rds \\\n",
                         "  --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_shell' works with valid inputs - dir not supplied", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  path_file <- fs::path(fs::path_rel(dir_tmp, dir_curr), "script.R")
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = fs::path(dir_tmp, "script.R"))
  capture.output(ans_obtained <- extract_shell(path_file))
  path_file_comb <- fs::path(getwd(), path_file)
  ans_expected <- paste0("Rscript ", path_file_comb, " \\\n",
                         "  data/mydata.csv \\\n",
                         "  out/cleaned.rds \\\n",
                         "  --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})




