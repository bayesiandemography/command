
## 'extract_make' -----------------------------------------------------------------

test_that("'extract_make' works with no dir_make", {
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  path_file <- path_rel(path = file.path(dir_tmp, "script.R"),
                        start = getwd())
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = path_file)
  capture.output(ans_obtained <- extract_make(path_file = path_file))
  path_file_comb <- path_join(".", path_file)
  ans_expected <- paste0("out/cleaned.rds: ", path_file_comb, " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_make' reads a cmd_assign_collect call", {
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  path_file <- "script.R"
  writeLines(paste("cmd_assign_collect(files = c('data/a.csv', 'data/b.csv',",
                   "'out/combined.rds'), use_log = TRUE)"),
             con = path_join(dir_tmp, path_file))
  capture.output(ans_obtained <- extract_make(path_file = path_file,
                                          dir_make = dir_tmp))
  path_file_comb <- path_join(dir_tmp, path_file)
  ans_expected <- paste0("out/combined.rds: ", path_file_comb, " \\\n",
                         "  data/a.csv \\\n",
                         "  data/b.csv\n",
                         "\t", "Rscript $^ $@ --use_log=TRUE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'extract_make' works with dir_make", {
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  path_file <- "script.R"
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = path_join(dir_tmp, path_file))
  capture.output(ans_obtained <- extract_make(path_file = path_file,
                                          dir_make = dir_tmp))
  path_file_comb <- path_join(dir_tmp, path_file)
  ans_expected <- paste0("out/cleaned.rds: ", path_file_comb, " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})


