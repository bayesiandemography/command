
## 'cmd_make' -----------------------------------------------------------------

test_that("'cmd_make' works with no dir_make", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- fs::path_rel(path = file.path(dir_tmp, "script.R"),
                       start = getwd())
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = file)
  capture.output(ans_obtained <- cmd_make(file))
  ans_expected <- paste0("out/cleaned.rds: ", file, " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'cmd_make' works with dir_make", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  file <- "script.R"
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = fs::path(dir_tmp, file))
  capture.output(ans_obtained <- cmd_make(file = file, dir_make = dir_tmp))
  ans_expected <- paste0("out/cleaned.rds: ", file, " \\\n",
                         "  data/mydata.csv\n",
                         "\t", "Rscript $^ $@ --use_log=FALSE\n")
  expect_identical(ans_obtained, ans_expected)
  unlink(dir_tmp, recursive = TRUE)
})



