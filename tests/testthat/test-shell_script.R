
## 'shell_script' -----------------------------------------------------------------

test_that("'shell_script' works with has dir_shell, no name_shell", {
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  dir.create(fs::path(dir_tmp, "src"))
  file_path <- fs::path(path = file.path(dir_tmp, "src/script.R"))
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = file_path)
  suppressMessages(ans_obtained <- shell_script(files = "src", dir_shell = dir_tmp))
  ans_expected <- paste0("\n",
                         "Rscript src/script.R \\\n",
                         "  data/mydata.csv \\\n",
                         "  out/cleaned.rds \\\n",
                         "  --use_log=FALSE\n\n")
  expect_identical(ans_obtained, ans_expected)
  expect_true(file.exists(fs::path(dir_tmp, "workflow.sh")))
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'shell_script' works with no dir_shell, has name_shell", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  dir.create("src")
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = "src/script.R")
  suppressMessages(ans_obtained <- shell_script(files = "src", name_shell = "shell"))
  ans_expected <- paste0("\n",
                         "Rscript src/script.R \\\n",
                         "  data/mydata.csv \\\n",
                         "  out/cleaned.rds \\\n",
                         "  --use_log=FALSE\n\n")
  expect_identical(ans_obtained, ans_expected)
  expect_true(file.exists("shell"))
  unlink(dir_tmp, recursive = TRUE)
  setwd(dir_curr)
})

test_that("'shell_script' throws error with no files", {
  expect_error(shell_script(),
               "argument \"files\" is missing, with no default")
})

test_that("'shell_script' throws appropriate error when file already exists", {
  dir_curr <- getwd()
  dir_tmp <- tempfile()
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  dir.create("src")
  writeLines("cmd_assign(.data = 'data/mydata.csv', use_log = FALSE, .out = 'out/cleaned.rds')",
             con = "src/script.R")
  writeLines("bla",
             con = "shell.sh")
  expect_error(suppressMessages(shell_script(files = "src", name_shell = "shell.sh")),
               "already contains a")
  suppressMessages(shell_script(files = "src", name_shell = "shell.sh", overwrite = TRUE))
  ans <- readLines("shell.sh")
  expect_identical(ans[[2]], "Rscript src/script.R \\")
  unlink(dir_tmp, recursive = TRUE)
  setwd(dir_curr)
})







