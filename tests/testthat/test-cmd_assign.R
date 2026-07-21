
## 'cmd_assign' ---------------------------------------------------------------

test_that("'cmd_assign' works with valid arguments - on command line", {
    dir_curr <- getwd()
    dir_tmp <- tempfile(tmpdir = getwd())
    if (file.exists(dir_tmp))
        unlink(dir_tmp, recursive = TRUE)
    dir.create(dir_tmp)
    setwd(dir_tmp)
    writeLines(c("command::cmd_assign(p_file1 = 'myfile.xsls', obj = 'obj.rds', n = 2L, named = 'goodbye')",
                 "saveRDS(p_file1, file = 'p_file1.rds')",
                 "saveRDS(obj, file = 'obj.rds')",
                 "saveRDS(n, file = 'n.rds')",
                 "saveRDS(named, file = 'named.rds')",
                 "saveRDS(ls(), file = 'ls.rds')"),
               con = "script.R")
    cmd <- sprintf("%s/bin/Rscript script.R thatfile.xlsx obj.rds -n=1 --named=hello",
                   R.home())
    system(cmd, ignore.stderr = TRUE)
    p_file1 <- readRDS("p_file1.rds")
    expect_identical(p_file1, "thatfile.xlsx")
    obj <- readRDS("obj.rds")
    expect_identical(obj, "obj.rds")
    n <- readRDS("n.rds")
    expect_identical(n, 1L)
    named <- readRDS("named.rds")
    expect_identical(named, "hello")
    ls <- readRDS("ls.rds")
    expect_setequal(ls, c("p_file1", "obj", "n", "named"))
    setwd(dir_curr)
    unlink(dir_tmp, recursive = TRUE)
})

test_that("'cmd_assign' works with valid arguments - interactively", {
  if (interactive()) {
    suppressMessages(
      cmd_assign(obj = "obj.rds", n = 2, named = 'goodbye')
    )
    expect_identical(obj, "obj.rds")
    expect_identical(n, 2)
    expect_identical(named, "goodbye")
    expect_setequal(ls(), c("obj", "n", "named"))
  }
  else
    expect_true(TRUE)
})

test_that("'cmd_assign' works with on command line with options passed before file argument", {
    dir_curr <- getwd()
    dir_tmp <- tempfile(tmpdir = getwd())
    if (file.exists(dir_tmp))
        unlink(dir_tmp, recursive = TRUE)
    dir.create(dir_tmp)
    setwd(dir_tmp)
    writeLines(c("command::cmd_assign(p_file1 = 'myfile.xsls', obj = 'obj.rds', n = 2L, named = 'goodbye')",
                 "saveRDS(p_file1, file = 'p_file1.rds')",
                 "saveRDS(obj, file = 'obj.rds')",
                 "saveRDS(n, file = 'n.rds')",
                 "saveRDS(named, file = 'named.rds')",
                 "saveRDS(ls(), file = 'ls.rds')"),
               con = "script.R")
    cmd <- sprintf(paste("%s/bin/Rscript --default-packages=methods,datasets --verbose",
                         "script.R thatfile.xlsx obj.rds -n=1 --named=hello"),
                   R.home())
    system(cmd, ignore.stderr = TRUE)
    p_file1 <- readRDS("p_file1.rds")
    expect_identical(p_file1, "thatfile.xlsx")
    obj <- readRDS("obj.rds")
    expect_identical(obj, "obj.rds")
    n <- readRDS("n.rds")
    expect_identical(n, 1L)
    named <- readRDS("named.rds")
    expect_identical(named, "hello")
    ls <- readRDS("ls.rds")
    expect_setequal(ls, c("p_file1", "obj", "n", "named"))
    setwd(dir_curr)
    unlink(dir_tmp, recursive = TRUE)
})

test_that("'cmd_assign' works with valid arguments - on command line, with littler (if available)", {
  skip_if_no_littler_available()
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("command::cmd_assign(p_file1 = 'myfile.xsls', obj = 'obj.rds', n = 2L, named = 'goodbye')",
               "saveRDS(p_file1, file = 'p_file1.rds')",
               "saveRDS(obj, file = 'obj.rds')",
               "saveRDS(n, file = 'n.rds')",
               "saveRDS(named, file = 'named.rds')",
               "saveRDS(ls(), file = 'ls.rds')"),
             con = "script.R")
  cmd <- "lr script.R thatfile.xlsx obj.rds -n=1 --named=hello"
  system(cmd, ignore.stderr = TRUE)
  p_file1 <- readRDS("p_file1.rds")
  expect_identical(p_file1, "thatfile.xlsx")
  obj <- readRDS("obj.rds")
  expect_identical(obj, "obj.rds")
  n <- readRDS("n.rds")
  expect_identical(n, 1L)
  named <- readRDS("named.rds")
  expect_identical(named, "hello")
  ls <- readRDS("ls.rds")
  expect_setequal(ls, c("p_file1", "obj", "n", "named", "argv"))
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})



## 'cmd_assign_quiet' ---------------------------------------------------------

test_that("'cmd_assign_quiet' works with valid arguments - on command line", {
    dir_curr <- getwd()
    dir_tmp <- tempfile(tmpdir = getwd())
    if (file.exists(dir_tmp))
        unlink(dir_tmp, recursive = TRUE)
    dir.create(dir_tmp)
    setwd(dir_tmp)
    writeLines(c("command::cmd_assign_quiet(p_file1 = 'myfile.xsls', obj = 'obj.rds', n = 2L, named = 'goodbye')",
                 "saveRDS(p_file1, file = 'p_file1.rds')",
                 "saveRDS(obj, file = 'obj.rds')",
                 "saveRDS(n, file = 'n.rds')",
                 "saveRDS(named, file = 'named.rds')",
                 "saveRDS(ls(), file = 'ls.rds')"),
               con = "script.R")
    cmd <- sprintf("%s/bin/Rscript script.R thatfile.xlsx obj.rds -n=1 --named=hello",
                   R.home())
    system(cmd, ignore.stderr = TRUE)
    p_file1 <- readRDS("p_file1.rds")
    expect_identical(p_file1, "thatfile.xlsx")
    obj <- readRDS("obj.rds")
    expect_identical(obj, "obj.rds")
    n <- readRDS("n.rds")
    expect_identical(n, 1L)
    named <- readRDS("named.rds")
    expect_identical(named, "hello")
    ls <- readRDS("ls.rds")
    expect_setequal(ls, c("p_file1", "obj", "n", "named"))
    setwd(dir_curr)
    unlink(dir_tmp, recursive = TRUE)
})

test_that("'cmd_assign_quiet' works with valid arguments - interactively", {
  if (interactive()) {
    cmd_assign_quiet(obj = "obj.rds", n = 2, named = 'goodbye')
    expect_identical(obj, "obj.rds")
    expect_identical(n, 2)
    expect_identical(named, "goodbye")
    expect_setequal(ls(), c("obj", "n", "named"))
  }
  else
    expect_true(TRUE)
})

test_that("'cmd_assign_quiet' works with on command line with options passed before file argument", {
    dir_curr <- getwd()
    dir_tmp <- tempfile(tmpdir = getwd())
    if (file.exists(dir_tmp))
        unlink(dir_tmp, recursive = TRUE)
    dir.create(dir_tmp)
    setwd(dir_tmp)
    writeLines(c("command::cmd_assign_quiet(p_file1 = 'myfile.xsls', obj = 'obj.rds', n = 2L, named = 'goodbye')",
                 "saveRDS(p_file1, file = 'p_file1.rds')",
                 "saveRDS(obj, file = 'obj.rds')",
                 "saveRDS(n, file = 'n.rds')",
                 "saveRDS(named, file = 'named.rds')",
                 "saveRDS(ls(), file = 'ls.rds')"),
               con = "script.R")
    cmd <- sprintf(paste("%s/bin/Rscript --default-packages=methods,datasets --verbose",
                         "script.R thatfile.xlsx obj.rds -n=1 --named=hello"),
                   R.home())
    system(cmd, ignore.stderr = TRUE)
    p_file1 <- readRDS("p_file1.rds")
    expect_identical(p_file1, "thatfile.xlsx")
    obj <- readRDS("obj.rds")
    expect_identical(obj, "obj.rds")
    n <- readRDS("n.rds")
    expect_identical(n, 1L)
    named <- readRDS("named.rds")
    expect_identical(named, "hello")
    ls <- readRDS("ls.rds")
    expect_setequal(ls, c("p_file1", "obj", "n", "named"))
    setwd(dir_curr)
    unlink(dir_tmp, recursive = TRUE)
})

test_that("'cmd_assign_quiet' works with valid arguments - on command line, with littler (if available)", {
  skip_if_no_littler_available()
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("command::cmd_assign_quiet(p_file1 = 'myfile.xsls', obj = 'obj.rds', n = 2L, named = 'goodbye')",
               "saveRDS(p_file1, file = 'p_file1.rds')",
               "saveRDS(obj, file = 'obj.rds')",
               "saveRDS(n, file = 'n.rds')",
               "saveRDS(named, file = 'named.rds')",
               "saveRDS(ls(), file = 'ls.rds')"),
             con = "script.R")
  cmd <- "lr script.R thatfile.xlsx obj.rds -n=1 --named=hello"
  system(cmd, ignore.stderr = TRUE)
  p_file1 <- readRDS("p_file1.rds")
  expect_identical(p_file1, "thatfile.xlsx")
  obj <- readRDS("obj.rds")
  expect_identical(obj, "obj.rds")
  n <- readRDS("n.rds")
  expect_identical(n, 1L)
  named <- readRDS("named.rds")
  expect_identical(named, "hello")
  ls <- readRDS("ls.rds")
  expect_setequal(ls, c("p_file1", "obj", "n", "named", "argv"))
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})


## Empty named value from Make: --v=$(V) with V undefined → --v=
## The empty value still counts as an argument, so arity checks pass.
test_that("'cmd_assign' with --v= (empty Make variable) and character default", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("command::cmd_assign(.data = 'in.csv', v = 'default', .out = 'out.rds')",
               "saveRDS(list(.data = .data, v = v, .out = .out), file = 'result.rds')"),
             con = "script.R")
  cmd <- sprintf("%s/bin/Rscript script.R in.csv out.rds --v=", R.home())
  status <- system(cmd, ignore.stderr = TRUE)
  expect_identical(status, 0L)
  result <- readRDS("result.rds")
  ## Currently: empty string is accepted for character v.
  expect_identical(result$v, "")
  expect_identical(result$.data, "in.csv")
  expect_identical(result$.out, "out.rds")
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'cmd_assign' with --v= (empty Make variable) and integer default", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("command::cmd_assign(.data = 'in.csv', v = 1L, .out = 'out.rds')",
               "saveRDS(list(.data = .data, v = v, .out = .out), file = 'result.rds')"),
             con = "script.R")
  cmd <- sprintf("%s/bin/Rscript script.R in.csv out.rds --v=", R.home())
  status <- system(cmd, ignore.stderr = TRUE)
  expect_false(identical(status, 0L))
  expect_false(file.exists("result.rds"))
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})

test_that("'cmd_assign' with --v= (empty Make variable) and logical default", {
  dir_curr <- getwd()
  dir_tmp <- tempfile(tmpdir = getwd())
  if (file.exists(dir_tmp))
    unlink(dir_tmp, recursive = TRUE)
  dir.create(dir_tmp)
  setwd(dir_tmp)
  writeLines(c("command::cmd_assign(.data = 'in.csv', v = TRUE, .out = 'out.rds')",
               "saveRDS(list(.data = .data, v = v, .out = .out), file = 'result.rds')"),
             con = "script.R")
  cmd <- sprintf("%s/bin/Rscript script.R in.csv out.rds --v=", R.home())
  status <- system(cmd, ignore.stderr = TRUE)
  expect_false(identical(status, 0L))
  expect_false(file.exists("result.rds"))
  setwd(dir_curr)
  unlink(dir_tmp, recursive = TRUE)
})
