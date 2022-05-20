
test_that("'target' works when R file run from command line and target passed", {
    project_dir <- tempdir()
    if (file.exists(project_dir))
        unlink(project_dir, recursive = TRUE)
    dir.create(project_dir)
    dir.create(file.path(project_dir, "src"))
    dir.create(file.path(project_dir, "out"))
    setwd(project_dir)
    writeLines(c("library(makr)",
                 "target <- target()",
                 "saveRDS(42, target)"),
               con = "src/results.R")
    cmd <- sprintf("%s/bin/Rscript src/results.R out/results.rds", R.home())
    system(cmd)
    expect_identical(readRDS("out/results.rds"), 42)
    unlink(project_dir, recursive = TRUE)
})

test_that("'target' works when R run interactively and default given", {
    expect_identical(target(default = "mytarget"),
                     "mytarget")
})

test_that("'target' throws appropriate error when more than one command line argument passed", {
    project_dir <- tempdir()
    if (file.exists(project_dir))
        unlink(project_dir, recursive = TRUE)
    dir.create(project_dir)
    dir.create(file.path(project_dir, "src"))
    dir.create(file.path(project_dir, "out"))
    setwd(project_dir)
    writeLines(c("library(makr)",
                 "library(testthat)",
                 "expect_error(target(), 'more than one command line argument passed')"),
               con = "src/results.R")
    cmd <- sprintf("%s/bin/Rscript src/results.R out/results1.rds out/results2.rds", R.home())
    val <- system(cmd)
    expect_equal(val, 0)
    unlink(project_dir, recursive = TRUE)
})

test_that("'target' throws appropriate error when no command line arguments passed", {
    project_dir <- tempdir()
    if (file.exists(project_dir))
        unlink(project_dir, recursive = TRUE)
    dir.create(project_dir)
    dir.create(file.path(project_dir, "src"))
    dir.create(file.path(project_dir, "out"))
    setwd(project_dir)
    writeLines(c("library(makr)",
                 "library(testthat)",
                 "expect_error(target(), 'could not find target')"),
               con = "src/results.R")
    cmd <- sprintf("%s/bin/Rscript src/results.R", R.home())
    val <- system(cmd)
    expect_equal(val, 0)
    unlink(project_dir, recursive = TRUE)
})

test_that("'target' throws appropriate error when interactive session", {
    expect_error(target(),
                 "could not find target : perhaps because file not run from command line?")
})









    
    
