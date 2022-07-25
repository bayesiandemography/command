
test_that("'assign_named' works with valid arguments - on command line", {
    dir_curr <- getwd()
    dir_tmp <- tempfile()
    if (file.exists(dir_tmp))
        unlink(dir_tmp, recursive = TRUE)
    dir.create(dir_tmp)
    setwd(dir_tmp)
    writeLines(c("argfun::assign_named(n = 2, named = 'goodbye')",
                 "saveRDS(n, file = 'n.rds')",
                 "saveRDS(named, file = 'named.rds')",
                 "saveRDS(ls(), file = 'ls.rds')"),
               con = "script.R")
    cmd <- sprintf("%s/bin/Rscript script.R fn1.rds -n=1 --named=hello fn2.rds",
                   R.home())
    system(cmd)
    n <- readRDS("n.rds")
    named <- readRDS("named.rds")
    ls <- readRDS("ls.rds")
    expect_identical(n, 1)
    expect_identical(named, "hello")
    expect_identical(ls, c("n", "named"))    
    setwd(dir_curr)
    unlink(dir_tmp, recursive = TRUE)
})

test_that("'assign_named' works with valid arguments - interactively", {
    assign_named(n = 2, named = 'goodbye')
    expect_identical(n, 2)
    expect_identical(named, "goodbye")
    expect_identical(ls(), c("n", "named"))
})


