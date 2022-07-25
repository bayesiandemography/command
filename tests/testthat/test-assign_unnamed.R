
test_that("'assign_unnamed' works with valid arguments - on command line", {
    dir_curr <- getwd()
    dir_tmp <- tempfile()
    if (file.exists(dir_tmp))
        unlink(dir_tmp, recursive = TRUE)
    dir.create(dir_tmp)
    setwd(dir_tmp)
    writeLines(c("argfun::assign_unnamed(p1 = 'a1', p2 = 'a2')",
                 "saveRDS(p1, file = 'p1.rds')",
                 "saveRDS(p2, file = 'p2.rds')",
                 "saveRDS(ls(), file = 'ls.rds')"),
               con = "script.R")
    cmd <- sprintf("%s/bin/Rscript script.R fn1.rds -n=1 --named=hello fn2.rds",
                   R.home())
    system(cmd)
    p1 <- readRDS("p1.rds")
    p2 <- readRDS("p2.rds")
    ls <- readRDS("ls.rds")
    expect_identical(p1, "fn1.rds")
    expect_identical(p2, "fn2.rds")
    expect_identical(ls, c("p1", "p2"))    
    setwd(dir_curr)
    unlink(dir_tmp, recursive = TRUE)
})

test_that("'assign_unnamed' works with valid arguments - interactively", {
    assign_unnamed(p1 = 'a1', p2 = 'a2')
    expect_identical(p1, "a1")
    expect_identical(p2, "a2")
    expect_identical(ls(), c("p1", "p2"))
})


