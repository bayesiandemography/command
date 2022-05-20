
test_that("'opt_target' works with all deafults", {
    expect_identical(opt_target("out/results-high.rds"),
                     "high")
    expect_identical(opt_target("out/results-1.rds"),
                     1L)
})

test_that("'opt_target' can pick out second and third options", {
    expect_identical(opt_target("out/results-high-1-12.3.rds"),
                     "high")
    expect_identical(opt_target("out/results-high-1-12.3.rds",
                                which = 2),
                     1L)
    expect_identical(opt_target("out/results-high-1-12.3.rds",
                                which = 3),
                     12.3)
})

test_that("'opt_target' work with non-default 'sep'", {
    expect_identical(opt_target("out/resultsXXhigh.rds",
                                sep = "XX"),
                     "high")
    expect_identical(opt_target("out/results.high.rds",
                                sep = "."),
                     "high")
    expect_identical(opt_target("out/results%high.rds",
                                sep = "%"),
                     "high")
})

test_that("'opt_target' works when 'sep' is a regular expression", {
    expect_identical(opt_target("out/results999high.rds",
                                sep = "[0-9]+",
                                fixed = FALSE),
                     "high")
})

test_that("'opt_target' works when 'choices' specified", {
    expect_identical(opt_target("out/results-high.rds",
                                choices = c("low", "high")),
                     "high")
    expect_error(opt_target("out/results-wrong.rds",
                            choices = c("low", "high")),
                     "value for option \\[\"wrong\"\\] not included in 'choices'")
})

test_that("'opt_target' converts correctly", {
    expect_identical(opt_target("out/result-1.3.rds"),
                     1.3)
    expect_identical(opt_target("out/result-3.rds"),
                     3L)
    expect_identical(opt_target("out/result-3.rds",
                                convert = FALSE),
                     "3")
})

test_that("'opt_target' treats extensions correctly", {
    expect_identical(opt_target("out/result.high.rds",
                                sep = "."),
                     "high")
    expect_error(opt_target("out/result.high.rds",
                            which = 2,
                            sep = "."))
    expect_identical(opt_target("out/result.high.rds",
                                which = 2,
                                sep = ".",
                                has_ext = FALSE),
                     "rds")
})

test_that("'opt_target' raises correct error when 'target' has not options", {
    expect_error(opt_target("out/result.rds"),
                 "target 'out/result\\.rds' has no options")
})

test_that("'opt_target' raises correct error when 'which' too high", {
    expect_error(opt_target("out/result-high.rds",
                            which = 2),
                 "'which' equals 2 but target 'out/result-high\\.rds' only has one option")
    expect_error(opt_target("out/result-high-2.rds",
                            which = 3),
                 "'which' equals 3 but target 'out/result-high-2\\.rds' only has 2 options")
})








