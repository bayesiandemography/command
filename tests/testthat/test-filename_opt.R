
test_that("'filename_opt' works with all deafults", {
    expect_identical(filename_opt("out/results-high.rds"),
                     "high")
    expect_identical(filename_opt("out/results-1.rds"),
                     1L)
})

test_that("'filename_opt' can pick out second and third options", {
    expect_identical(filename_opt("out/results-high-1-12.3.rds"),
                     "high")
    expect_identical(filename_opt("out/results-high-1-12.3.rds",
                                which = 2),
                     1L)
    expect_identical(filename_opt("out/results-high-1-12.3.rds",
                                which = 3),
                     12.3)
})

test_that("'filename_opt' work with non-default 'sep'", {
    expect_identical(filename_opt("out/resultsXXhigh.rds",
                                sep = "XX"),
                     "high")
    expect_identical(filename_opt("out/results.high.rds",
                                sep = "."),
                     "high")
    expect_identical(filename_opt("out/results%high.rds",
                                sep = "%"),
                     "high")
})

test_that("'filename_opt' works when 'sep' is a regular expression", {
    expect_identical(filename_opt("out/results999high.rds",
                                sep = "[0-9]+",
                                fixed = FALSE),
                     "high")
})

test_that("'filename_opt' works when 'choices' specified", {
    expect_identical(filename_opt("out/results-high.rds",
                                choices = c("low", "high")),
                     "high")
    expect_error(filename_opt("out/results-wrong.rds",
                            choices = c("low", "high")),
                     "value for option \\[\"wrong\"\\] not included in 'choices'")
})

test_that("'filename_opt' converts correctly", {
    expect_identical(filename_opt("out/result-1.3.rds"),
                     1.3)
    expect_identical(filename_opt("out/result-3.rds"),
                     3L)
    expect_identical(filename_opt("out/result-3.rds",
                                convert = FALSE),
                     "3")
})

test_that("'filename_opt' treats extensions correctly", {
    expect_identical(filename_opt("out/result.high.rds",
                                sep = "."),
                     "high")
    expect_error(filename_opt("out/result.high.rds",
                            which = 2,
                            sep = "."))
    expect_identical(filename_opt("out/result.high.rds",
                                which = 2,
                                sep = ".",
                                has_ext = FALSE),
                     "rds")
})

test_that("'filename_opt' raises correct error when 'target' has no options", {
    expect_error(filename_opt("out/result.rds"),
                 "filename 'out/result\\.rds' has no options")
})

test_that("'filename_opt' raises correct error when 'which' too high", {
    expect_error(filename_opt("out/result-high.rds",
                            which = 2),
                 "'which' equals 2 but filename 'out/result-high\\.rds' only has one option")
    expect_error(filename_opt("out/result-high-2.rds",
                            which = 3),
                 "'which' equals 3 but filename 'out/result-high-2\\.rds' only has 2 options")
})








