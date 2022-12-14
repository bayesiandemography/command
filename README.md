
<!-- README.md is generated from README.Rmd. Please edit that file -->

# command

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bayesiandemography/command/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/command/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Turn arguments passed at the command line into objects in the current
environment. Scripts behave like functions, leading to a safer, more
modular workflow.

## Installation

``` r
devtools::install_github("bayesiandemography/command")
```

## Example

``` r
cmd_assign(survey_file = "out/survey_file.rds",
           n_iterations = 1000,
           trim_outliers = TRUE)
```
