
<!-- README.md is generated from README.Rmd. Please edit that file -->

# command

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Turn arguments passed at the command line into objects in the current
environment. Scripts become more function-like, with all inputs
explicitly defined. The aim is to create a clear, safe, modular
workflow.

## Installation

You can install the development version of command from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bayesiandemography/command")
```

## Example

command has a single function, `cmd_assign`:

``` r
cmd_assign(survey_file = "out/survey_file.rds",
           n_iterations = 1000,
           trim_outliers = TRUE)
```
