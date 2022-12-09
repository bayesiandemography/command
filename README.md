
<!-- README.md is generated from README.Rmd. Please edit that file -->

# command

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Create objects in the current environment, based on arguments passed at
the command line. This can make a script work like a function: all
inputs are explicitly defined, and all variables are local. The result
is a clearer, safer, more modular workflow.

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
