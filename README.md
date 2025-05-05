
<!-- README.md is generated from README.Rmd. Please edit that file -->

# command

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bayesiandemography/command/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/command/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bayesiandemography/command/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bayesiandemography/command?branch=main)
<!-- badges: end -->

Process command line arguments as part of data analysis workflow.

- \[cmd_assign()\] is the main function.
- [Quick Start
  Guide](https://bayesiandemography.github.io/command/articles/quickstart.html)
  shows how to use `cmd_assign()`.
- [A Workflow for Data
  Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
  presents an approach to the design of data analysis workflows.

## Installation

``` r
devtools::install_github("bayesiandemography/command")
```

## Example

``` r
cmd_assign(.data = "data/raw_data.csv",
           date_start = "2025-01-01",
           trim_outliers = TRUE,
       .out = "out/cleaned_data.rds")
```
