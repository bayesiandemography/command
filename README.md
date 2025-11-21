
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href="https://github.com/bayesiandemography/command">
<img src="man/figures/sticker.png"
       style="float:right; height:138px;" /> </a>

# command

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bayesiandemography/command/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/command/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bayesiandemography/command/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bayesiandemography/command?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/command)](https://CRAN.R-project.org/package=command)
<!-- badges: end -->

Process command line arguments as part of data analysis workflow.

The main function is
[cmd_assign()](https://bayesiandemography.github.io/command/reference/cmd_assign.html).

For an overview of the package, see [Quick
Start](https://bayesiandemography.github.io/command/articles/a1_quickstart.html).

For ideas on building safe, modular workflows, see [Modular Workflows
for Data
Analysis](https://bayesiandemography.github.io/command/articles/a4_workflow.html).

## Installation

``` r
install.packages("command")
```

## Example

``` r
cmd_assign(.data = "data/raw_data.csv",
           date_start = "2025-01-01",
           trim_outliers = TRUE,
           .out = "out/cleaned_data.rds")
```
