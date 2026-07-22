# Using command with renv

## Key message

If a project uses package `renv`, all R scripts should start with the
line

``` r

command::use_renv()
```

## Introduction

One obstacle to creating reproducible workflows in R is that the
behavior of R packages can differ across versions. An R script that
behaves one way if it is run using version 1.0 of a package may behave a
different way if it is run using version 2.0 of the package. The
[renv](https://rstudio.github.io/renv/) package solves this problem by
allowing users to lock down the exact versions of the packages a project
uses.

The mechanism that `renv` uses to lock packages down is, however,
bypassed by
[Rscript](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/Rscript.html)
or [littler](https://CRAN.R-project.org/package=littler). A workflow
that uses Rscript or littler needs to be tweaked before `renv` performs
correctly. Function
[`command::use_renv()`](https://bayesiandemography.github.io/command/reference/use_renv.md)
helps with this tweaking.

## How renv can fail with Rscript and littler

A project that uses `renv` contains a script `renv/activate.R`. That
script tells R which package versions to use when running project code.
`renv` only performs correctly if R runs `renv/activate.R` when the
session starts.

`renv` arranges for `renv/activate.R` to be run at startup by adding
instructions to a file called `.Rprofile`. Normally, when R is launched,
it looks for `.Rprofile` and follows any instructions the file contains,
including sourcing `renv/activate.R`.

If R is launched from `Rscript` or `littler`, however, it does *not*
look for an `.Rprofile` file. Since it ignores `.Rprofile`, it does not
run `renv/activate.R`, and `renv` does not work.

## Fixing the problem with `command::use_renv()`

The problem of `Rscript` and `littler` bypassing `renv/activate.R` can
be fixed by including the line
[`command::use_renv()`](https://bayesiandemography.github.io/command/reference/use_renv.md)
at the top of each R script in the project, as in,

``` r

command::use_renv()

suppressPackageStartupMessages({
  library(dplyr)
  library(command)
})

cmd_assign(.data = "data/cleaned.rds",
           n_iter = 2000L,
           .out = "out/model.rds")

# ... rest of script ...
```

Function
[`use_renv()`](https://bayesiandemography.github.io/command/reference/use_renv.md)
looks for `renv` infrastructure and, if it finds it, sources
`renv/activate.R`. The sourcing occurs later than it would in a normal
`.Rprofile` startup, but early enough for subsequent
[`library()`](https://rdrr.io/r/base/library.html) calls to see the
project library.

If
[`use_renv()`](https://bayesiandemography.github.io/command/reference/use_renv.md)
does not find any `renv` infrastructure, it simply returns `NULL`.
Including
[`command::use_renv()`](https://bayesiandemography.github.io/command/reference/use_renv.md)
in projects that do not use `renv` does no harm.

## The exact format matters

The call to
[`command::use_renv()`](https://bayesiandemography.github.io/command/reference/use_renv.md)
must come before any calls to
[`library()`](https://rdrr.io/r/base/library.html). If
[`library()`](https://rdrr.io/r/base/library.html) has already been
called, then `renv/activate.R` does not work properly.

The prohibition against calling
[`library()`](https://rdrr.io/r/base/library.html) before
[`command::use_renv()`](https://bayesiandemography.github.io/command/reference/use_renv.md)
includes not calling
[`library(command)`](https://bayesiandemography.github.io/command/). Use
the `::` operator instead:

``` r

command::use_renv()

suppressPackageStartupMessages({
  library(dplyr)
  library(command)
})

# ... rest of script ...
```

Do *not* load `command` first:

``` r

library(command)  ## not correct!!
use_renv()        ## not correct!!

suppressPackageStartupMessages({
  library(dplyr)
  library(command)
})

# ... rest of script ...
```
