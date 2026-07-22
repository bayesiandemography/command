# Activate a renv Project for the Current Session

Call this as the first line of a pipeline script run with `Rscript` (or
littler), **before** any
[`library()`](https://rdrr.io/r/base/library.html) calls. `Rscript` does
not source `.Rprofile`, so renv is otherwise not activated and packages
may be loaded from the wrong library.

## Usage

``` r
use_renv(project = NULL, quiet = TRUE)
```

## Arguments

- project:

  Optional path to a project root. If `NULL` (the default), search
  upward from the script's directory when known, otherwise from
  [`getwd()`](https://rdrr.io/r/base/getwd.html). The script directory
  is taken from `Rscript`'s `--file=` argument when present, or from
  source references under littler.

- quiet:

  If `TRUE` (the default), suppress messages.

## Value

The project root path (invisibly) if renv was activated or was already
active; otherwise `NULL`.

## Details

`use_renv()` looks for a renv project root, and if one is found and not
already active, sources `renv/activate.R`. If no renv project is found,
it does nothing. Non-renv projects are fine.

## Finding the `command` package

To call `command::use_renv()` before
[`library(command)`](https://bayesiandemography.github.io/command/),
`command` must already be findable—typically because it is installed in
your user or system library (e.g. from CRAN). That is the usual setup
for a CRAN package used across projects.

## What `use_renv()` does not do

It does not run `renv::restore()`, install packages, or activate other
environment managers. It does not run automatically inside
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md).

If a directory has .file renv.lock but no .file renv/activate.R,
`use_renv()` issues a warning and returns `NULL` (nothing was
activated).

## See also

- [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  Process command line arguments

- [Using command with
  renv](https://bayesiandemography.github.io/command/articles/a5_renv.html)

- [renv](https://rstudio.github.io/renv/)

## Examples

``` r
if (FALSE) { # \dontrun{
# At the top of a pipeline script:
command::use_renv()

library(dplyr)
library(command)

cmd_assign(.data = "data/cleaned.rds",
           .out = "out/model.rds")
} # }
```
