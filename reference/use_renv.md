# Activate an renv Project for the Current Session

Make sure `renv` is activated, even if a script is run from Rscript or
littler.

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

Package [renv](https://rstudio.github.io/renv/) makes workflows
reproducible by locking down the versions of R packages. Normal `renv`
mechanisms can fail, however, if a script is run from Rscript or
littler. Adding

    command::use_renv()

to the top of R scripts fixes the problem.

For more details, see the article [Using command with
renv](https://bayesiandemography.github.io/command/articles/a5_renv.html).

## See also

- [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  Process command line arguments

- [Using command with
  renv](https://bayesiandemography.github.io/command/articles/a5_renv.html)

## Examples

``` r
command::use_renv() ## if renv not used here, no effect

## see article for full example
```
