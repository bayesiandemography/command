# Create a Makefile

Create a Makefile for a data analysis workflow. The Makefile can include
rules extracted from existing R files.

## Usage

``` r
makefile(
  path_files = NULL,
  dir_make = NULL,
  name_make = "Makefile",
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- path_files:

  A path from `dir_make` to a directory with R scripts containing calls
  to
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md).
  Optional.

- dir_make:

  The directory where `makefile()` will create the Makefile. If no value
  is supplied, then \`makefile(); creates the Makefile the current
  working directory.

- name_make:

  The name of the Makefile. The default is `"Makefile"`.

- overwrite:

  Whether to overwrite an existing Makefile. Default is `FALSE`.

- quiet:

  Whether to suppress progress messages. Default is `FALSE`.

## Value

`makefile()` is called for its side effect, which is to create a file.
However, `makefile()` also returns a string with the contents of the
Makefile.

## Details

To create a Makefile in the `files` directory, set `files` to `"."`.

To obtain the contents of the Makefile without creating a file on disk,
creating the file on disk, set `name_make` to `NULL`.

Supplying a value for `files` is optional for `makefile()`, but
compulsory for
[`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md).
The output from `makefile()` includes some general-purpose Makefile
commands, while the output from
[`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
is generated entirely from `files`.

## References

- [Project Management with
  Make](https://jeroenjanssens.com/dsatcl/chapter-6-project-management-with-make)
  Makefiles in data analysis workflows

- [GNU
  make](https://www.gnu.org/software/make/manual/make.html#SEC_Contents)
  Definitive guide

- [Command-Line
  Programs](https://swcarpentry.github.io/r-novice-inflammation/05-cmdline.html)
  Introduction to Rscript

## See also

- [Creating a
  Makefile](https://bayesiandemography.github.io/command/articles/a3_makefile.html)
  More on `makefile()`

- [`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
  Turn a
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  call into a Makefile rule

- [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
  Shell script equivalent of `makefile()`

- [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  Process command line arguments

- [Modular Workflows for Data
  Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
  Safe, flexible data analysis workflows

- [littler](https://CRAN.R-project.org/package=littler) Alternative to
  Rscript

## Examples

``` r
library(fs)
library(withr)

with_tempdir({

  ## create 'src'  directory
  dir_create("src")

  ## put R scripts containing calls to
  ## 'cmd_assign' in the 'src' directory
  writeLines(c("cmd_assign(x = 1, .out = 'out/results.rds')",
               "results <- x + 1",
               "saveRDS(results, file = .out)"),
             con = "src/results.R")
  writeLines(c("cmd_assign(x = 1, .out = 'out/more_results.rds')",
               "more_results <- x + 2",
               "saveRDS(more_results, file = .out)"),
             con = "src/more_results.R")

  ## call 'makefile()'
  makefile(path_files = "src",
           dir_make = ".")

  ## Makefile has been created
  dir_tree()

  ## print contents of Makefile
  cat(readLines("Makefile"), sep = "\n")

})
#> ✔ Extracted call to `cmd_assign()` in src/more_results.R.
#> ✔ Extracted call to `cmd_assign()` in src/results.R.
#> .
#> ├── Makefile
#> └── src
#>     ├── more_results.R
#>     └── results.R
#> 
#> .PHONY: all
#> all:
#> 
#> 
#> out/more_results.rds: src/more_results.R
#>  Rscript $^ $@ --x=1
#> 
#> out/results.rds: src/results.R
#>  Rscript $^ $@ --x=1
#> 
#> 
#> .PHONY: clean
#> clean:
#>  rm -rf out
#>  mkdir out
#> 
#> 
```
