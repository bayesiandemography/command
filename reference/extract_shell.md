# Turn a 'cmd_assign' Call Into a Shell Command

Extract a call to
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
from an R script, and turn it into a shell command.

## Usage

``` r
extract_shell(path_file, dir_shell = NULL)
```

## Arguments

- path_file:

  Path to the R script containing the call to
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md).
  The path starts at `dir_shell`.

- dir_shell:

  The directory that contains the shell script. The default is the
  current working directory.

## Value

`extract_shell()` is typically called for its side effect, which is to
print a shell command. However, `extract_shell()` invisibly returns a
text string with the command.

## The components of a shell command

The shell command produced by `extract_shell()` normally looks something
like this:

    Rscript src/model.R \
      data/cleaned.rds \
      out/model.rds \
      --use_log=TRUE

In this command

- `Rscript` is a call to
  [`utils::Rscript()`](https://rdrr.io/r/utils/Rscript.html);

- `\` is a "line continuation character";

- `data/cleaned.rds` and `out/model.rds` are unnamed arguments that
  Rscript passes to `src/model.R`; and

- `--use_log=TRUE` is a named argument that Rscript passes to
  `src/model.R`

## Using `extract_shell()` to build a data analysis workflow

- Step 1. Write an R script that carries out a step in analysis (eg
  tidying data, fitting a model, making a graph.) This script will
  contain a call to
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md),
  and will be the first argument passed to Rscript in the shell command.
  When writing and testing the script, use
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  interactively.

- Step 2. Once the R script is working correctly, call
  `extract_shell()`, and add the command to your shell script.

## Location of the shell script

The shell script normally sits at the top level of the project, so that
the project folder looks something like this:

    workflow.sh
    - data/
    - src/
    - out/
    report.qmd

## Identifying file arguments

To construct the rule, `extract_shell()` needs to be able to identify
arguments that refer to a file name. To do so, it uses the following
heuristic:

- if the call includes arguments whose names start with a dot, then
  these arguments are assumed to refer to file names;

- otherwise, find arguments whose values actually are file names (as
  determined by [`file.exists()`](https://rdrr.io/r/base/files.html)) or
  that look like they could be.

## References

- Episodes 1–3 of [The Unix
  Shell](https://swcarpentry.github.io/shell-novice/index.html)
  Introduction to the command line

- [Command-Line
  Programs](https://swcarpentry.github.io/r-novice-inflammation/05-cmdline.html)
  Introduction to Rscript

- [littler](https://CRAN.R-project.org/package=littler) Alternative to
  Rscript

## See also

- [`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
  Makefile equivalent of `extract_shell()`

- [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
  Create a shell script from calls to
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)

- [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  Process command line arguments

- [Quick
  Start](https://bayesiandemography.github.io/command/articles/quickstart.html)
  How to use
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)

- [Modular Workflows for Data
  Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
  Safe, flexible data analysis workflows

## Examples

``` r
library(fs)
library(withr)

with_tempdir({

  ## create 'src' directory
  dir_create("src")

  ## add an R script containing a call to 'cmd_assign'
  writeLines(c("cmd_assign(x = 1, .out = 'out/results.rds')",
               "results <- x + 1",
               "saveRDS(results, file = .out)"),
             con = "src/results.R")

  ## call 'extract_shell()'
  extract_shell(path_file = "src/results.R",
                dir_shell = ".")

})
#> Rscript ./src/results.R \
#>   out/results.rds \
#>   --x=1
```
