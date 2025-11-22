# Create a Shell Script

Create a shell script for a data analysis workflow consisting of
commands extracted from existing R files.

## Usage

``` r
shell_script(
  path_files,
  dir_shell = NULL,
  name_shell = "workflow.sh",
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- path_files:

  A path from `dir_shell` to a directory with R scripts containing calls
  to
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md).

- dir_shell:

  The directory where `shell_script()` will create the shell script. If
  no value is supplied, then `shell_script()` creates the shell script
  in the current working directory.

- name_shell:

  The name of the shell script. The default is `"workflow.sh"`.

- overwrite:

  Whether to overwrite an existing shell script. Default is `FALSE`.

- quiet:

  Whether to suppress progress messages. Default is `FALSE`.

## Value

`shell_script()` is called for its side effect, which is to create a
file. However, `shell_script()` also returns a string with the contents
of the shell script.

## Details

To create a shell script in the `files` directory, set `files` to `"."`.

To obtain the contents of the shell script without creating a file on
disk, creating the file on disk, set `name_shell` to `NULL`.

Supplying a value for `files` is compulsory for `shell_script()`, but
optional for
[`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md).
The output from `shell_script()` is generated entirely from `files`
while the output from
[`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
also includes some general-purpose Makefile commands.

## References

- Episodes 1–3 of [The Unix
  Shell](https://swcarpentry.github.io/shell-novice/index.html)
  Introduction to the command line

- [Command-Line
  Programs](https://swcarpentry.github.io/r-novice-inflammation/05-cmdline.html)
  Introduction to Rscript

## See also

- [Creating a Shell
  Script](https://bayesiandemography.github.io/command/articles/a2_shell_script.html)
  More on `shell_script()`

- [`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
  Turn a
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  call into a shell command

- [`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
  Makefile equivalent of `shell_script()`

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

  ## call 'shell_script()'
  shell_script(path_files = "src",
               dir_shell = ".")

  ## shell script has been created
  dir_tree()

  ## print contents of shell script
  cat(readLines("workflow.sh"), sep = "\n")

})
#> ✔ Extracted call to `cmd_assign()` in src/more_results.R.
#> ✔ Extracted call to `cmd_assign()` in src/results.R.
#> .
#> ├── src
#> │   ├── more_results.R
#> │   └── results.R
#> └── workflow.sh
#> 
#> Rscript src/more_results.R \
#>   out/more_results.rds \
#>   --x=1
#> 
#> Rscript src/results.R \
#>   out/results.rds \
#>   --x=1
#> 
#> 
```
