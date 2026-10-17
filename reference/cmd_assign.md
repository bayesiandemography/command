# Assign Values Passed at the Command Line or Interactively

Assign names to values in the global environment. The values are
typically supplied through the command line, but can be supplied
interactively.

`cmd_assign_quiet()` is identical to `cmd_assign()` except that it does
not print progress messages to the console.

## Usage

``` r
cmd_assign(...)

cmd_assign_quiet(...)
```

## Arguments

- ...:

  Name-value pairs.

## Value

`cmd_assign()` is called for its side effect, which is to create objects
in the global environment. However, `cmd_assign()` also invisibly
returns a named list of objects.

## Details

`cmd_assign()` is a user-friendly alternative to
[`commandArgs()`](https://rdrr.io/r/base/commandArgs.html), designed
specifically for data analysis workflows.

`cmd_assign()` can be used to record and enforce assumptions about
inputs and outputs. This provides scripts with better-defined
interfaces, making them clearer and safer.

## Types of session

`cmd_assign()` behaves differently depending on whether it is called
interactively, or from within an R script being run from the command
line.

For instance, if the code

`cmd_assign(use_log = TRUE)`

is run interactively, it creates an object called `use_log` with value
`TRUE`.

But if the same code is run from within a script that has been invoked
using the command

`Rscript tidy_data.R --use_log=FALSE`

it creates an object called `use_log` with value `FALSE`.

`cmd_assign()` is typically called interactively when a workflow is
being developed, and through the command line when the workflow is in
production.

## Matching names and values

When used in a script called from the command line, `cmd_assign()` first
matches named command line arguments, and then matches unnamed command
line arguments, in the order in which they are supplied.

If, for instance, the script `person.R` contains the lines

    cmd_assign(
      .data = "raw_data.csv",
       max_age = 85,
       .out = "person.rds"
    )

and if `person.R` is run from the command line using

    Rscript person.R \
      raw_data.csv \
      person.rds \
      --max_age=100

then `cmd_assign()` assigns `cmd_assign()` names to command line
arguments in the following order:

|       |                     |                       |
|-------|---------------------|-----------------------|
| order | `cmd_assign()` name | command line argument |
| 1     | `max_age`           | `--max_age=100`       |
| 2     | `.data`             | `raw_data.csv`        |
| 3     | `.out`              | `person.rds`          |

## The values specified in the call to cmd_assign() are not defaults

The values specified in the call to `cmd_assign()` are not defaults in
the sense of values to fall back on when no value is supplied. For
instance, if we take a script containing the call

    cmd_assign(
      n_draw = 10,
      .out = "simulation.rds"
    )

and run it from the command line without passing a value for `n_draw`,
then `cmd_assign()` will throw an error. It will *not* set `n_draw` to
`10`.

Defaults can hide bugs and make code less predictable.

## Coercing values passed at the command line

Values passed at the command line start out as text strings.
`cmd_assign()` coerces these strings to have the same class as the
corresponding values in the call to `cmd_assign()`. For instance, if
script `fit.R` contains the lines

    cmd_assign(
      .data = "data/individuals.rds",
      impute = TRUE,
      date = as.Date("2025-01-01"),
      .out = "output/fitted.rds"
    )

and if `fit.R` is run from the command line using

    Rscript src/fit.R \
      data/individuals.rds \
      output/fitted.rds \
      --impute=TRUE \
      --date=2025-01-01

then `cmd_assign()` will create the following length-1 vectors

|          |               |                          |
|----------|---------------|--------------------------|
| name     | class         | value                    |
| `.data`  | `"character"` | `"data/individuals.rds"` |
| `impute` | `"logical"`   | `TRUE`                   |
| `date`   | `"Date"`      | `"2025-01-01"`           |
| `.out`   | `"character"` | `"output/fitted.rds"`    |

## The dot-name convention

When naming arguments to `cmd_assign()`, it is typically good practice
to use names starting with dots for file paths, and names without dots
for ordinary objects, as in

    cmd_assign(
      .households = "data/households.csv",
      n_record = 500,
      sex = "Female",
      .out = "out/subsample_female.rds"
    )

Function `cmd_assign()` does not in fact distinguish between names with
dots and names without dots. But the dot-name convention is still useful
because of the way it calls attention to the distinction between file
paths and ordinary objects. For instance, a dot-name is a reminder that
the object being named is a reference to an input or output, rather than
the input or output itself.

In contrast, functions
[`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md),
[`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md),
[`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md),
and
[`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
*do* treat names with dots differently from names without dots. See the
help for
[`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md),
[`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md),
[`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md),
and
[`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
for details.

## References

- [Command-Line
  Programs](https://swcarpentry.github.io/r-novice-inflammation/05-cmdline.html)
  Introduction to Rscript

## See also

- [`cmd_assign_collect()`](https://bayesiandemography.github.io/command/reference/cmd_assign_collect.md)
  Collect unnamed command line arguments into one character vector

- [`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
  Turn a `cmd_assign()` call into a shell command

- [`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
  Turn a `cmd_assign()` call into a Makefile rule

- [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
  Create a shell script

- [`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
  Create a Makefile

- [`use_renv()`](https://bayesiandemography.github.io/command/reference/use_renv.md)
  Activate a renv project before loading packages

- [Quick
  Start](https://bayesiandemography.github.io/command/articles/a1_quickstart.html)
  How to use `cmd_assign()`

- [Modular Workflows for Data
  Analysis](https://bayesiandemography.github.io/command/articles/a4_workflow.html)
  Safe, flexible data analysis workflows.

- [Using command with
  renv](https://bayesiandemography.github.io/command/articles/a5_renv.html)
  Running pipeline scripts with renv

- Base R function
  [`commandArgs()`](https://rdrr.io/r/base/commandArgs.html) uses a more
  general, lower-level approach to processing command line arguments.
  ([`commandArgs()`](https://rdrr.io/r/base/commandArgs.html) is called
  internally by `cmd_assign()`.)

- [littler](https://CRAN.R-project.org/package=littler) Alternative to
  Rscript

## Examples

``` r
if (interactive()) {
  cmd_assign(.data = "mydata.csv",
             n_iter = 2000,
             .out = "results.rds")
}
```
