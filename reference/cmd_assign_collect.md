# Assign Varying Set of Values Passed at the Command Line or Interactively

Assign names to values in the global environment, like
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md),
but without specifying the number of values in advance. Values for
unnamed command line arguments are collected into a single character
vector.

`cmd_assign_collect_quiet()` is identical to `cmd_assign_collect()`
except that it does not print progress messages to the console.

## Usage

``` r
cmd_assign_collect(...)

cmd_assign_collect_quiet(...)
```

## Arguments

- ...:

  Name-value pairs.

## Value

`cmd_assign_collect()` is called for its side effect, which is to create
objects in the global environment. However, `cmd_assign_collect()` also
invisibly returns a named list of objects.

## Matching names and values

`cmd_assign_collect()`, like
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md),
starts by processing named command line arguments (i.e. arguments of the
form `--n_draw=100`). Unlike
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md),
however, `cmd_assign_collect()` puts all the values that are left over
into a single character vector. This vector can be any length, including
zero. `cmd_assign_collect()` must contain a single name-value pair that
is not used for named command line arguments. `cmd_assign_collect()`
assigns the name from this name-value to the character vector.

If, for instance, the script `combine.R` contains

    cmd_assign_collect(
      .files = c("data/a.csv", "data/b.csv", "out/combined.rds"),
      use_log = TRUE
    )

and if `combine.R` is run from the command line using

    Rscript combine.R \
      data/a.csv \
      data/c.csv \
      out/combined.rds \
      --use_log=TRUE

then `use_log` will be assigned the value `TRUE` and `.files` will be
assigned the value `c("data/a.csv", "data/c.csv", "out/combined.rds")`.

## Processing the vector of unnamed arguments

The R script in which `cmd_assign_collect()` is called has
responsibility for processing the character vector of values from
unnamed command line arguments. The call to `cmd_assign_collect()`
above, for instance, might be followed by statements such as

    .inputs <- head(.files, -1)
    .out <- tail(.files, 1)
    inputs <- lapply(.inputs, read.csv)

## Use cmd_assign() where possible

`cmd_assign_collect()` is more flexible than
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md),
in that it does not require the number, name, or type of values to be
specified in advance. But if these things are in fact known in advance,
then it is good practice to document and test for them, which is what
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
does.

## See also

- [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  Process command line arguments with one value per argument

- [`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
  Turn a
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  or `cmd_assign_collect()` call into a shell command

- [`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
  Turn a
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  or `cmd_assign_collect()` call into a Makefile rule

- [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
  Create a shell script

- [`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
  Create a Makefile

- [Quick
  Start](https://bayesiandemography.github.io/command/articles/a1_quickstart.html)
  How to use
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)

## Examples

``` r
if (interactive()) {
  cmd_assign_collect(
    files = c("data/x.csv", "data/y.csv"),
    use_log = TRUE
  )
}
```
