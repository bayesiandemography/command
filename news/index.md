# Changelog

## command 0.3.0

### Variable-length file arguments

- Added
  [`cmd_assign_collect()`](https://bayesiandemography.github.io/command/reference/cmd_assign_collect.md)
  and
  [`cmd_assign_collect_quiet()`](https://bayesiandemography.github.io/command/reference/cmd_assign_collect.md),
  which assign every unnamed command-line argument to the one argument
  that was not passed by name.
- [`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md),
  [`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md),
  [`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md),
  and
  [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
  read calls to
  [`cmd_assign_collect()`](https://bayesiandemography.github.io/command/reference/cmd_assign_collect.md).
  A character vector of paths becomes one file name per element, and in
  a Makefile the last element is the target.
- These functions also now recognize the
  [`cmd_assign_quiet()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md),
  [`cmd_assign_collect_quiet()`](https://bayesiandemography.github.io/command/reference/cmd_assign_collect.md),
  and a call written with a `command::` or `command:::` prefix.
- Documented the dot-name convention for file arguments in the help for
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  and
  [`cmd_assign_collect()`](https://bayesiandemography.github.io/command/reference/cmd_assign_collect.md).
- [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
  orders commands by matching output paths with input paths.
- [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  and
  [`cmd_assign_collect()`](https://bayesiandemography.github.io/command/reference/cmd_assign_collect.md)
  reject a named command-line argument that is passed more than once.
- [`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md),
  [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md),
  [`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md),
  and
  [`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
  read literal values from calls. A name or a function call such as
  [`file.path()`](https://rdrr.io/r/base/file.path.html) or
  [`paste()`](https://rdrr.io/r/base/paste.html) is an error.

### Command-line values

- A named argument is split on the first `=`, so the value may itself
  contain `=`. An argument with an empty name, such as `--=value`, is an
  error.

### Shell commands

- [`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
  and
  [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
  leave a plain token unchanged, and quote a path or value only when it
  contains a space or a character that is special to the shell. `NULL`
  is written as `--name=NULL`.
- [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
  rejects a `quiet` value that is not `TRUE` or `FALSE`.

### Bug fixes

- The message for a missing directory now says that the directory does
  not exist.

### Documentation

- Revised the package description and the help pages.

## command 0.2.0

CRAN release: 2026-07-23

### Dependence on ‘fs’ package ([\#8](https://github.com/bayesiandemography/command/issues/8))

- Removed dependence on `fs` package

### Handling of empty strings ([\#6](https://github.com/bayesiandemography/command/issues/6))

- Empty command line values like `--v=` (from an undefined Make
  variable) are now an error unless the corresponding
  [`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
  argument is character
- When an empty string is assigned, the message notes that the value is
  an empty string

### Paths

- [`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
  and
  [`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
  no longer prefix relative paths with `./`

### DESCRIPTION and package help ([\#5](https://github.com/bayesiandemography/command/issues/5))

- Revised descriptions, and removed `internal` keyword from
  `command-package.R`.

### renv ([\#7](https://github.com/bayesiandemography/command/issues/7))

- Added
  [`use_renv()`](https://bayesiandemography.github.io/command/reference/use_renv.md),
  which activates a renv project at the start of scripts run with
  `Rscript` (which skips `.Rprofile`).
- Added article “Using command with renv”

## command 0.1.4

- Added test coverage

## command 0.1.3

CRAN release: 2025-11-22

### Documentation

- Tidied articles, and added detail on the way that
  [`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
  and
  [`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
  treat dotted vs non-dotted arguments.

### Bug fixes

- Fixed bug in
  [`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md),
  [`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md),
  [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md),
  and
  [`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
  where the shell commands or Makefile rules created by these function
  used absolute paths to R scripts, rather than relative paths.

## command 0.1.2

CRAN release: 2025-10-16

- Expanded description and provided options to turn off messages.

## command 0.1.0

- Initial CRAN submission.
