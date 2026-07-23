# Changelog

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
