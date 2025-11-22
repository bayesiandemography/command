# Changelog

## command 0.1.3

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
