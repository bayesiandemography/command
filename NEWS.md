
# command 0.2.0

## Dependence on 'fs' package (#8)

* Removed dependence on `fs` package

## Handling of empty strings (#6)

* Empty command line values like `--v=` (from an undefined
  Make variable) are now an error unless the corresponding
  `cmd_assign()` argument is character
* When an empty string is assigned, the message notes
  that the value is an empty string

## Paths

* `extract_make()` and `extract_shell()` no longer prefix relative
  paths with `./`

## DESCRIPTION and package help (#5)

* Revised descriptions, and removed `internal` keyword from 
  `command-package.R`.

## renv (#7)

* Added `use_renv()`, which activates a renv project at the start of
  scripts run with `Rscript` (which skips `.Rprofile`).
* Added article "Using command with renv"

# command 0.1.4

* Added test coverage

# command 0.1.3

## Documentation

* Tidied articles, and added detail on the way that `extract_make()`
  and `extract_shell()` treat dotted vs non-dotted arguments.

## Bug fixes

* Fixed bug in `extract_shell()`, `extract_make()`, `shell_script()`,
  and `makefile()` where the shell commands or Makefile rules created
  by these function used absolute paths to R scripts, rather than
  relative paths.


# command 0.1.2

* Expanded description and provided options to turn off messages.


# command 0.1.0

* Initial CRAN submission.
