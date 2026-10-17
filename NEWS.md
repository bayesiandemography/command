
# command 0.3.0

## Variable-length file arguments

* Added `cmd_assign_collect()` and `cmd_assign_collect_quiet()`,
  which assign every unnamed command-line argument to the one
  argument that was not passed by name.
* `extract_make()`, `extract_shell()`, `makefile()`, and
  `shell_script()` read calls to `cmd_assign_collect()`. A
  character vector of paths becomes one file name per element,
  and in a Makefile the last element is the target.
* `extract_make()`, `extract_shell()`, `makefile()`, and
  `shell_script()` now recognize `cmd_assign_quiet()`,
  `cmd_assign_collect_quiet()`, and a call
  written with a `command::` or `command:::` prefix.
* The dot-name convention for file arguments has been documented
  in the help for `cmd_assign()` and `cmd_assign_collect()`.
* `shell_script()` orders commands by matching output paths
  with input paths. 
* `cmd_assign()` and `cmd_assign_collect()` reject a named
  command-line argument that is passed more than once.
* `makefile()`, `shell_script()`, `extract_make()`, and
  `extract_shell()` read literal values from calls.
  A name or a function call such as `file.path()` or
  `paste()` is an error.

## Command-line values

* A named argument is split on the first `=`, so the value
  may itself contain `=`. An argument with an empty name,
  such as `--=value`, is an error.

## Shell commands

* `extract_shell()` and `shell_script()` leave a plain
  length-1 vector unchanged,
  and quote a path or value only when it contains
  a space or a character that is special to the shell.
  `NULL` is written as `--name=NULL`.
* `shell_script()` rejects a `quiet` value that is not
  `TRUE` or `FALSE`.

## Bug fixes

* The message for a missing directory now says that the
  directory does not exist.

## Documentation

* Revised the package description and the help pages.

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
