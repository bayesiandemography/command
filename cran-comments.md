
# Resubmission of version 0.1.1, 11 October 2025

Thank you for the feedback on the original submission. Here's my
responses.

## Description field too short

The original Description was indeed a bit terse. I have extended it
from 

*Process command line arguments, as part of a data analysis pipeline*

to

*Process command line arguments, as part of a data analysis
pipeline. The pipeline is controlled by a Makefile or shell
script. Functions to construct Makefiles and shell scripts are
included in a the package. The aim is a pipeline that
is modular, transparent, and reliable.*

There is not (yet) a reference I can point to, so I haven't included
anything in the Description field.


## Example uses if(interactive())

The example is for function `cmd_assign()`. `cmd_assign()` usually
looks for aguments passed at the command line, but can also be used
interactively. My understanding is that there is no safe way to pass
arguments at the command line in an example. If so, then I think this
is in fact a case where wrapping the example in a call to
`is_interactive()` is appropriate?


## You write information messages to the console that cannot be easily
suppressed.

`cmd_assign()` is the main offender here. The messages are for
logging, but it's true that some users might prefer not to have
them.

I have added a second function called `cmd_assign_quiet()`, which is
identical to `cmd_assign()`, except that it does not print
messages. (This seems like a better strategy than adding an argument
called `quiet` or similar to `cmd_assign()` itself, because this could
easily lead to name clashes, given the way that `cmd_assign()` uses its `...` argument.)

I have also added `quiet` arguments to functions `extract_make()`,
`extract_shell()`, `makefile()`, and `shell_script()`, which
print small progress messages.



## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
