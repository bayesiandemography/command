# Creating a Makefile

## Introduction

Functions
[`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
and
[`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
help with writing a Makefile to control a data analysis workflow.
[`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
and
[`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
do much of the work, but some extra hand coding is also necessary.

## An example workflow

To demonstrate, we use a simple workflow where all the code, aside from
the shell script, has already been written. The project directory
contains the following files:

``` fansi
.
├── data
│   └── raw_data.csv
├── out
├── report.qmd
└── src
    ├── cleaned_data.R
    ├── fig_fitted.R
    ├── model.R
    └── vals_fitted.R
```

1.  `src/cleaned_data.R` reads in the raw data, cleans it, and creates
    `out/cleaned_data.rds`
2.  `src/model.R` fits a model to the cleaned data, and creates
    `out/model.rds`
3.  `src/vals_fitted.R` extract fitted values from the model, and
    creates `vals_fitted.rds`
4.  `src/fig_fitted.R` uses `vals_fitted.rds` to create the plot in
    `out/fig_fitted.png`
5.  `report.qmd` uses `out/fig_fitted.png` to create the document
    `report.html`

`cleaned_data.R`, `model.R`, and `vals_fitted.R`, and `fig_fitted.R` all
contain calls to
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md).
For instance, `src/model.R` contains the lines

`cleaned_data.R`, `model.R`, and `vals_fitted.R`, and `fig_fitted.R` all
contain calls to
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md).
For instance, `src/model.R` contains the lines

    cmd_assign(.cleaned_data = "out/cleaned_data.rds",
               rlm_method = "M",
               .out = "out/model.rds")

## extract_make()

[`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
extracts the call to
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
from an R file, and turns it into a Makefile rule. For instance, the
call

``` r
extract_make("src/model.R")
```

has return value

    out/model.rds: ./src/model.R \
      out/cleaned_data.rds
        Rscript $^ $@ --rlm_method=M

In the call to
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
above, the names `.cleaned_data` and `.out` start with a dot, and the
name `rlm_method` does not.
[`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
treats dotted name-value pairs differently fron non-dotted ones. A
dotted name-value pair is assumed to refer to an unnamed file path
passed at the command line, such as `out/cleaned_data.rds` or
`out/model.rds`. A non-dotted pair is assumed to refer to a named
argument, such as `--rlm_method=M`.

The easiest way to use
[`extract_make()`](https://bayesiandemography.github.io/command/reference/extract_make.md)
is to call it from the R console, and then cut-and-paste the results
into Makefile.

## makefile()

[`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
creates a draft of the whole Makefile.
[`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
loops through all the R files in a directory, extracting
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
calls and converting them into Makefile rules. It then puts the rules
into a Makefile.

For instance, the call

``` r
makefile("src")
```

creates a new file called `Makefile` with the lines

    .PHONY: all
    all:


    out/cleaned_data.rds: src/cleaned_data.R \
      data/raw_data.csv
        Rscript $^ $@

    out/fig_fitted.png: src/fig_fitted.R \
      out/vals_fitted.rds
        Rscript $^ $@

    out/model.rds: src/model.R \
      out/cleaned_data.rds
        Rscript $^ $@ --rlm_method=M

    out/vals_fitted.rds: src/vals_fitted.R \
      out/cleaned_data.rds \
      out/model.rds
        Rscript $^ $@


    .PHONY: clean
    clean:
        rm -rf out
        mkdir out

The output from
[`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
needs some editing before it is ready for use. We need to a rule for
creating the report, and make that report a prerequisite for `all:` at
the top of the file:

    .PHONY: all
    all: report.html


    out/cleaned_data.rds: src/cleaned_data.R \
      data/raw_data.csv
        Rscript $^ $@

    out/fig_fitted.png: src/fig_fitted.R \
      out/vals_fitted.rds
        Rscript $^ $@

    out/model.rds: src/model.R \
      out/cleaned_data.rds
        Rscript $^ $@ --rlm_method=M

    out/vals_fitted.rds: src/vals_fitted.R \
      out/cleaned_data.rds \
      out/model.rds
        Rscript $^ $@

    report.html: report.qmd \
      out/fig_fitted.png
        quarto render $<


    .PHONY: clean
    clean:
        rm -rf out
        mkdir out

The ordering of the rules in the Makefile reflects the ordering of the
files in the `src` directory, not the order in which the rules should be
executed. This does not matter to `make`, which constructs its own
dependency map. But for the benefit of human readers, we rearrange the
rules to match the execution order.

    .PHONY: all
    all: report.html


    out/cleaned_data.rds: src/cleaned_data.R \
      data/raw_data.csv
        Rscript $^ $@

    out/model.rds: src/model.R \
      out/cleaned_data.rds
        Rscript $^ $@ --rlm_method=M

    out/vals_fitted.rds: src/vals_fitted.R \
      out/cleaned_data.rds \
      out/model.rds
        Rscript $^ $@

    out/fig_fitted.png: src/fig_fitted.R \
      out/vals_fitted.rds
        Rscript $^ $@


    report.html: report.qmd \
      out/fig_fitted.png
        quarto render $<


    .PHONY: clean
    clean:
        rm -rf out
        mkdir out

## Running the Makefile

Our project directory looks like this.

``` fansi
.
├── Makefile
├── data
│   └── raw_data.csv
├── out
├── report.qmd
└── src
    ├── cleaned_data.R
    ├── fig_fitted.R
    ├── model.R
    └── vals_fitted.R
```

We run the Makefile.

``` bash
make
```

``` fansi
Rscript src/cleaned_data.R data/raw_data.csv out/cleaned_data.rds
✔ Assigned object `.raw_data` with value "data/raw_data.csv" and class "character".
✔ Assigned object `.out` with value "out/cleaned_data.rds" and class "character".
Rscript src/model.R out/cleaned_data.rds out/model.rds --rlm_method=M
✔ Assigned object `.cleaned_data` with value "out/cleaned_data.rds" and class "character".
✔ Assigned object `rlm_method` with value "M" and class "character".
✔ Assigned object `.out` with value "out/model.rds" and class "character".
Rscript src/vals_fitted.R out/cleaned_data.rds out/model.rds out/vals_fitted.rds
✔ Assigned object `.cleaned_data` with value "out/cleaned_data.rds" and class "character".
✔ Assigned object `.model` with value "out/model.rds" and class "character".
✔ Assigned object `.out` with value "out/vals_fitted.rds" and class "character".
Rscript src/fig_fitted.R out/vals_fitted.rds out/fig_fitted.png
✔ Assigned object `.vals_fitted` with value "out/vals_fitted.rds" and class "character".
✔ Assigned object `.out` with value "out/fig_fitted.png" and class "character".
null device 
          1 
quarto render report.qmd


processing file: report.qmd
1/3                  
2/3 [unnamed-chunk-1]
3/3                  
output file: report.knit.md

pandoc 
  to: html
  output-file: report.html
  standalone: true
  section-divs: true
  html-math-method: mathjax
  wrap: none
  default-image-extension: png
  variables: {}
  
metadata
  document-css: false
  link-citations: true
  date-format: long
  lang: en
  title: Swiss Fertility
  
Output created: report.html
```

Our project directory now looks like this.

``` fansi
.
├── Makefile
├── data
│   └── raw_data.csv
├── out
│   ├── cleaned_data.rds
│   ├── fig_fitted.png
│   ├── model.rds
│   └── vals_fitted.rds
├── report.html
├── report.qmd
├── report_files
│   └── libs
└── src
    ├── cleaned_data.R
    ├── fig_fitted.R
    ├── model.R
    └── vals_fitted.R
```

We have created `scaled.rds`, `model.rds`, and `tab_coef` in the `out`
directory, and `report.html` in the main directory.

### Named vs unnamed arguments

In the examples above, all the command line arguments were filenames.
Command line arguments can also, however, be values. The following
command, for instance, includes a named argument called `n_iter` with
value `100`.

``` bash
Rscript myfile.R --n_iter=100 myoutput.rds
```

The corresponding call to
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
will look something like this:

``` r
cmd_assign(.myfile = "myfile.R",
           n_iter = 100,
       .out = "myoutput.R")
```

When

## Other resources

- [shell_script()](https://bayesiandemography.github.io/command/reference/makefile.html)
  The shell script equivalent of
  [`makefile()`](https://bayesiandemography.github.io/command/reference/makefile.md)
- [Modeular Workflows for Data
  Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
  Safe, flexible workflows for data analysis
- [Project Management with
  Make](https://jeroenjanssens.com/dsatcl/chapter-6-project-management-with-make)
  Makefiles in data analysis workflows
- [GNU
  make](https://www.gnu.org/software/make/manual/make.html#SEC_Contents)
  Definitive guide
- [Command-Line
  Programs](https://swcarpentry.github.io/r-novice-inflammation/05-cmdline.html)
  Introduction to Rscript
