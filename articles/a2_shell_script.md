# Creating a Shell Script

## Introduction

Functions
[`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
and
[`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
help with writing a shell script to control a data analysis workflow.
[`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
and
[`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
do most of the work, but some additional editing is usually necessary.

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

The workflow has five steps:

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

    cmd_assign(.cleaned_data = "out/cleaned_data.rds",
               rlm_method = "M",
               .out = "out/model.rds")

## extract_shell()

[`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
extracts the call to
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
from an R file, and turns it into a shell command. For instance, the
call

``` r
extract_shell("src/model.R")
```

has return value

    Rscript ./src/model.R \
      out/cleaned_data.rds \
      out/model.rds \
      --rlm_method=M

In the call to
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
above, the names `.cleaned_data` and `.out` start with a dot, and the
name `rlm_method` does not.
[`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
treats dotted name-value pairs differently fron non-dotted ones. A
dotted name-value pair is assumed to refer to an unnamed file path
passed at the command line, such as `out/cleaned_data.rds` or
`out/model.rds`. A non-dotted pair is assumed to refer to a named
argument, such as `--rlm_method=M`.

The easiest way to use
[`extract_shell()`](https://bayesiandemography.github.io/command/reference/extract_shell.md)
is to call it from the R console, and then cut-and-paste the results
into the shell script.

## shell_script()

[`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
creates a draft of the whole shell script.
[`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
loops through all the R files in a directory, extracting
[`cmd_assign()`](https://bayesiandemography.github.io/command/reference/cmd_assign.md)
calls and converting them into shell commands. It then puts the shell
commands into a script.

For instance, the call

``` r
shell_script("src")
```

creates a new file called `workflow.sh` with the lines

    Rscript src/cleaned_data.R \
      data/raw_data.csv \
      out/cleaned_data.rds

    Rscript src/fig_fitted.R \
      out/vals_fitted.rds \
      out/fig_fitted.png

    Rscript src/model.R \
      out/cleaned_data.rds \
      out/model.rds \
      --rlm_method=M

    Rscript src/vals_fitted.R \
      out/cleaned_data.rds \
      out/model.rds \
      out/vals_fitted.rds

Here, as in most cases, the output from
[`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
needs some editing before it is ready for use. The ordering of the
commands in `workflow.sh` reflects the ordering of the files in the
`src` directory. We need to rearrange the commands to instead reflect
the order in which they need to be executed. We also add instructions on
rendering the file `report.qmd`.

    Rscript src/cleaned_data.R \
      data/raw_data.csv \
      out/cleaned_data.rds

    Rscript src/model.R \
      out/cleaned_data.rds \
      out/model.rds \
      --rlm_method=M

    Rscript src/vals_fitted.R \
      out/cleaned_data.rds \
      out/model.rds \
      out/vals_fitted.rds

    Rscript src/fig_fitted.R \
      out/vals_fitted.rds \
      out/fig_fitted.png

    quarto render report.qmd

## Running the shell script

Our project directory looks like this.

``` fansi
.
├── data
│   └── raw_data.csv
├── out
├── report.qmd
├── src
│   ├── cleaned_data.R
│   ├── fig_fitted.R
│   ├── model.R
│   └── vals_fitted.R
└── workflow.sh
```

We run the shell script.

``` bash
bash workflow.sh
```

``` fansi
✔ Assigned object `.raw_data` with value "data/raw_data.csv" and class "character".
✔ Assigned object `.out` with value "out/cleaned_data.rds" and class "character".
✔ Assigned object `.cleaned_data` with value "out/cleaned_data.rds" and class "character".
✔ Assigned object `rlm_method` with value "M" and class "character".
✔ Assigned object `.out` with value "out/model.rds" and class "character".
✔ Assigned object `.cleaned_data` with value "out/cleaned_data.rds" and class "character".
✔ Assigned object `.model` with value "out/model.rds" and class "character".
✔ Assigned object `.out` with value "out/vals_fitted.rds" and class "character".
✔ Assigned object `.vals_fitted` with value "out/vals_fitted.rds" and class "character".
✔ Assigned object `.out` with value "out/fig_fitted.png" and class "character".
null device 
          1 


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
├── src
│   ├── cleaned_data.R
│   ├── fig_fitted.R
│   ├── model.R
│   └── vals_fitted.R
└── workflow.sh
```

We have created `cleaned_data.rds`, `model.rds`, and `vals_fitted.rds`,
and `fig_fitted.png` in the `out` directory, and `report.html` in the
main directory. The `report_files` directory was created by quarto.

## Other resources

- [makefile()](https://bayesiandemography.github.io/command/reference/makefile.html)
  The Makefile equivalent of
  [`shell_script()`](https://bayesiandemography.github.io/command/reference/shell_script.md)
- [Modular Workflows for Data
  Analysis](https://bayesiandemography.github.io/command/articles/workflow.html)
  Safe, flexible workflows
- [The Unix
  Shell](https://swcarpentry.github.io/shell-novice/index.html) Episodes
  1–3 introduce the command line
- [Command-Line
  Programs](https://swcarpentry.github.io/r-novice-inflammation/05-cmdline.html)
  Introduction to Rscript
