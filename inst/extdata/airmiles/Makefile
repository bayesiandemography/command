
.PHONY: all
all: report_smooth.pdf

fig_smoothed_5.png: fig_smoothed.R airmiles.csv
	Rscript $^ $@ --n=5  

fig_smoothed_10.png: fig_smoothed.R airmiles.csv
	Rscript $^ $@ --n=10

report_smooth.pdf: report_smooth.Rmd fig_smoothed_5.png fig_smoothed_10.png
	R --quiet -e "rmarkdown::render('$<')"

