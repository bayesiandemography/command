
.PHONY: all
all: report_smooth.pdf

out/fig_smoothed_5.png: src/fig_smoothed.R data/airmiles.csv
	Rscript $^ $@ --n=5  

out/fig_smoothed_10.png: src/fig_smoothed.R data/airmiles.csv
	Rscript $^ $@ --n=10

report_smooth.pdf: report_smooth.Rmd out/fig_smoothed_5.png out/fig_smoothed_10.png
	R --quiet -e "rmarkdown::render('$<')"

