
.PHONY: all
all: report.html

fig_smoothed_5.png: fig_smoothed.R airmiles.csv
	Rscript $^ $@ --n_knot=5  

fig_smoothed_10.png: fig_smoothed.R airmiles.csv
	Rscript $^ $@ --n_knot=10

report.html: report.qmd fig_smoothed_5.png fig_smoothed_10.png
	quarto render $<

