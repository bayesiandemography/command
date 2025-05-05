
.PHONY: all
all: report.pdf

fig_smoothed_5.png: fig_smoothed.R airmiles.csv
	Rscript $^ $@ --n_knot=5  

fig_smoothed_10.png: fig_smoothed.R airmiles.csv
	Rscript $^ $@ --n_knot=10

report.pdf: report.qmd fig_smoothed_5.png fig_smoothed_10.png
	Rscript -e "quarto::quarto_render('$<')"

