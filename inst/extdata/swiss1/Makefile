
.PHONY: all
all: report.html


out/cleaned_data.rds: src/cleaned_data.R \
  data/raw_data.csv
	Rscript $^ $@

out/model.rds: src/model.R \
  out/cleaned_data.rds
	Rscript $^ $@

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
