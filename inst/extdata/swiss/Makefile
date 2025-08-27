
.PHONY: all
all: report.html

out/cleaned_data.rds: src/cleaned_data.R \
  data/raw_data.csv
	Rscript $^ $@

out/model_m.rds: src/model.R \
  out/cleaned_data.rds
	Rscript $^ $@ --method=M

out/model_mm.rds: src/model.R \
  out/cleaned_data.rds
	Rscript $^ $@ --method=MM

out/vals_fitted.rds: src/vals_fitted.R \
  out/cleaned_data.rds \
  out/model_m.rds \
  out/model_mm.rds
	Rscript $^ $@

out/fig_fitted.png: src/fig_fitted.R \
  out/vals_fitted.rds
	Rscript $^ $@

report.html: report.qmd \
  out/fig_fitted.png
	quarto render report.qmd


.PHONY: clean
clean:
	rm -rf out
	mkdir out


