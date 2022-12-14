
## Outputs from analysis ------------------------------------------------------

.PHONY: all
all: out/tab_coef.csv out/fig_fertility.pdf out/tab_fertility.csv


## Analysis -------------------------------------------------------------------

## Prepare data

out/dataset.rds: src/dataset.R data/swiss.csv
	Rscript $^


## Run model

out/model.rds: src/model.R out/dataset.rds
	Rscript $^ --robust_method=MM


## Output coefficient estimates

out/tab_coef.csv: src/tab_coef.R out/model.rds
	Rscript $^


## Extract actual and predicted fertility

out/vals_fertility.rds: src/vals_fertility.R out/model.rds
	Rscript $^


## Graph actual and predicted fertility

out/fig_fertility.pdf: src/fig_fertility.R out/vals_fertility.rds
	Rscript $^ --robust_method=MM


## Output numbers for actual and predicted fertility

out/tab_fertility.csv: src/tab_fertility.R out/vals_fertility.rds
	Rscript $^ --digits_round=1


## Clean up -------------------------------------------------------------------

.PHONY: clean
clean:
	rm -rf out
	mkdir out

