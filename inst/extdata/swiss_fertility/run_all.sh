
## Prepare data
## (create file 'out/dataset.rds')

Rscript src/dataset.R data/swiss.csv


## Run model
## (create file 'out/model.rds')

Rscript src/model.R out/dataset.rds --robust_method=MM


## Output coefficient estimates
## (create file 'out/tab_coef.csv')

Rscript src/tab_coef.R out/model.rds


## Extract actual and predicted fertility
## (create file 'out/vals_fertility.rds')

Rscript src/vals_fertility.R out/model.rds


## Graph actual and predicted fertility
## (create file 'out/fig_fertility.pdf')

Rscript src/fig_fertility.R out/vals_fertility.rds --robust_method=MM


## Output numbers for actual and predicted fertility
## (create file 'out/tab_fertility.csv')

Rscript src/tab_fertility.R out/vals_fertility.rds --digits_round=1


