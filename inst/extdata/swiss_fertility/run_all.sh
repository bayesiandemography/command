
## Read in data, and output to 'out/dataset.rds'
Rscript src/dataset.R data/swiss.csv

## Run model, and output results to 'out/model.rds'
Rscript src/model.R out/dataset.rds --robust_method=MM

## Put estimates in 'out/tab_coef.csv'
Rscript src/tab_coef.R out/model.rds

## Put values for fertility in 'out/vals_fertility.rds'
Rscript src/vals_fertility.R out/model.rds

## Put graph of fertility in 'out/fig_fertility.pdf'
Rscript src/fig_fertility.R out/vals_fertility.rds --robust_method=MM

## Put data from graph in 'out/tab_fertility.csv'
Rscript src/tab_fertility.R out/vals_fertility.rds --digits_round=1


