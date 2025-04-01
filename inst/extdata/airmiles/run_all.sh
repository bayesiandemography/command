
Rscript src/fig_smoothed.R data/airmiles.csv 5 out/fig_smoothed_5.png 

Rscript src/fig_smoothed.R data/airmiles.csv 10 out/fig_smoothed_10.png 

R --quiet -e "rmarkdown::render('report_smooth.Rmd')"



