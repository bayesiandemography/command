
Rscript fig_smoothed.R airmiles.csv 5 fig_smoothed_5.png 

Rscript fig_smoothed.R airmiles.csv 10 fig_smoothed_10.png 

R --quiet -e "rmarkdown::render('report_smooth.Rmd')"



