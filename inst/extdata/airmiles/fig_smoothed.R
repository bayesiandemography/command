
## Specify packages, inputs, and outputs ------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(command)
})

cmd_assign(.airmiles = "data/airmiles.csv",
           n_knot = 10,
           .out = "fig_smoothed.png")


## Read in data -------------------------------------------

airmiles <- read.csv(.airmiles)


## Analyse ------------------------------------------------

smoothed <- airmiles |>
  mutate(smoothed = fitted(smooth.spline(x = passengers,
                                         nknots = n_knot)))

p <- ggplot(smoothed, aes(x = year)) +
  geom_line(aes(y = smoothed)) +
  geom_point(aes(y = passengers)) +
  ggtitle(paste("Smoothed using", n_knot, "knots"))


## Save results -------------------------------------------

png(file = .out, width = 200, height = 200)
plot(p)
dev.off()

