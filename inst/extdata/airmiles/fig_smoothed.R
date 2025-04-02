suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(command)
})

cmd_assign(.airmiles = "data/airmiles.csv",
           n = 10,
           .out = "fig_smoothed.png")

## read in the passenger data
airmiles <- read.csv(.airmiles)

## smooth passenger series
smoothed <- airmiles |>
  mutate(smoothed = fitted(smooth.spline(x = passengers, nknots = n)))

## create a plot
p <- ggplot(smoothed, aes(x = year)) +
  geom_line(aes(y = smoothed)) +
  geom_point(aes(y = passengers)) +
  ggtitle(paste("Smoothed using", n, "knots"))

## write the plot to a png file
png(file = .out,
    width = 200,
    height = 200)
plot(p)
dev.off()

