# Rscript bakery.R bakery.csv
# Exercise 5.6
# Example with n = 4
# (def r (five/compare-baker-and-poincare 950 50 4 1000))
#

require(ggplot2)
require(reshape2)

args <- commandArgs(TRUE)
breada <- read.csv(args[1], header=TRUE)

ggplot(data=breada) +
    geom_line(aes(x=bweights,y=Baker,color=c("Baker"))) +
    geom_line(aes(x=pweights,y=Poincare,color=c("Poincaré"))) +
    labs(y="Frequency", x="Weight in Grams", title="Poincaré vs the Baker") +
    theme(legend.title=element_blank())
ggsave("plots/bakery.png")

