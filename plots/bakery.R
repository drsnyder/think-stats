# Rscript bakery.R bakery.csv bakery-raw.csv
# Exercise 5.6
# Example with n = 4
# (def r (five/compare-baker-and-poincare 950 50 4 1000))
#

require(ggplot2)
require(reshape2)

args <- commandArgs(TRUE)
breada <- read.csv(args[1], header=TRUE)
rawbreada <- read.csv(args[2], header=TRUE)


# generate the boxplot
png("plots/bakery-boxplot.png")
boxplot(rawbreada$baker, rawbreada$poincare, col=c("lightgreen", "lightblue"))
axis(side=1, 1:2, labels=c("Baker", "Poincaré"), cex.axis=0.8)
title("Baker vs Poincaré")
dev.off()

t.test(rawbreada$baker, rawbreada$poincare, alternative="less")

ggplot(data=breada) +
    geom_line(aes(x=bweights,y=Baker,color=c("Baker"))) +
    geom_line(aes(x=pweights,y=Poincare,color=c("Poincaré"))) +
    labs(y="Frequency", x="Weight in Grams", title="Poincaré vs the Baker") +
    theme(legend.title=element_blank())
ggsave("plots/bakery.png")

