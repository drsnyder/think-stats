# Rscript exponential.R data.csv alpha
require(ggplot2)
require(reshape2)
# 
args <- commandArgs(TRUE)
data <- read.csv(args[1], header = TRUE)

title <- paste("Exponential CDF(x), λ = ", args[2])
ggplot(data, aes(x=x)) + 
    geom_line(aes(y=y, color="x")) + 
    scale_colour_manual("", breaks=c("y"), values=c("blue")) +
    labs(y="CDF(x)", x="x", title=title) 
ggsave("plots/exponential-cdf.png")


# Plotting the CCDF of the exponential distribution on a log-y scale should be
# a straight line with slope -lambda.
title <- paste("Exponential CCDF(x) on Log-y Scale, λ = ", args[2])
ggplot(data, aes(x=x)) + 
    geom_line(aes(y=log(1-y), color="x")) + 
    scale_colour_manual("", breaks=c("y"), values=c("blue")) +
    labs(y="1 - CDF(x)", x="x", title=title) 
ggsave("plots/exponential-ccdf-log-y.png")

