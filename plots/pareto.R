# Rscript pareto.R data.csv alpha threshold 
require(ggplot2)
require(reshape2)
# 
args <- commandArgs(TRUE)
data <- read.csv(args[1], header = TRUE)
alpha <- args[2]
threshold <- args[3]
print(alpha)
print(threshold)

title <- paste("Pareto CDF(x), α = ", alpha, ", threshold = ", threshold)
ggplot(data, aes(x=x)) + 
    geom_line(aes(y=y, color="x")) + 
    scale_colour_manual("", breaks=c("y"), values=c("blue")) +
    labs(y="CDF(x)", x="x", title=title) 
ggsave("plots/pareto-cdf.png")


# Plotting the CCDF of the Pareto on a log-log scale should be
# a straight line with slope -alpha and intercept alpha * xm.
ggplot(data, aes(x=log(x))) + 
    geom_line(aes(y=log(1-y), color="x")) + 
    scale_colour_manual("", breaks=c("y"), values=c("blue")) +
    labs(y="1 - CDF(x)", x="x", title="Pareto CCDF(x) On Log-Log Scale") 
ggsave("plots/pareto-ccdf-log-log.png")

