# Rscript cdf.R data.csv "title" cdf.png
require(ggplot2)
require(reshape2)

# 
args <- commandArgs(TRUE)
data <- read.csv(args[1], header = TRUE)
#data.m <- melt(data, id="x",  measure = c("y"))
print(args)
ggplot(data, aes(x=x)) + 
    geom_line(aes(y=y, color="x")) + 
    scale_colour_manual("", breaks=c("y"), values=c("blue")) +
    labs(y="CDF", x="x", title=args[2]) 
ggsave(args[3])



