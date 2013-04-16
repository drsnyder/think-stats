# 
require(ggplot2)
require(reshape2)

# 
args <- commandArgs(TRUE)
data <- read.csv(args[1], header = TRUE)
data.m <- melt(data, id="speed",  measure = c("cdf.x."))
ggplot(data.m, aes(speed, value, fill=variable)) + 
    geom_line() + labs(y="CDF(x)", x="Running Speed", title="") 
ggsave(args[2])



