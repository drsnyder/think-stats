# 
require(ggplot2)
require(reshape2)

# 
args <- commandArgs(TRUE)
data <- read.csv(args[1], header = TRUE)
data.m <- melt(data, id="prglength",  measure = c("first", "other", "live"))
ggplot(data.m, aes(prglength, value, fill=variable)) + 
    geom_bar(stat="identity", position="dodge", width=0.99) +
    scale_x_continuous(breaks=seq(min(data.m$prglength), max(data.m$prglength)))
ggsave(args[2])


