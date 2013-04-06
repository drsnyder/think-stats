
# 
require(ggplot2)
require(reshape2)

# 
args <- commandArgs(TRUE)
data <- read.csv(args[1], header = TRUE)
data.m <- melt(data, id="size",  measure = c("sampled", "unbiased"))
ggplot(data.m, aes(size, value, fill=variable)) + 
    geom_bar(stat="identity", position="dodge") 
ggsave(args[2])


