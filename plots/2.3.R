# 
require(ggplot2)
require(reshape2)

# with hist-data
args <- commandArgs(TRUE)
preglen <- read.csv(args[1], header = TRUE)
preglen.m <- melt(preglen, id="prglength", variable.name = "order", measure = c("difference"))
ggplot(preglen.m, aes(prglength, value)) + geom_bar(stat="identity", position="dodge", width=0.99) +
    labs(x="Length in Weeks", y="100*(PMF(first) - PMF(others))", title="Difference in PMFs") 
ggsave(args[2])

