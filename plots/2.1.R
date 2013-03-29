# 
require(ggplot2)
require(reshape2)

# letting R do the hist
# args <- commandArgs(TRUE)
# preglen <- read.csv(args[1], header = TRUE)
# hist_prglen <- ggplot(preglen, aes(x=prglength,fill=birthord))
# hist_prglen + geom_bar(position="dodge")
# ggsave("plots/2.1.png")


# with hist-data
args <- commandArgs(TRUE)
preglen <- read.csv(args[1], header = TRUE)
preglen.m <- melt(preglen, id="prglength", measure = c("first", "others"))
ggplot(preglen.m, aes(prglength, value, color=variable)) + geom_bar(stat="identity", position="dodge")
ggsave("plots/2.1.png")
