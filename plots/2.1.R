# 
require("ggplot2")

args <- commandArgs(TRUE)
preglen <- read.csv(args[1], header = TRUE)
hist_prglen <- ggplot(preglen, aes(x=prglength,fill=birthord))
hist_prglen + geom_bar(position="dodge")
ggsave("plots/2.1.png")
