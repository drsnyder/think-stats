# 
require(ggplot2)
require(reshape2)

# 
args <- commandArgs(TRUE)
data <- read.csv(args[1], header = TRUE)
data.m <- melt(data, id="weight",  measure = c("survey", "sample"))
ggplot(data, aes(x=weight)) + 
    geom_line(aes(y=survey, color="survey")) + 
    geom_line(aes(y=sample, color="sample")) + 
    scale_colour_manual("", breaks=c("survey", "sample"), values=c("red","blue")) +
    labs(y="Probability", x="Weight", title="") 
ggsave(args[2])



