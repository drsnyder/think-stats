# http://www.gutenberg.org/files/1399/1399-0.txt 
require(ggplot2)
require(reshape2)

# 
data <- read.csv("data/ak-cdf.csv", header = TRUE)


# remove last element where Y=1, so we can plot the log
data <- head(data, -1)

# TODO: use this to add the alpha to the plot. we can assume that xm is 1
regression_xmin <- 200
regression_ymin <- 372
fit <- lm(log(1-y)~log(x), data=data, subset=seq(regression_xmin, regression_ymin))
print(fit)



# in the console
# plot(log(data$x), log(1-data$y))
# abline(lm(log(1-y)~log(x), data=data, subset=seq(200, 372)))

# i'm splicing out the straight part of the ccdf after looking at it
ggplot() +
    geom_line(aes(x=log(x), y=log(1-y)), data=data, color="blue") + 
    geom_smooth(aes(x=log(x), y=log(1-y)), method = "lm", data=data[180:372,], color="red", size=0.3) +
    labs(y="y", x="x", title="Anna Karenina Word Frequencies\nCCDF on Log-Log") 
ggsave("plots/ak-ccdf-log-log.png")




