# Rscript h-post-exponential.R data.csv
# byte length of each
require(ggplot2)
require(reshape2)

args <- commandArgs(TRUE)
data <- scan(args[1])
data <- sort(data)

# strip the outliers; builtin for doing this?
valid <- data[1:(length(data)*0.96)]

# construct the empirical CDF
ecdf <- (1:length(valid))/length(valid)
df <- data.frame(cbind(x=valid, y=ecdf))
# if this is exponential, get the slope and the x intercept
lm(log(1-y)~x, data=df, subset=seq(1,length(valid)-1))

# plot the ccdf log(y) and the linear model. does it fit?
ggplot(aes(x=x, y=log(1-y)), data=df[1:length(valid)-1,]) + 
    geom_line(color="blue") +
    geom_smooth(method="lm", color="red", size=0.3) +
    labs(x="Data", y="CCDF of Data Log(y)", title="Line of Best Fit on Log(CCDF) of Data")
ggsave("plots/h-post-exponential.png")
