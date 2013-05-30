# Rscript is-distribution-qq.R data.csv
# Generate a Q-Q plot (also called probability plot). We generate the data in
# clojure and then plot it here. When we generate the data, we want to take the
# population (or sample), generate a sample of the same size for the
# distribution we are comparing, sort them, and then plot the distribution on
# the x-axis and the empirical data on the y-axis.
# 
# The data should be sorted and trimmed before written to the input file.
# Example:
# (def header ["x" "y"])
# (def data 
#   (map vector 
#       (stats/trim (random/sample (count population) (partial random/expovariate 0.001762)) 0.03 :left false) 
#       (stats/trim epic 0.03 :left false)))
# (util/write-to-csv "tmp-qq.csv" (conj data header))
# Rscript is-distribution-qq.R tmp-qq.csv
#
# Refereces:
# http://en.wikipedia.org/wiki/Q%E2%80%93Q_plot
# http://en.wikipedia.org/wiki/Rankit

require(ggplot2)
require(reshape2)

args <- commandArgs(TRUE)
df <- read.csv(args[1], header=TRUE)

# if this is exponential, get the slope and the x intercept
# model <- lm(y~x, data=df)
# summary(model)
# plot(model)

# the two data sets and the linear model. does it fit?
ggplot(data=df) +
    geom_line(aes(x=x, y=y), color="blue") +
    geom_line(aes(x=x, y=x), color="red", size=0.3) +
#    geom_smooth(method="lm", color="red", size=0.3) +
    labs(y="E Data Set", x="random variate", title="Q-Q Plot x=Random Variate, y=E Data Set")
ggsave("plots/is-distribution-qq.png")

