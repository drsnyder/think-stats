# Generate a sample from a Weibull distribution and test a
# linear transformation.

# shape = k
shape = 2
# scale = lambda
scale = 1

weibull.rsample = sort(rweibull(1e5, shape, scale))
weibull.ecdf = (1:length(weibull.rsample))/length(weibull.rsample)
df = data.frame(cbind(x=weibull.rsample, y=weibull.ecdf))
df$xp = log(df$x)
df$yp = log(log(1/(1-df$y)))


# exclude y = 1
model = lm(df$yp ~ df$xp, subset=seq(1,length(df$yp)-1))
summary(model)

# -shape * log(scale) = ~0
intercept = coef(model)[1]
# shape ~ 2
slope = coef(model)[2]

