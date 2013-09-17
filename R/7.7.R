#
first.mean = 38.60095
first.sd = 2.791901
first.count = 4413

other.mean = 38.51326
other.sd = 2.634525
other.count = 4735

z = replicate(10^5, {
  first = rnorm(first.count, mean=first.mean, sd=first.sd)
  other = rnorm(other.count, mean=other.mean, sd=other.sd)
  t.test(first, other)$p.value
})
length(z[z < 0.05])/length(z) # mean(z < 0.05)
