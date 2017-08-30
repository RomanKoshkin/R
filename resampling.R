library(moments)
Nres <- 100000
sample_size <- 30
resamples <- lapply(1:Nres, function(i) sample(c(0,1), sample_size, prob=c(0.8, 0.2), replace = TRUE))
c = sapply(resamples, sum)/sample_size
hist(c, breaks=20)
skewness(c)