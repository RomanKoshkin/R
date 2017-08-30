## BOOTSTRAP SAMPLING:
# http://www.ats.ucla.edu/stat/r/library/bootstrap.htm

cat("\014")     # clear the console
rm(list=ls())   # clear the workspace

# parameters:
size = 10000
s = 1000 # number of resamples (i.e. number of samples we take from our randomly generated data. With replacement)
SD = 1
M = 0

data = round(rnorm(size,M,SD))
resamples <- lapply(1:s, function(i) sample(data, replace=T))
r.mean <- sapply(resamples, mean)
message("std of the mean = ", sqrt(var(r.mean)))
message("SEM = ", sqrt(var(r.mean)))
histinfo <- hist(r.mean, breaks=50, main="Dahistogram") # here we specify the number of bins and title

# browser()

source("bMean.R") # load the function inside bMean.R in the current directory
q <- b.mean(data,20)   # pass the data and the number of resamples into the function (that can be veiwed by typing b.mean in the console)
message(q$std.err)      # view specific lists within the q list
# or you can view the second element in list "means" in the list "q"
# message(q[["means"]][2])



