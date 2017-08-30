library(boot)
# we'll bootstrap from this sample to estimate the underlying distribution
x <- c(30, 37, 36, 43, 42, 43, 43, 46, 41, 42) 
Mean <- function(x, d) {return(mean(x[d]))} # d are the indexes that the boot function generates
results <- boot(data=x, statistic=Mean, R=1000)
boot.ci(results, type="bca", index=1)
plot(results)

bootSE <- sd(results$t)
bias <- mean(results$t)-results$t0
x_bar <- mean(x)
##################################################################
library(boot)
# we'll bootstrap from this sample to estimate the underlying distribution

r <- function(dat, d) {return(cor(dat[d,])[1,2]}
results <- boot(data=cbind(x,y), statistic=r, R=1000)
boot.ci(results, type="bca", index=1)
plot(results)

bootSE <- sd(results$t)
bias <- mean(results$t)-results$t0
x_bar <- mean(x)

######################
D <- cbind(x, y)
# compute CIs for correlation between mother's weight and birth weight
cor.boot <- function(data, k)  cor(data[k,1], data[c(1:length(x)),2])
cor.res <- boot(data=D, statistic=cor.boot, R=5000)
boot.ci(cor.res, type = "all")
