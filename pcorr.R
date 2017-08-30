# BOOTSTRAPPED PARTIAL CORRELATION:
#stolen from https://stat.ethz.ch/pipermail/r-help/2009-March/193523.htmlx

rm(list=ls())   # clear the workspace
cat("\014")     # clear the console

partial.cor <- function (X, ...) 
{
  R <- cor(X, ...)
  RI <- solve(R)
  D <- 1/sqrt(diag(RI))
  R <- -RI * (D %o% D)
  diag(R) <- 0
  rownames(R) <- colnames(R) <- colnames(X)
  R
}

#Create some fake data set
#myData <- data.frame(xPre = rnorm(100), xPost = rnorm (100), SAT = rnorm(100))

db = file.choose()
myData = read.spss(db, to.data.frame=TRUE)

#Partial correlation
partial.cor(myData)

#creat bootstrap sample
R <- 8500 #number of replications
#Bootstrap samples
boots<- list()
for (i in 1:R){
  boots[[i]] <- as.data.frame(myData[sample(1:nrow(myData),nrow(myData), replace=TRUE),])
}

#Bootstrap responses for xPost SAT partial correlation ONLY
#change index [3,2] if you want a different one.
boot.partial <- sapply(boots, function(x) partial.cor(x)[3,2])
hist(boot.partial)
w <- quantile(boot.partial, probs=c(.025, .975))