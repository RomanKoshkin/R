# once you put a vector through a softmax function, all the transformed values sum to one.
softmax <- function(x){
  sigma <- 0
  for(i in seq(1:length(x))){
    sigma[i] <- exp(x[i])/sum(exp(x))
    } 
    return(sigma)}
# once you put a vector through a softmax function, 
# all the transformed values sum to the mean of min+max
minmax <- function(x){
  z <- 0
  q <- max(x)-min(x)
  for(i in seq(1:length(x))){
    z[i] <- (x[i] - min(x))/q}
    return(z)}