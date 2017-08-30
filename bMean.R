b.mean <- function(data, num) {
  resamples <- lapply(1:num, function(i) sample(data, replace=T))
  r.mean <- sapply(resamples, mean)
  std.err <- sqrt(var(r.mean))
  list(std.err=std.err, resamples=resamples, means=r.mean)
}
