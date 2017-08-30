nTrials = 27
p <- as.numeric(NULL)
pSuccess = 1/12
for (x in seq(0, n, by=1)){
  p <- append (p, (
    factorial(nTrials)/(factorial(x)*factorial(nTrials-x))) *
                 pSuccess^x * (1-pSuccess)^(nTrials-x))
}
barplot(p)
print(sum(p[4:28]))