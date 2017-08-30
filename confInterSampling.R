library(statsr)
library(dplyr)
library(ggplot2)

set.seed(9102015)

data(ames)

# compute the true population mean first:
mu = mean(ames$area)

n <- 60 # sample size
NumberOfSamples = 100 # how many times to perform the sampling and CI calculation and determining 
                        # if the CI contains the true population mean mu
samp <- sample_n(ames, n)

z_star_95 <- qnorm(0.975)

y <- 0
y <- as.numeric(summarise(samp, lower = mean(area) - z_star_95 * (sd(area) / sqrt(n)),
            upper = mean(area) + z_star_95 * (sd(area) / sqrt(n))))

x <- 0
for (i in 1:NumberOfSamples){
  samp <- sample_n(ames, n)
  y <- as.numeric(summarise(samp, lower = mean(area) - z_star_95 * (sd(area) / sqrt(n)),
                                  upper = mean(area) + z_star_95 * (sd(area) / sqrt(n))))
  x[i] <- y[1]< mu & mu < y[2]
}
prop <- length(subset(x, x==TRUE))/NumberOfSamples
print('number of samples in which the confidence interval contains the true populaiton mean', str(prop))