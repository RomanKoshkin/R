cat("\014")

subjects = c('XXX', 'KOK', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')#, 'GRU')
languages = c('er', 'RE')
K = 150
estimator = "CLred"
perm = 1000

y <- 0
df <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")
df2 <- df[df$subj != 'a', ] # cleanup data

# compute edges
bounds1 <- seq(0, 1-1/K, length.out=K)
bounds2 <- seq(0+1/K, 1, length.out=K)

# recompute suject- and language-specific WM load labels:
df2$load <- 0
for (subject in subjects){
  for(language in languages){
    
    idx = c(df2$subj==subject & df2$lang==language)
    
    C1 <- quantile(df2[[estimator]][idx], bounds1) %>% as.numeric()
    C2 <- quantile(df2[[estimator]][idx], bounds2) %>% as.numeric()
    
    for(j in seq(1,K)){
      df2$load[idx & 
                 as.numeric(df2[[estimator]]) >= C1[j] &
                 as.numeric(df2[[estimator]]) < C2[j] ] <- toString(j)
    }
  }
}
estimator <- "load" # <---------<-!!<-!!!!!!!!!!!ВНИМАНИЕ!!!!!!!!!!!!!!!!!!!!!!!!!

LB <- seq(0:(max(as.numeric(df2$load))-1))
UB <- LB + 1

for (LeftOutSubj in subjects){
  for (i in seq(1:length(LB))){
    addr <- which(df2$subj!=LeftOutSubj &
                    df2[[estimator]]>=LB[i] &
                    df2[[estimator]]<UB[i]) 
    y[i] <- mean(df2$mu_N1[addr])
  }
  
  y <- na.omit(y)
  x <- LB[1:length(y)]
  # x <- scale(x)
  # y <- scale(y)
  plot(x, y)
  q <- summary(lm(y~x))
  abline(lm(y~x), col="red")
  #lines(lowess(y~x), col="blue")
  r <- q$adj.r.squared
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("Adjusted R squared = ", round(r, digits = 4), "p = ", round(q$coefficients[8], digits = 3)))
  print(paste("Pearson R =",cor(x,y)))
  
  # compute bootstrapped p-value:
  r_boot <- 0
  for(j in seq(1,perm)){
    idx <- sample(c(1:length(y)), length(y), replace = T)
    y_resampled <- y[idx]
    x_resampled <- x[idx]
    # q_boot <- summary(lm(y_resampled~x))
    # r_boot[j] <- q_boot$adj.r.squared
    r_boot[j] <- cor(x_resampled, y_resampled)
  }
  # compute using the boot package:
  D <- cbind(x, y)
  # compute CIs for correlation between mother's weight and birth weight
  cor.boot <- function(data, k)  cor(data[k,1], data[k,2])
  cor.res <- boot(data=D, statistic=cor.boot, R=perm)
  w <- boot.ci(cor.res, type = "all")
  
  print(paste("Left out", LeftOutSubj, ". Bootsrapped 0.95 CI for r:"))
  print(quantile(r_boot,c(0.025, 0.975)))
  
  print(paste("BCa:", w$bca[4], w$bca[5], "   Percentile: ", w$percent[4], w$percent[5]))
} # next LeftOutSubj
