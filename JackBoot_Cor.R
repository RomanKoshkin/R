cat("\014")
estimator <- c("CWred", "CLred", "SYL", "CWnored", "CLnored")
subjects = c('KOK', 'GRU', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')
languages = c('er', 'RE')

estimator = "CWred"
Step = 1
perm = 1000
LB = seq(1, 18, by=Step)
UB = LB + Step
y <- 0

df <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")
df2 <- df[df$subj != 'a', ] # cleanup data
for (LeftOutSubj in subjects){
  for (i in seq(1:length(LB))){
  addr <- which(df2$subj!=LeftOutSubj & df2[[estimator]]>=LB[i] & df2[[estimator]]<UB[i]) # 
   y[i] <- mean(df2$mu_N1[addr])
  }

  x <- LB[1:length(na.omit(y))]
  y <- na.omit(y)
  # x <- scale(x)
  # y <- scale(y)
  plot(x, y)
  q <- summary(lm(y~x))
  abline(lm(y~x), col="red")
  #lines(lowess(y~x), col="blue")
  r <- q$adj.r.squared
  print(paste("Adjusted R squared = ", round(r, digits = 4), "p = ", round(q$coefficients[8], digits = 3)))

  # compute bootstrapped p-value:
  r_boot <- 0
  for(j in seq(1,perm)){
  y_resampled <- sample(y,length(y), replace=T)
  q_boot <- summary(lm(y_resampled~x))
  r_boot[j] <- q_boot$adj.r.squared
  }
  print(paste("Left out", LeftOutSubj, ". Bootsrapped 0.95 CI for r:"))
  print(quantile(r_boot,c(0.025, 0.975)))
} # next LeftOutSubj
