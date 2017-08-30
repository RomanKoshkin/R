# This code plots ERP amplitudes against WM loads. Jackknifing is used to plot different curves

cat("\014")
library(dplyr)
library(gridExtra)
library(grid)
library(cowplot)
library(nortest)
library(gplots)
cat("\014")
############## PARAMETERS #############################
K = 10
par(mfrow=c(2,2))
estimator <- "CLred"
subjects = c('KOK', 'ELT', 'POG', 'KOS', 'ROM', 'SHE', 'BUL', 'KOZ')
L = list(subjects)
languages = c('er', 'RE')

# load data:
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
      df2$load[idx & df2[[estimator]] >= C1[j] & df2[[estimator]] < C2[j] ] <- toString(j)
    }
  }
}

for (win in c("mu_N1", "mu_P1")){
er <- 0 
RE <- 0
for (j in seq(1,K)){er[j] <- mean(df2[df2$load==toString(j) & df2$lang=='er', win])}
for (j in seq(1,K)){RE[j] <- mean(df2[df2$load==toString(j) & df2$lang=='RE', win])}

# plot(er, xlab="WM load")
# title("English-Russian")
# lines(er, type="b", lwd=1.5)
# plot(RE, xlab="WM load")
# title("Russian-English")
# lines(RE, type="b", lwd=1.5)

# initialize an empty matrix:
er <- matrix(nrow=K, ncol=length(subjects))
colnames(er) <- subjects

# implement jackknifing:
for (q in 1:length(subjects)){
  for (j in seq(1,K)){er[j,q] <- mean(df2[df2$load==toString(j) & df2$lang=='er' & df2$subj!=subjects[q], win])}
}

# get the range for the x and y axis 
xrange <- range(0:K+1) 
yrange <- range(er) 

# set up the plot:
plot(xrange, yrange, type="n", ylab="N1 mean amplitude", xlab="WM load" )
title(paste (win, "English-Russian"))
colors <- rainbow(length(subjects)) 
linetype <- c(1:length(subjects)) 
plotchar <- seq(18,18+length(subjects),1)

# add lines:
for (i in 1:length(subjects)){lines(er[,i], type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i])}

# add a legend 
legend('bottomright', subjects, cex=0.4, col=colors,
       pch=plotchar, lty=linetype, title='Left\nout')

######################### now the same thing for RE ######################################
# initialize an empty matrix:
RE <- matrix(nrow=K, ncol=length(subjects))
colnames(RE) <- subjects

# implement jackknifing:
for (q in 1:length(subjects)){
  for (j in seq(1,K)){RE[j,q] <- mean(df2[df2$load==toString(j) & df2$lang=='RE' & df2$subj!=subjects[q], win])}
}

# get the range for the x and y axis 
xrange <- range(0:K+1) 
yrange <- range(RE) 

# set up the plot:
plot(xrange, yrange, type="n", ylab="N1 mean amplitude", xlab="WM load" ) 
title(paste (win, "Russian-English"))
colors <- rainbow(length(subjects)) 
linetype <- c(1:length(subjects)) 
plotchar <- seq(18,18+length(subjects),1)

# add lines:
for (i in 1:length(subjects)){lines(RE[,i], type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i])}

# add a legend 
legend('bottomright', subjects, cex=0.4, col=colors,
       pch=plotchar, lty=linetype, title='Left\nout')
}

  