# DESCRIPTION AND JUSTIFICATION IS HERE: http://www.uvm.edu/~dhowell/StatPages/Permutation%20Anova/PermTestsAnova.html
#This code performs a randomization/bootstrap test:
library(Rmisc)
#parameters:
loQ = 0.10
hiQ = 0.90
tp = 0  # 1 - Randomize separately the means within-factor (wihin-load and within-lang), 
        # 0 - randomize indiscriminately (Manly's approach)
random = 1000 # how many times to resample
TypeFlag = F # F - randomization, T - bootstrap
win = "mu_P1"
estimator <- "CLred"
subjects = c('KOK', 'GRU', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')
formula <- as.formula(paste(win," ~ lang * load + Error(subj / (lang * load))", sep=""))

# internals:
bounds = c(loQ, hiQ)
languages = c('er', 'RE')
df <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")
P <- read.table('P.txt')
df2 <- df[df$subj != 'a', ] # cleanup data

est <- df2[[estimator]]
for (subject in subjects){
  for(language in languages){
    idx = c(df2$subj==subject & df2$lang==language)
    C <- quantile(est[idx], bounds) %>% as.numeric()
    df2$load[idx & (est <= C[1])] <- 'low'
    df2$load[idx & (est > C[2])] <- 'high'
    df2$load[idx & (est > C[1]) & (est <= C[2])] <- 'mid'
  }
}

df2.meanc <- summarySE(df2, measurevar=win, groupvars=c("subj","lang", "load"))
df2.aov <- aov(formula, data=df2.meanc)
lang_main = unlist(summary(df2.aov)[[2]])[[9]]
load_main = unlist(summary(df2.aov)[[3]])[[9]]
langXload = unlist(summary(df2.aov)[[4]])[[9]]
F_lang_main = unlist(summary(df2.aov)[[2]])[[7]]
F_load_main = unlist(summary(df2.aov)[[3]])[[7]]
F_langXload = unlist(summary(df2.aov)[[4]])[[7]]

# this code performs a randomization version of ANOVA

# RANDOMIZATION TEST / BOOTSTRAP :
vec <- df2[[win]]
F_lang_mainR = 0
F_load_mainR = 0
F_langXloadR = 0
pb <- txtProgressBar(min = 0, max = random, style = 3)
for (i in 1:random){
if (tp == 0){
  df2[[win]] <- sample(vec, nrow(df2), replace = TypeFlag)
  df2.sum <- summarySE(df2, measurevar=win, groupvars=c("subj","lang", "load"))
  }
else{
  df2.sum <- summarySE(df2, measurevar=win, groupvars=c("subj","lang", "load"))
  
  # randomize the measurements within "load":
  for (s in subjects){
    for (l in languages){
      idx <- which(df2.sum$subj==s & df2.sum$lang==l)
      df2.sum[[win]][idx] <- df2.sum[[win]][sample(idx, length(idx), replace = F)]
    }
  }
  # randomize the measurements within "language":
  # mapping function (internal)
  fun1 <- function(x) {
    x[x==1] <- "er"
    x[x==0] <- "RE"
    x
  }
  for (s in subjects){
    df2.sum$lang[which(df2.sum$subj==s)] <-
      fun1(abs((df2.sum$lang[which(df2.sum$subj==s)]=='er') - rbinom(1, 1, 0.5)))
  }
}
  
# perform the ANOVA on the randomized sample:
df2.aov <- aov(formula, data=df2.sum)

F_lang_mainR[i] = unlist(summary(df2.aov)[[2]])[[7]]
F_load_mainR[i] = unlist(summary(df2.aov)[[3]])[[7]]
F_langXloadR[i] = unlist(summary(df2.aov)[[4]])[[7]]
setTxtProgressBar(pb, i)
}

p_loadR <- sum(F_load_mainR >= F_load_main)/random
p_langR <- sum(F_lang_mainR >= F_lang_main)/random
p_langXloadR <- sum(F_langXloadR >= F_langXload)/random

print(paste("RANDOMIZATION PROCEDURE:", tp))
print(paste("Bootstrapping (applies to type 0 only)", TypeFlag))
print(paste("original pvalues:", "load", load_main, "lang", lang_main, "load:lang", langXload))
print(paste("randomization pvalues:","load", p_loadR, "lang", p_langR, "load:lang", p_langXloadR))
