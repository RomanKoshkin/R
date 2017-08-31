# This script compares different linear mixed effects models (e.g. fixed and random slopes)

library(languageR)
library(dplyr)
library(lme4)
subjects = c('KOK', 'GRU', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')
languages = c('er', 'RE')

for (estimator in c("CLred", "CWred", "SYL", "CLnored", "CWnored")){
print(paste("\n", estimator, sep=""))
df2 <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")
df2 <- df2[df2$subj != 'a', ] # cleanup data

  bounds <- c(0.1, 0.9)
  df2$load <- 0
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
  df2.meanc <- summarySE(df2, measurevar="mu_P1", groupvars=c("subj","lang", "load"))
  fml0 <- as.formula("mu_P1 ~ lang + load + (1|subj)")
  fml1 <- as.formula("mu_P1 ~ lang + load + (1+load+lang|subj)")
  mod0 = lmer(fml0, data=df2, REML=FALSE)
  mod1 = lmer(fml1, data=df2, REML=FALSE)
  print(anova(mod0, mod1))
}
