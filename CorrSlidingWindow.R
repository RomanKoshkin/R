library(languageR)
library(dplyr)
library(lme4)
subjects = c('KOK', 'GRU', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')
languages = c('er', 'RE')

estimator = "CLred"

df2 <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")
df2 <- df2[df2$subj != 'a', ] # cleanup data

for (i in seq(0, 0.9, by=0.1))
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
