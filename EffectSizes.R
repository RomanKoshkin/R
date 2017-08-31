# this code performs ANOVA using aov and the EZ package.
# The statistics, p-values, effect sizes are reported in the APA format
# UNLIKE effSizeEZ_multiple.R, it does not perform multiple testing.

library(plyr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(cowplot)
library(nortest)
library(gplots)
library(RColorBrewer)
library(ez)
library(apa)
cat("\014")
############## PARAMETERS #############################

# set the window of interest (mu_N1 or mu_P1):
win = "mu_N1"

# define the model formula:
formula <- as.formula(paste(win," ~ lang * load + Error(subj / (lang * load))", sep=""))

# way to estimate WM load:
estimatorVector <- c("CLred") # "CWred", "SYL", "CLnored", "CWnored"

subjects = c('KOK', 'GRU', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')
languages = c('er', 'RE')

# read in the data:
df <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")

LO <- 0.1 #seq(0.05, 0.95, by = step)
HI <- 0.9 #seq(0.05, hiQ, by = step)

########################################################
bounds = c(LO, HI)

# new recode WM loads based on the cutoffs specified:
for (estimator in estimatorVector){
    print(estimator)
    df2 <- df[df$subj != 'a', ] # cleanup data
    est <- df2[[estimator]]
    df2$load <- 0
    for (subject in subjects){
      for(language in languages){
        idx = c(df2$subj==subject & df2$lang==language)
        C <- quantile(est[idx], bounds) %>% as.numeric()
        df2$load[idx & (est <= C[1])] <- 'low'
        df2$load[idx & (est > C[2])] <- 'high'
        df2$load[idx & (est > C[1]) & (est <= C[2])] <- 'mid'
      }
    }
    
    df2$load <- as.factor(df2$load)
    df2.meanc <- summarySE(df2, measurevar=win, groupvars=c("subj","lang", "load"))
    df2.aov <- aov(data=df2.meanc, formula)
    df2.meanc$load <- as.factor(df2.meanc$load)
}


cat("\014")
print(summary(aov(data=df2.meanc, formula)))
ee <- ezANOVA(data=df2.meanc, dv=mu_N1,
              wid=.(subj), within=.(lang, load), 
              between = NULL, type = 3, detailed = TRUE)
print(ee)
anova_apa(ee, es = c("pes", format = "docx" )) # ges for general eta-squared
stop('CODE STOPPED')
###################### NOW LET'S CHECK THE RESIDUALS ##############
# https://stats.stackexchange.com/questions/6081/testing-the-normality-assumption-for-repeated-measures-anova-in-r
# I don't know why, but this method gives completely crazy residuals:
aov.out.pr <- proj(df2.aov) # project the model
df2.meanc$resi <- aov.out.pr[[4]][, "Residuals"]
qqnorm(df2.meanc$resi, main="Normal Q-Q")
qqline(df2.meanc$resi)
boxplot(resi ~ interaction(lang, load), main="Homoscedasticity", 
        xlab = "Code Categories", ylab = "Residuals", data=df2.meanc)

# or like this (better, because the residuals are VERY close to SPSS results)
# http://stackoverflow.com/questions/30238213/ggplot2-residuals-with-ezanova
library(afex)
anova_1<-aov_ez(data=df2.meanc, id="subj", dv="mu_N1", within=c("lang", "load"))
resid <- anova_1$lm$residuals
qqnorm(resid, main="Normal Q-Q")
qqline(resid)
boxplot(resid)
