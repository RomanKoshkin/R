# THIS SCRIPT COMPUTES ANTERIORITY/LATERALITY ANOVA

# this code performs ANOVA using aov and the EZ package.
# The statistics, p-values, effect sizes are reported in the APA format


require(plyr)
require(Rmisc)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(grid)
require(cowplot)
require(nortest)
require(gplots)
require(RColorBrewer)
require(ez)
require(apa)
require(gdata)
cat("\014")

############## PARAMETERS #############################

# set the window of interest (mu_N1 or mu_P1):
win = "mu_P1"
DSfolder = "NGA3_0.25-30" #"NGA3_0.1-20-ICA" #"NGA3xxx"
estimatorVector <- c("CLred") # "CWred", "SYL", "CLnored", "CWnored"
subjects = c('KOK', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL', 'GRU')
languages = c('er', 'RE')
LO <- 0.1 #seq(0.05, 0.95, by = step)
HI <- 0.9 #seq(0.05, hiQ, by = step)
formula <- as.formula(paste(win," ~ lang * load * laterality * anteriority + Error(subj / (lang * load * laterality * anteriority))", sep=""))

# # GET RID of GRU:
# df2 <- subset(df2, subj != "GRU")
# # drop the level:
# df2 <- drop.levels(df2) 

######################################################

# read in the data:
n <- read.csv('n.csv')
n<-n[,2:4]

te <- read.csv(paste(DSfolder, "/dataframe_", estimatorVector,"_", n[1,1],"_", n[1,2],"_", n[1,3], '.csv', sep = ""))
te$anteriority <- NA
te$laterality <- NA
te <- te[0,]

for (i in seq(1,30)){
temp1 <- read.csv(paste(DSfolder, "/dataframe_", estimatorVector,"_", n[i,1], "_", n[i,2],"_", n[i,3], '.csv', sep = ""))
temp1$anteriority <- n$anteriority[i]
temp1$laterality <- n$laterality[i]

bounds = c(LO, HI)

# new recode WM loads based on the cutoffs specified:
for (estimator in estimatorVector){
  print(estimator)
  est <- temp1[[estimator]]
  temp1$load <- 0
  for (subject in subjects){
    for(language in languages){
      idx = c(temp1$subj==subject & temp1$lang==language)
      C <- quantile(est[idx], bounds) %>% as.numeric()
      temp1$load[idx & (est <= C[1])] <- 'low'
      temp1$load[idx & (est > C[2])] <- 'high'
      temp1$load[idx & (est > C[1]) & (est <= C[2])] <- 'mid'
    }
  }
}




te <-rbind(te, temp1)
print(i)
}
df2<-te
rm(te,n)

# ########################################################

df2.meanc <- summarySE(df2, measurevar=win, groupvars=c("subj","laterality","anteriority", "lang", "load"))
df2.aov <- aov(data=df2.meanc, formula)
df2.meanc$load <- as.factor(df2.meanc$load)

cat("\014")
print(summary(aov(data=df2.meanc, formula)))


ee <- ezANOVA(data=df2.meanc, dv=mu_P1,
              wid=.(subj), within=.(anteriority, laterality, lang, load), 
              between = NULL, type = 3, detailed = TRUE)

print(ee)
anova_apa(ee, es = c("pes"), format = "docx") # ges for general eta-squared

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
anova_1<-aov_ez(data=df2.meanc, id="subj", dv="mu_P1", within=c("lang", "load"))
resid <- anova_1$lm$residuals
qqnorm(resid, main="Normal Q-Q")
qqline(resid)
boxplot(resid)
