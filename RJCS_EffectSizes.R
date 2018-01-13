# this code performs ANOVA using aov and the EZ package.
# run MYdataAnalysis.R FIRST!!!
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
library(gdata)
cat("\014")

######################################################
#clusters = c("front", "back", "left", "right")
clusters = "select"

if (clusters!="select"){
  # read in the dataframes under the given names:
  for (i in clusters){
    assign(paste("df", i, sep="_"), 
           read.csv(paste("/Users/RomanKoshkin/Documents/R/dataframe_", i, ".csv", sep="")))
    x = nrow(get(paste("df", i, sep="_")))
  }
  
  # put the dataframes into a list of dataframes (for binding later):
  datalist<-lapply(1:length(clusters), function(x) get(paste("df_", clusters[x], sep="")))
  
  #bind the dataframe from fragments:
  df2 = do.call(rbind, datalist)
  
  # remove the temp dataframes:
  do.call(rm,
          lapply(1:length(clusters), function(x) paste("df_", clusters[x], sep=""))
          )
  
  df2$load <- as.factor(df2$tmplab)
  
  l = nrow(df2)/length(clusters)
  for (i in seq(length(clusters))){
    k = 1+x*(i-1)
    m = l+l*(i-1)
    df2$chan[k:m] <- clusters[i]
  }
  } else{
    df2 <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_select.csv")
    df2$load <- as.factor(df2$tmplab)
  }

############## PARAMETERS #############################


# set the window of interest (mu_N1 or mu_P1):
win = "mu_P1"

# GET RID of GRU:
df2 <- subset(df2, subj != "GRU")
# drop the level:
df2 <- drop.levels(df2) 

# define the model formula:
if (clusters!="select"){
  formula <- as.formula(paste(win," ~ lang * load * chan + Error(subj / (lang * load * chan))", sep=""))
  } else {formula <- as.formula(paste(win," ~ lang * load + Error(subj / (lang * load))", sep=""))}

# way to estimate WM load:
estimatorVector <- c("CLred") # "CWred", "SYL", "CLnored", "CWnored"

# subjects = c('GRU', 'KOK', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')
subjects = c('KOK', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL', 'GRU')
languages = c('er', 'RE')


# # read in the data:
# #df <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")
# df <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_Cz.csv")
# 
# # get rid of the subject we don't want to consider:
# df <- df[df$subj %in% subjects,]
# 
# LO <- 0.1 #seq(0.05, 0.95, by = step)
# HI <- 0.9 #seq(0.05, hiQ, by = step)
# 
# ########################################################
# bounds = c(LO, HI)
# 
# # new recode WM loads based on the cutoffs specified:
for (estimator in estimatorVector){
#     print(estimator)
#     df2 <- df[df$subj != 'a', ] # cleanup data
#     est <- df2[[estimator]]
#     df2$load <- 0
#     for (subject in subjects){
#       for(language in languages){
#         idx = c(df2$subj==subject & df2$lang==language)
#         C <- quantile(est[idx], bounds) %>% as.numeric()
#         df2$load[idx & (est <= C[1])] <- 'low'
#         df2$load[idx & (est > C[2])] <- 'high'
#         df2$load[idx & (est > C[1]) & (est <= C[2])] <- 'mid'
#       }
#     }
    
    df2$load <- as.factor(df2$load)
    if (clusters!="select"){
      df2.meanc <- summarySE(df2, measurevar=win, groupvars=c("subj","chan", "lang", "load"))
    } else {df2.meanc <- summarySE(df2, measurevar=win, groupvars=c("subj", "lang", "load"))}
    df2.aov <- aov(data=df2.meanc, formula)
    df2.meanc$load <- as.factor(df2.meanc$load)
}


cat("\014")
print(summary(aov(data=df2.meanc, formula)))


if (clusters!="select"){
ee <- ezANOVA(data=df2.meanc, dv=mu_P1,
              wid=.(subj), within=.(chan, lang, load), 
              between = NULL, type = 3, detailed = TRUE)
} else {
  ee <- ezANOVA(data=df2.meanc, dv=mu_P1,
                wid=.(subj), within=.(lang, load), 
                between = NULL, type = 3, detailed = TRUE)
}

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
