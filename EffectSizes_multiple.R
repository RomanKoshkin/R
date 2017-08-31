# use this code to generate effect size HEATMAPS for the thesis/paper
# this code creates heatmaps for anova P-values and means as a function of condition cutoff
# quantiles. The p-values are for ANOVA
# you can also CHECK THE RESIDUALS and export an ANOVA table to Word
# See also compareERP.R (similar code).

cat("\014")
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

subjects = c('KOK', 'GRU', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')
languages = c('er', 'RE')

# read in the data:
df <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")

bounds = c(0.05, 0.95)  # quantilies
step = 0.1              # step size

# we'll need this to monitor the progress:
total = (length(seq(bounds[1], bounds[2], by = step)))^2/2

# set the window of interest (mu_N1 or mu_P1):
win = "mu_N1"

# define the model formula:
formula <- as.formula(paste(win," ~ lang * load + Error(subj / (lang * load))", sep=""))

# ways to estimate WM load:
estimatorVector <- c("CLred", "CWred", "SYL", "CLnored", "CWnored")

# that's for creating the plots that can be pasted into the paper:
newlabels <- c( "CLall", "CWall", "SYL", "CW", "CL")

jj <- 0
plot1 <- list()
plot2 <- list()
plot3 <- list()
plot4 <- list()
plot5 <- list()
plot6 <- list()
plot7 <- list()
plot8 <- list()
###############################################################

HI <- seq(0.05, 0.95, by = step)

for (estimator in estimatorVector){
print(estimator)
jj = jj + 1
i = 0
P <- read.table('P.txt')
pb <- txtProgressBar(min = 0, max = total, style = 3)
df2 <- df[df$subj != 'a', ] # cleanup data

est <- df2[[estimator]]
for (hiQ in HI){
  LO <- seq(0.05, hiQ, by = step)
  for(loQ in LO){
    bounds = c(loQ, hiQ)
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
    
    lang_main = 0
    load_main = 0
    langXload = 0
    P_mixed = 0
    idxLOWer <- df2$load=='low' & df2$lang=='er'
    idxHIer <- df2$load=='high' & df2$lang=='er'
    idxLOWRE <- df2$load=='low' & df2$lang=='RE'
    idxHIRE <- df2$load=='high' & df2$lang=='RE'
    effSizeN1er = mean(df2[idxLOWer, 'mu_N1']) - mean(df2[idxHIer, 'mu_N1'])
    effSizeN1RE = mean(df2[idxLOWRE, 'mu_N1']) - mean(df2[idxHIRE, 'mu_N1'])
    effSizeP1er = mean(df2[idxLOWer, 'mu_P1']) - mean(df2[idxHIer, 'mu_P1'])
    effSizeP1RE = mean(df2[idxLOWRE, 'mu_P1']) - mean(df2[idxHIRE, 'mu_P1'])

    
    lang_main = unlist(summary(df2.aov)[[2]])[[9]]
    load_main = unlist(summary(df2.aov)[[3]])[[9]]
    langXload = unlist(summary(df2.aov)[[4]])[[9]]
    P <- rbind(P, c(loQ, hiQ, lang_main, load_main, langXload, P_mixed, effSizeN1er, effSizeN1RE, effSizeP1er, effSizeP1RE))
    i = i + 1
    setTxtProgressBar(pb, i)
  }
}

Them2 <- theme(axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text = element_text(size = 6),
               legend.text = element_text(size = 6),
               legend.title = element_text(size = 6))
plot5[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
  geom_tile(aes(fill = effSizeN1er)) +
  ggtitle(paste("N1, Eng-Rus", newlabels[jj])) +
  scale_x_continuous(name = "Higher cutoff boundary (quantile)") +
  scale_y_continuous(name = "Lower cutoff boundary (quantile)") +
  scale_fill_gradientn(limits=c(min(P$effSizeN1er[-c(1)]),
                                max(P$effSizeN1er[-c(1)])), 
                       colours = rainbow(5), name=expression(paste(mu, "V"))) + Them2
plot6[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
  geom_tile(aes(fill = effSizeN1RE)) +
  ggtitle(paste("N1, Rus-Eng", newlabels[jj])) +
  scale_x_continuous(name = "Higher cutoff boundary (quantile)") +
  scale_y_continuous(name = "Lower cutoff boundary (quantile)") +
  scale_fill_gradientn(limits=c(min(P$effSizeN1RE[-c(1)]),
                                max(P$effSizeN1RE[-c(1)])),
                       colours = rainbow(5), name=expression(paste(mu, "V"))) + Them2
plot7[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
  geom_tile(aes(fill = effSizeP1er)) +
  ggtitle(paste("P1, Eng-Rus", newlabels[jj])) +
  scale_x_continuous(name = "Higher cutoff boundary (quantile)") +
  scale_y_continuous(name = "Lower cutoff boundary (quantile)") +
  scale_fill_gradientn(limits=c(min(P$effSizeP1er[-c(1)]),
                                max(P$effSizeP1er[-c(1)])), 
                       colours = rainbow(5), name=expression(paste(mu, "V"))) + Them2
plot8[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
  geom_tile(aes(fill = effSizeP1RE)) +
  ggtitle(paste("P1, Rus-Eng", newlabels[jj])) +
  scale_x_continuous(name = "Higher cutoff boundary (quantile)") +
  scale_y_continuous(name = "Lower cutoff boundary (quantile)") +
  scale_fill_gradientn(limits=c(min(P$effSizeP1RE[-c(1)]),
                                max(P$effSizeP1RE[-c(1)])), 
                       colours = rainbow(5), name=expression(paste(mu, "V"))) + Them2


b <- c(0, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.6, 0.8)
clrs <- c("black","purple", "navyblue","blue", "darkmagenta","green", "yellow", "darkorange", "red")
tit <- paste(win, newlabels[jj])
Them <- theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), 
              axis.title.y=element_blank(), axis.ticks.y=element_blank(), legend.position="none")
plot1[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) + geom_raster(aes(fill = load_main)) + ggtitle(paste(tit, "Load")) + scale_fill_gradientn(colours=clrs,breaks=b) + Them
plot2[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) + geom_raster(aes(fill = lang_main)) + ggtitle(paste(tit, "S.Lang.")) + scale_fill_gradientn(colours=clrs,breaks=b) + Them
plot3[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) + geom_raster(aes(fill = langXload)) + ggtitle(paste(tit, "Load x S.Lang.")) + scale_fill_gradientn(colours=clrs,breaks=b) + Them

}

# plot p-values (exploratory):
# qq <- list()
# qq <- rbind(plot1,plot2, plot3)
# plot_grid(plotlist = qq, nrow = 5, ncol = 3, align = "v")

# plot effect sizes:
ee <- list()
ee <- rbind(plot5, plot6, plot7, plot8)
plot_grid(plotlist = ee, nrow = 5, ncol = 4, align = "v")
stop("that's it for now")


########################## SOME FRILLS THAT YOU MOST LIKELY DON'T NEED #######################
cat("\014")
#print(summary(aov(data=df2.meanc, formula)))
ee <- ezANOVA(data=df2.meanc, dv=mu_N1,
              wid=.(subj), within=.(lang, load), 
              between = NULL, type = 3, detailed = TRUE)
print(ee)
anova_apa(ee, es = c("pes"), format = "docx") # ges for general eta-squared, format = "docx"

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
