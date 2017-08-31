# this code plots heatmaps of P-values (obtained from ANOVA and LMEM tests) 
# as well as voltage means for P1 and N1 as a function of condition cutoff
# quantiles.
# SEE ALSO EffectSizes_multiple.R

cat("\014")
library(plyr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(cowplot)
library(jtrans)
library(nortest)
library(gplots)
library(RColorBrewer)
library(lme4)
cat("\014")
############## PARAMETERS #############################
transform = 0
plotting = 1
bounds = c(0.05, 0.95)
step = 0.05
total = (length(seq(bounds[1], bounds[2], by = step)))^2/2
i = 0
win = "mu_P1"
formula <- as.formula(paste(win," ~ lang * load + Error(subj / (lang * load))", sep=""))
lmeformula_null <- as.formula(paste(win, "~ lang + (1|subj)")) 
lmeformula_alt <- as.formula(paste(win, "~ lang + load + (1|subj)")) 

estimator <- "CLred"
###############################################################

subjects = c('KOK', 'GRU', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')
languages = c('er', 'RE')
df <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")
P <- read.table('P.txt')

df2 <- df[df$subj != 'a', ] # cleanup data
## some shenanigans with the data:
# qplot(xtrans$transformed, geom = "histogram", bins = 80)
# if you want to transform the data to normality:
if (transform ==1){
  df2[[win]] <- jtrans(df2[[win]], test = 'ad.test', exclude_original = TRUE, z_lim = c(0.25, 1.25))
  df2[[estimator]] <- jtrans(df2[[estimator]], test = 'ad.test', exclude_original = TRUE, z_lim = c(0.25, 1.25))
  }
pb <- txtProgressBar(min = 0, max = total, style = 3)
est <- df2[[estimator]]
for (hiQ in seq(0.05, 0.95, by = step)){
  
  for(loQ in seq(0.05, hiQ, by = step)){
    bounds = c(loQ, hiQ)
    # cutoffs = c(0.00, 1.0) # anything beyond those quantiles will be ignored as outliers
    df2$load <- 0
    for (subject in subjects){
      for(language in languages){
        
        idx = c(df2$subj==subject & df2$lang==language)
        
        C <- quantile(df2[[estimator]][idx], bounds) %>% as.numeric()
        # L <- quantile(df2[[estimator]][idx], cutoffs) %>% as.numeric()
        
        df2$load[idx & (est <= C[1])] <- 'low'
        df2$load[idx & (est > C[2])] <- 'high'
        df2$load[idx & (est > C[1]) & (est <= C[2])] <- 'mid'
      }
    }
    
    df2 <- subset(df2, is.na(df2$load)==FALSE)
    
    df2.meanc <- summarySE(df2, measurevar=win, groupvars=c("subj","lang", "load"))
    
      # LINEAR MODEL (REPEATED MEASURES ANOVA)
      df2.aov <- aov(data = df2.meanc, formula)
      # MIXED EFFECTS MODEL
      mod.null = lmer(lmeformula_null, data=df2, REML=FALSE)
      mod.alt = lmer(lmeformula_alt, data=df2, REML=FALSE)
      LR <- anova(mod.null, mod.alt)    
      P_mixed <- LR$`Pr(>Chisq)`[2]
    
    if (plotting == 1){
      df2.meanc$load <- factor(df2.meanc$load, levels = c("high", "mid", "low")) # order the factors appropriately
      pd <- position_dodge(0.1)
      counter = 0
      plots = list()
      for (subject in subjects){
        counter = counter + 1
        plots[[counter]] <- ggplot(df2.meanc[df2.meanc$subj == subject, ], aes(x=load, y=win, colour=lang, group=lang)) + 
        geom_errorbar(aes(ymin=win-se, ymax=win+se), width=.1, position = pd) +
        geom_line() +
        geom_point() +
        ggtitle(subject) +
        theme(axis.title.x=element_blank()) # +
        # scale_y_continuous(limits=c(-2, 1.5))
        }
      }
    lang_main = unlist(summary(df2.aov)[[2]])[[9]]
    load_main = unlist(summary(df2.aov)[[3]])[[9]]
    langXload = unlist(summary(df2.aov)[[4]])[[9]]
    idxLOWer <- df2$load=='low' & df2$lang=='er'
    idxHIer <- df2$load=='high' & df2$lang=='er'
    idxLOWRE <- df2$load=='low' & df2$lang=='RE'
    idxHIRE <- df2$load=='high' & df2$lang=='RE'
    effSizeN1er = mean(df2[idxLOWer, 'mu_N1']) - mean(df2[idxHIer, 'mu_N1'])
    effSizeN1RE = mean(df2[idxLOWRE, 'mu_N1']) - mean(df2[idxHIRE, 'mu_N1'])
    effSizeP1er = mean(df2[idxLOWer, 'mu_P1']) - mean(df2[idxHIer, 'mu_P1'])
    effSizeP1RE = mean(df2[idxLOWRE, 'mu_P1']) - mean(df2[idxHIRE, 'mu_P1'])
    P <- rbind(P, c(loQ, hiQ, lang_main, load_main, langXload, P_mixed, effSizeN1er, effSizeN1RE, effSizeP1er, effSizeP1RE))
    i = i + 1
    setTxtProgressBar(pb, i)
  }
}
# save the p-values for each model and factor:
write.csv(P, 'Pvalues.csv')
stop('asdf')

b <- c(0, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.6, 0.8)
clrs <- c("black","purple", "navyblue","blue", "darkmagenta","green", "yellow", "darkorange", "red")
plot1 = ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) + geom_raster(aes(fill = load_main)) + ggtitle(win) + scale_fill_gradientn(colours=clrs,breaks=b)
plot2 = ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) + geom_raster(aes(fill = lang_main)) + ggtitle(win) + scale_fill_gradientn(colours=clrs,breaks=b)
plot3 = ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) + geom_raster(aes(fill = langXload)) + ggtitle(win) + scale_fill_gradientn(colours=clrs,breaks=b)
plot4 = ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) + geom_raster(aes(fill = P_mixed)) + ggtitle(win) + scale_fill_gradientn(colours=clrs,breaks=b)
plot_grid(plot1, plot2, plot3, plot4, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)

plot5 <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
  geom_tile(aes(fill = effSizeN1er)) +
  ggtitle("N1, English-Russian") +
  scale_x_continuous(name = "Higher cutoff boundary (quantile)") +
  scale_y_continuous(name = "Lower cutoff boundary (quantile)") +
  scale_fill_gradientn(limits=c(min(P$effSizeN1er[-c(1)]),
                                max(P$effSizeN1er[-c(1)])), 
                       colours = rainbow(5), name=expression(paste(mu, "V")))
plot6 <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
  geom_tile(aes(fill = effSizeN1RE)) +
  ggtitle("N1, Russian-English") +
  scale_x_continuous(name = "Higher cutoff boundary (quantile)") +
  scale_y_continuous(name = "Lower cutoff boundary (quantile)") +
  scale_fill_gradientn(limits=c(min(P$effSizeN1RE[-c(1)]),
                                max(P$effSizeN1RE[-c(1)])),
                       colours = rainbow(5), name=expression(paste(mu, "V")))
plot7 <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
  geom_tile(aes(fill = effSizeP1er)) +
  ggtitle("P1, English-Russian") +
  scale_x_continuous(name = "Higher cutoff boundary (quantile)") +
  scale_y_continuous(name = "Lower cutoff boundary (quantile)") +
  scale_fill_gradientn(limits=c(min(P$effSizeP1er[-c(1)]),
                                max(P$effSizeP1er[-c(1)])), 
                       colours = rainbow(5), name=expression(paste(mu, "V")))
plot8 <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
  geom_tile(aes(fill = effSizeP1RE)) +
  ggtitle("P1, Russian-English") +
  scale_x_continuous(name = "Higher cutoff boundary (quantile)") +
  scale_y_continuous(name = "Lower cutoff boundary (quantile)") +
  scale_fill_gradientn(limits=c(min(P$effSizeP1RE[-c(1)]),
                                max(P$effSizeP1RE[-c(1)])), 
                       colours = rainbow(5), name=expression(paste(mu, "V")))
plot_grid(plot5, plot6, plot7, plot8, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)

  

# now launch heatmap.R

do.call(plot_grid, plots)

# ##################
# # now try to plot the same thing, but average data between subjects:
# #####################
# # average like cases and get SEs and SDs, while getting rid of "bad" subjects:
df2.meanc <- summarySE(df2, measurevar=win, groupvars=c("lang", "load"))
# order the factors appropriately
df2.meanc$load <- factor(df2.meanc$load, levels = c("high", "mid", "low"))
# plot average between subjects lines with error bars:
ggplot(df2.meanc, aes(x=load, y=win, colour=lang, group=lang)) +
  geom_errorbar(aes(ymin=win-se, ymax=win+se), width=.1, position = pd) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits=c(-1.5, 0.5))
