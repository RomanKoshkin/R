# this code creates heatmaps for anova P-values and means as a function of condition cutoff quantiles. 
# The p-values are for ANOVA
# + TABLE2 of the paper.
# + INTERACTION PLOTS FOR THE PAPER (AND PREVIOUSLY THE THESIS)
# this code IS BETTER than EffectSizes.R because it TryCatches when the aov model is singular.
# If you modify lines 59 and 61, you can do single tests.

# if you do P1, modify, lines 197 (for the proper limits c(0,1))
# ALSO CHANGE LINE 27, 97, 190, 194
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
library(jtrans)
cat("\014")
############## PARAMETERS #############################
bounds = c(0.05, 0.95)
step = 0.05
easy = 1 # use ezANOVA, compute GG/HF-corrected p-values, otherwise use plain vanilla ANOVA
total = (length(seq(bounds[1], bounds[2], by = step)))^2/2
win = "mu_N1" # CHANGE THIS IN LINE 92, 187, 191 ALSO
formula <- as.formula(paste(win," ~ lang * load + Error(subj / (lang * load))", sep=""))

estimatorVector <- c("CLred")#, "CWred", "SYL", "CLnored", "CWnored")



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
intPlot <- list()
counter = 0
subjects = c('KOK', 'GRU', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')
languages = c('er', 'RE')
df <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")
print("tranforming N1...")
a <- jtrans(df$mu_N1, test = "ad.test", exclude_original = TRUE, z_lim = c(0.25, 1.25))
df$mu_N1 <- a$transformed
print("tranforming P1...")
a <- jtrans(df$mu_P1, test = "ad.test", exclude_original = TRUE, z_lim = c(0.25, 1.25))
df$mu_p1 <- a$transformed
print("transformation to normality complete.")
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
      df2.meanc <- summarySE(df2, measurevar=win, groupvars=c("subj","lang", "load"))
      df2.aov <- aov(data=df2.meanc, formula)
      
     
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
      
      if (easy==1){
      EE = tryCatch({
        suppressWarnings(ezANOVA(data=df2.meanc, dv=mu_N1, 
                                 wid=.(subj), within=.(load, lang),
                                 between = NULL, type = 3, detailed = TRUE))},
        error = function(e) {
          print(paste("ERROR!", loQ, hiQ))
          return(NULL)})
        finally = {print(EE)}
      if (is.null(EE)){
        pval_lang <- 1
        pes_lang <- 0
        pval_load <- 1
        pes_load <- 0
        pval_langload <- 1
        pes_langload <- 0}
      else{
      qq <- anova_apa(EE, es = c("pes")) # ges for general eta-squared, format = "docx"
      q <- read.table(text = qq$` `[2], sep=",") # lang
      pval_lang <- as.numeric(gsub(".*(<|=)","",as.character(q$V3)))
      pes_lang <- as.numeric(gsub(".*(<|=)","",as.character(q$V4))) #lang
      q <- read.table(text = qq$` `[3], sep=",") # load
      pval_load <- as.numeric(gsub(".*(<|=)","",as.character(q$V3)))
      pes_load <- as.numeric(gsub(".*(<|=)","",as.character(q$V4))) #lang
      q <- read.table(text = qq$` `[4], sep=",") # lang:load
      pval_langload <- as.numeric(gsub(".*(<|=)","",as.character(q$V3)))
      pes_langload <- as.numeric(gsub(".*(<|=)","",as.character(q$V4)))
      P <- rbind(P, c(loQ, hiQ, 
                      pval_lang, pval_load, pval_langload, 
                      P_mixed, effSizeN1er, effSizeN1RE, effSizeP1er, effSizeP1RE))}
      }
      else{
      lang_main = unlist(summary(df2.aov)[[2]])[[9]]
      load_main = unlist(summary(df2.aov)[[3]])[[9]]
      langXload = unlist(summary(df2.aov)[[4]])[[9]]
      P <- rbind(P, c(loQ, hiQ, lang_main, load_main, langXload, P_mixed, effSizeN1er, effSizeN1RE, effSizeP1er, effSizeP1RE))
      }
      i = i + 1
      setTxtProgressBar(pb, i)
    }
  }
  
  Them2 <- theme(axis.title.x=element_blank(), axis.title.y=element_blank())
  x_lab <- scale_x_continuous(name = "med./high WM load boundary (quantile)")
  y_lab <- scale_y_continuous(name = "low/med. WM load boundary (quantile)")
  plot5[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
    geom_tile(aes(fill = effSizeN1er)) +
    ggtitle(paste("N1, Eng-Rus", newlabels[jj])) +
    x_lab +
    y_lab +
    scale_fill_gradientn(limits=c(min(P$effSizeN1er[-c(1)]),
                                  max(P$effSizeN1er[-c(1)])), 
                         colours = rainbow(5), name=expression(paste(mu, "V")))# + Them2
  plot6[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
    geom_tile(aes(fill = effSizeN1RE)) +
    ggtitle(paste("N1, Rus-Eng", newlabels[jj])) +
    x_lab +
    y_lab +
    scale_fill_gradientn(limits=c(min(P$effSizeN1RE[-c(1)]),
                                  max(P$effSizeN1RE[-c(1)])),
                         colours = rainbow(5), name=expression(paste(mu, "V")))# + Them2
  plot7[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
    geom_tile(aes(fill = effSizeP1er)) +
    ggtitle(paste("P1, Eng-Rus", newlabels[jj])) +
    x_lab +
    y_lab +
    scale_fill_gradientn(limits=c(min(P$effSizeP1er[-c(1)]),
                                  max(P$effSizeP1er[-c(1)])), 
                         colours = rainbow(5), name=expression(paste(mu, "V")))# + Them2
  plot8[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) +
    geom_tile(aes(fill = effSizeP1RE)) +
    ggtitle(paste("P1, Rus-Eng", newlabels[jj])) +
    x_lab +
    y_lab +
    scale_fill_gradientn(limits=c(min(P$effSizeP1RE[-c(1)]),
                                  max(P$effSizeP1RE[-c(1)])), 
                         colours = rainbow(5), name=expression(paste(mu, "V")))# + Them2
  # plot_grid(plot5, plot6, plot7, plot8, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
  
  b <- c(0, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.6, 0.8)
  clrs <- c("black","purple", "navyblue","blue", "darkmagenta","green", "yellow", "darkorange", "red")
  tit <- paste(win, newlabels[jj])
  Them <- theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), 
                axis.title.y=element_blank(), axis.ticks.y=element_blank(), legend.position="none")
  plot1[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) + geom_raster(aes(fill = load_main)) + ggtitle(paste(tit, "Load")) + scale_fill_gradientn(colours=clrs,breaks=b) + Them
  plot2[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) + geom_raster(aes(fill = lang_main)) + ggtitle(paste(tit, "S.Lang.")) + scale_fill_gradientn(colours=clrs,breaks=b) + Them
  plot3[[jj]] <- ggplot(data = P[-c(1), ], aes(x = hiQ, y = loQ)) + geom_raster(aes(fill = langXload)) + ggtitle(paste(tit, "Load x S.Lang.")) + scale_fill_gradientn(colours=clrs,breaks=b) + Them
  
  
############## NOW make INTERACTION PLOTS (only in sinle-step cycle mode) ####### 
  df3.meanc <- summarySE(df2, measurevar=win, groupvars=c("lang", "load"))
  df3.meanc$load <- factor(df3.meanc$load, levels = c("high", "mid", "low")) # order the factors appropriately
  counter = counter + 1
  pd <- position_dodge(0.1)
  intPlot[[counter]] <- ggplot(df3.meanc, 
                               aes(x=load, y=mu_N1, 
                               color=factor(lang, labels=c("Eng-Rus", "Rus-Eng")),
                               group=lang)) + 
    labs(color = "Direction\nof interp.") +
    geom_errorbar(aes(ymin=mu_N1-se, ymax=mu_N1+se), width=.1, position = pd) +
    geom_line() +
    ggtitle(newlabels[counter]) +
    theme_grey() +
    theme(axis.title.y = element_text(angle = 0, vjust=1)) +
    theme(axis.title.x=element_blank()) +
    scale_y_continuous(limits = c(-1.5, 0)) +
    labs(x="load", y=expression(paste(mu, "V")))
  
  
  # plot_grid(plot1, plot2, plot3, plot4, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
}


qq <- list()
qq <- rbind(plot1,plot2, plot3)
plot_grid(plotlist = qq, nrow = 5, ncol = 3, align = "v")

ee <- list()
ee <- rbind(plot5, plot6, plot7, plot8)
plot_grid(plotlist = ee, nrow = 5, ncol = 4, align = "v")

# cat("\014")
#print(summary(aov(data=df2.meanc, formula)))
intPlot[[1]] <- intPlot[[1]] + theme(legend.position = "none")
intPlot[[2]] <- intPlot[[2]] + theme(legend.position = "none")
intPlot[[3]] <- intPlot[[3]] + theme(legend.position = "none")
intPlot[[4]] <- intPlot[[4]] + theme(legend.position = "none")
intPlot[[5]] <- intPlot[[5]] + theme(legend.position = c(0.8, 0.8))
plot_grid(plotlist = intPlot, nrow = 2, ncol = 3)



