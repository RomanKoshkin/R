############## NOW make INTERACTION PLOTS (only in sinle-step cycle mode) ####### 
newlabels <- c( "CLall", "CWall", "SYL", "CW", "CL")
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

intPlot[[1]] <- intPlot[[1]] + theme(legend.position = "none")
intPlot[[2]] <- intPlot[[2]] + theme(legend.position = "none")
intPlot[[3]] <- intPlot[[3]] + theme(legend.position = "none")
intPlot[[4]] <- intPlot[[4]] + theme(legend.position = "none")
intPlot[[5]] <- intPlot[[5]] + theme(legend.position = c(0.8, 0.8))
plot_grid(plotlist = intPlot, nrow = 2, ncol = 3)