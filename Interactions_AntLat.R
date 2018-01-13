# get the legend for recycling later:
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

pd <- position_dodge(0.1)
#====================================================  N1 laterality:load
df4.meanc <- summarySE(df2, measurevar="mu_N1", groupvars=c("laterality", "load"))
df4.meanc$load <- factor(df4.meanc$load, levels = c("high", "mid", "low")) # order the factors appropriately
a <- ggplot(df4.meanc, 
       aes(x=load, y=mu_N1, 
           color=factor(laterality),
           group=laterality)) + 
  labs(color = "Laterality") +
  geom_errorbar(aes(ymin=mu_N1-se, ymax=mu_N1+se), width=.1, position = pd) +
  geom_line(aes(y=mu_N1), size=1) +
  theme(legend.position = "bottom",
        axis.title.y = element_text(angle = 0, vjust=1),
        axis.title.x=element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(face='bold', size = 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-1, -0.5, 0), limits = c(-1, 0)) +
  labs(x="", y = "")
mylegend1 <- g_legend(a)
#====================================================  P1 laterality:load
df4.meanc <- summarySE(df2, measurevar="mu_P1", groupvars=c("laterality", "load"))
df4.meanc$load <- factor(df4.meanc$load, levels = c("high", "mid", "low")) # order the factors appropriately
b <- ggplot(df4.meanc, 
       aes(x=load, y=mu_P1, 
           color=factor(laterality),
           group=laterality)) + 
  labs(color = "Laterality") +
  geom_errorbar(aes(ymin=mu_P1-se, ymax=mu_P1+se), width=.1, position = pd) +
  geom_line(aes(y=mu_P1), size=1) +
  theme(legend.position = "bottom",
        axis.title.y = element_text(angle = 0, vjust=1),
        axis.title.x=element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(face='bold', size = 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 0.5, 1), limits = c(0, 1)) +
  labs(x="", y = "")


############################################################################################################
#====================================================  N1 anteriority:load
df4.meanc <- summarySE(df2, measurevar="mu_N1", groupvars=c("anteriority", "load"))
df4.meanc$load <- factor(df4.meanc$load, levels = c("high", "mid", "low")) # order the factors appropriately
c <- ggplot(df4.meanc, 
       aes(x=load, y=mu_N1, 
           color=factor(anteriority),
           group=anteriority)) + 
  labs(color = "Anteriority") +
  geom_errorbar(aes(ymin=mu_N1-se, ymax=mu_N1+se), width=.1, position = pd) +
  geom_line(aes(y=mu_N1), size=1) +
  theme(legend.position = "bottom",
        axis.title.y = element_text(angle = 0, vjust=1),
        axis.title.x=element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(face='bold', size = 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(-1, -0.5, 0), limits = c(-1, 0)) +
  labs(x="", y = "")
mylegend2 <- g_legend(c)
#====================================================  P1 anteriority:load
df4.meanc <- summarySE(df2, measurevar="mu_P1", groupvars=c("anteriority", "load"))
df4.meanc$load <- factor(df4.meanc$load, levels = c("high", "mid", "low")) # order the factors appropriately
d <- ggplot(df4.meanc, 
       aes(x=load, y=mu_P1, 
           color=factor(anteriority),
           group=anteriority)) + 
  labs(color = "Anteriority") +
  geom_errorbar(aes(ymin=mu_P1-se, ymax=mu_P1+se), width=.1, position = pd) +
  geom_line(aes(y=mu_P1), size=1) +
  theme(legend.position = "bottom",
        axis.title.y = element_text(angle = 0, vjust=1),
        axis.title.x=element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(face='bold', size = 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 0.5, 1), limits = c(0, 1)) +
  labs(x="", y = "")

gr <- grid.arrange(
  #row 1 of the table:
  arrangeGrob(
    # row 1
    textGrob(estimator, gp=gpar(fontsize=18, fontface = "bold"), rot=0),
    textGrob("N1", gp=gpar(fontsize=18, fontface = "bold"), rot=0),
    textGrob("P1", gp=gpar(fontsize=18, fontface = "bold"), rot=0),
    # row 2
    textGrob("laterality", gp=gpar(fontsize=18, fontface = "bold"), rot=90),
    a,
    b,
    # row 3
    textGrob("anteriority", gp=gpar(fontsize=18, fontface = "bold"), rot=90),
    c,
    d,
    nrow=3, widths=c(1,6,6), heights = c(1,5,5)),
  # ROW 2 OF THE TABLE:
  nrow=2, heights=c(10, 1))
gr