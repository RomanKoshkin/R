library(ggplot2)
library(grid)
library(gridExtra)

channel = "Cz"
DSfolder = "NGA3_0.25-30" #"NGA3_0.1-20-ICA" #"NGA3
estimatorVector <- c("CLred", "CWred", "SYL") # "CLnored", "CWnored")

# let's build the axes and ticks and shit:
gglist <- list(scale_y_continuous(expand = c(0, 0), breaks = c(-1, -0.5, 0.5, 1), limits = c(-1.3, 1.1)), 
            scale_x_continuous(expand = c(0, 0), breaks = c(0, 100,200,300,400)), 
            labs(x="", y=""), 
            theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(),
                  axis.line.y = element_blank(), axis.ticks.y = element_blank(),
                  axis.text = element_text(face='bold', size = 10),
                  legend.key.size=unit(1,"cm"),
                  legend.position = "bottom",
                  panel.background=element_rect(fill="transparent",colour=NA),
                  plot.background=element_rect(fill="transparent",colour=NA),
                  legend.key = element_rect(fill = "transparent", colour = "transparent"),
                  axis.text.y = element_text(margin = margin(t = 0, r = -40, b = 0, l = 0)),
                  axis.title.y = element_text(angle = 0, vjust=1)), 
            geom_hline(yintercept=0), 
            geom_segment(aes(x=0,y=-1, xend=0, yend=1), size=0.25),
            
            geom_segment(aes(x=-100,y=-0.025,xend=-100,yend=0.025), size=0.25), 
            geom_segment(aes(x=100,y=-0.025,xend=100,yend=0.025), size=0.25), 
            geom_segment(aes(x=200,y=-0.025,xend=200,yend=0.025), size=0.25), 
            geom_segment(aes(x=300,y=-0.025,xend=300,yend=0.025), size=0.25), 
            geom_segment(aes(x=400,y=-0.025,xend=400,yend=0.025), size=0.25), 
            geom_segment(aes(x=500,y=-0.025,xend=500,yend=0.025), size=0.25), 
            
            geom_segment(aes(x=-6,y=-1,xend=0,yend=-1), size=0.25), 
            geom_segment(aes(x=-6,y=-0.5,xend=0,yend=-0.5), size=0.25), 
            geom_segment(aes(x=-6,y=0.5,xend=0,yend=0.5), size=0.25), 
            geom_segment(aes(x=-6,y=1,xend=0,yend=1), size=0.25), 
            
            guides(linetype = guide_legend(override.aes = list(color = c("black", "black", "grey"),
                                                               labels = c("high", "medium", "low")),
                                                               nrow=1)))
        

# get the legend for recycling later:
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

i = 0
ER = list()
RE = list()

for (estimator in estimatorVector){
i = i + 1
filename <- paste(DSfolder, "/ERPs", estimator, "_", channel, ".csv", sep = "")
erp <- read.csv(filename)

ER[[i]] <- ggplot(data=erp, aes(x=times)) +
  geom_line(aes(y=mu_N1_er_low, linetype = "low"),color = "black", size=1) +
  geom_line(aes(y=mu_N1_er_mid, linetype = "medium"),color = "grey", size=1) +
  geom_line(aes(y=mu_N1_er_high, linetype = "high"),color = "black", size=1) +
  gglist

RE[[i]] <- ggplot(data=erp, aes(x=times)) +
  geom_line(aes(y=mu_N1_RE_low, linetype = "low"), color = "black", size=1) +
  geom_line(aes(y=mu_N1_RE_mid, linetype = "medium"),color = "darkgrey", size=1) +
  geom_line(aes(y=mu_N1_RE_high, linetype = "high"),color = "black", size=1) +
  gglist

ER[[i]]$labels$linetype <- '          WM load'

mylegend<-g_legend(ER[[i]])
}

them <- theme(legend.position="none",
              plot.title = element_text(color="black",
                                        size=18,
                                        face="bold",
                                        hjust = 0.5),
              panel.background = element_rect(fill = "white"))

gr <- grid.arrange(arrangeGrob(
                   textGrob(paste('L2', '>', 'L1'), gp=gpar(fontsize=18, fontface = "bold"), rot=90),
                         ER[[1]] + them + ggtitle('Scaled Content Words (CL)'),
                         ER[[2]] + them + ggtitle("Content Words (CW)"),
                         ER[[3]] + them + ggtitle('Syllables (SYL)'),
                   textGrob(paste('L1', '>', 'L2'), gp=gpar(fontsize=18, fontface = "bold"), rot=90),
                         RE[[1]] + them,
                         RE[[2]] + them,
                         RE[[3]] + them,
                   nrow=2, widths=c(1,4,4,4)),
             mylegend, nrow=2, heights=c(10, 1))
gr