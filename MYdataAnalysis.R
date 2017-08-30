library(cowplot )
library(plyr)

estimator <- c("CWnored", "CLnored", "SYL", "CWred", "CLred")
subjects = c('GRU', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL', 'KOK')
languages = c('er', 'RE')
df <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_interp1.csv")
df2 <- df[df$subj != 'a', ] # cleanup data

# generate a summary table:
ddply(df2, ~subj~lang, summarise, mean=mean(CLred, na.rm = T))

# plotting subplots:
bp <- list()
for (i in seq(1, length(estimator))){
bp[[i]] <- ggplot(df2[which(is.na(df2[[estimator[i]]])==F), ], 
                  aes_string(x="subj", y=estimator[i], fill="lang"), color=lang) +
  geom_boxplot() + 
  theme(legend.position = "none") +
  scale_y_continuous(name = "Load") +
  scale_x_discrete(name = "Participants") +
  background_grid(major = "xy", minor = "none") +
  ggtitle(estimator[i]) +
  theme(axis.text.x = element_text(size=8))
}
# bp [[5]] <- bp[[5]] + theme(legend.position = "right")

newlabels <- c( "CW", "CL", "SYL", "CWall", "CLall")
for (i in seq(1:length(newlabels))){bp[[i]] <- bp[[i]] + ggtitle(newlabels[i])}

# create a density plot:
xdensity1 <- ggplot(df2, aes(CWnored, fill=lang)) + 
  geom_density(alpha=.5, na.rm = TRUE, adjust = 2) + 
  theme(legend.position=c(0.8,1), legend.justification=c(0,1)) +
  scale_fill_discrete(name="Direction of\ninterpretation",
                      breaks=c("er", "RE"),
                      labels=c("En-Ru", "Ru-En")) +
  scale_x_discrete(name = "Load (CW)") +
  theme(legend.position="right")
xlim(0, 10)

for (i in seq(2, 5, 1)){
  bp[[i]] <- bp[[i]] + theme(axis.text.x = element_text(colour = "white"),
                           axis.title = element_text(colour = "white"))}
plot_grid(bp[[1]], bp[[2]] , bp[[3]], bp[[4]], bp[[5]], xdensity1 , labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2)  
  

# bp + scale_fill_discrete(name="Direction of\ninterpretation",
#                          breaks=c("er", "RE"),
#                          labels=c("En-Ru", "Ru-En"))


xdensity2 <- ggplot(df2, aes(CLred, fill=lang)) + 
  geom_density(alpha=.5, na.rm = TRUE, adjust = 1.2) + 
  theme(legend.position=c(0.8,1), legend.justification=c(0,1)) +
  xlim(0, 7) +
  scale_fill_discrete(name="Direction of\ninterpretation",
                      breaks=c("er", "RE"),
                      labels=c("En-Ru", "Ru-En")) +
  scale_x_continuous(name = "Lag, (words weighted by freq.)")
plot_grid(xdensity1, xdensity2, labels = c("A", "B"), ncol = 2, nrow = 1)
xhistog <- ggplot(df2, aes(CWnored)) +
  geom_histogram(alpha = 0.5, data=subset(df2,lang=='er'), fill = 'blue', colour = "darkgreen") +
  geom_histogram(alpha = 0.5, data=subset(df2,lang=='RE'), fill = 'yellow', colour = "red") +
  scale_x_continuous(name = "Lag, (content words)") +
  theme(legend.position=c(0.8,1), legend.justification=c(0,1)) +
  scale_fill_discrete(name="Direction of\ninterpretation",
                        breaks=c("er", "RE"),
                        labels=c("En-Ru", "Ru-En"))
  

# plot_grid(xdensity, xhistog)

xdensityCL <- ggplot() + 
  geom_density(alpha=.5, aes(x=CLred, fill=lang), na.rm = TRUE, adjust = 1.5, colour="blue", data=df[which(df$lang=="RE"), ]) + 
  geom_density(alpha=.5, aes(x=CLred, fill=lang), na.rm = TRUE, adjust = 1.5, colour="red", data=df[which(df$lang=="er"), ]) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0.8,1), legend.justification=c(0,1))
xdensityCW <- ggplot() + 
  geom_density(alpha=.5, aes(x=CWred, fill=lang), na.rm = TRUE, adjust = 0.5, colour="blue", data=df[which(df$lang=="RE"), ]) + 
  geom_density(alpha=.5, aes(x=CWred, fill=lang), na.rm = TRUE, adjust = 0.5, colour="red", data=df[which(df$lang=="er"), ]) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0.8,1), legend.justification=c(0,1))
plot_grid(xdensityCL, xdensityCW, labels=c("CL", "CW"), ncol = 2, nrow = 1)