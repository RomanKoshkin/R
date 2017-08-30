# The ERP pictures for the VirtualCogLab were generated using the dataframe_20Hz_interp1.mat file (CLred)

estimator <- "CLred"
filename <- paste("ERPs", estimator, ".csv", sep = "")
erp <- read.csv(filename)
Them2 <- theme(axis.title.y = element_text(angle = 0, vjust=1),
               legend.position="none")
# panel.grid.major=element_line(colour="lightgrey"))
er <- ggplot(data=erp, aes(x=times)) +
  geom_line(aes(y=mu_N1_er_low, linetype = "low")) +
  geom_line(aes(y=mu_N1_er_mid, linetype = "medium")) +
  geom_line(aes(y=mu_N1_er_high, linetype = "high")) +
  scale_y_continuous(limits = c(-1.7, 1.2)) +
  ggtitle("English-Russian") +
  labs(x="time, ms", y=expression(paste(mu, "V"))) +
  guides(linetype=guide_legend(title="WM load")) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust=1, size=18),
        legend.position="none", 
        text = element_text(size=20))
RE <- ggplot(data=erp, aes(x=times)) +
  geom_line(aes(y=mu_N1_RE_low, linetype = "low")) +
  geom_line(aes(y=mu_N1_RE_mid, linetype = "medium")) +
  geom_line(aes(y=mu_N1_RE_high, linetype = "high")) +
  scale_y_continuous(limits = c(-1.7, 1.2)) +
  ggtitle("Russian-English") +
  labs(x="time, ms", y=expression(paste(mu, "V"))) +
  guides(linetype=guide_legend(title="WM load")) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust=1, size=18),
        legend.position=c(0.8, 0.25),
        text = element_text(size=20))


# I fucked with this code for 2 or 3 hrs. Get bp from MYdataAnalysis.R.
bp [[5]] <- bp[[5]] +
  ggtitle("WM load") +
  scale_x_discrete(name = "Participants", labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  scale_fill_grey(name="Direction of\ninterpretation",
                      breaks=c("er", "RE"),
                      labels=c("Eng-Rus", "Rus-Eng")) + 
  theme_bw() +
  theme(axis.title.x = element_text(vjust=1, size=18),
        axis.title.y = element_text(vjust=1, size=18),
        legend.position=c(0.75, 0.85),
        text = element_text(size=20))

plot_grid(er, RE, bp[[5]], nrow = 1, ncol = 3)