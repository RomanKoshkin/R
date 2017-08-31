# convert data from default R (long) to wide (SPSS)

library(reshape2)
library(Rmisc)
df2.sum <- summarySEwithin(df2, measurevar = "mu_N1",
                           betweenvars = "subj", withinvars = c("lang","load"),
                           na.rm = TRUE, conf.interval = 0.95)
data_wide <- dcast(df2.sum, subj ~ lang + load, value.var="mu_N1")
write.csv(data_wide, "/Users/RomanKoshkin/Documents/IBM/data_wide.csv")


# wide to long:
data_long<-reshape(data_wide, 
                      varying=c("er_high", "er_low", "er_mid",  "RE_low", "RE_high", "RE_mid"),
                      direction="long",
                      idvar="id",
                      sep="_")

data_long <- data_long[, -c(5)]
data_long <- melt(data_long, id.vars=c("subj", "time"))
colnames(data_long) <- c("subj", "load", "lang", "value")
