# convert data from default R (long) to wide (SPSS)
library(reshape2)
library(Rmisc)

df2 <- read.csv('p_val_0.1.csv')
levels(df2$estimator) <- c('CL', 'CLall', 'CW', 'CWall', 'SYL')
df2 <- df2[which(df2$lang!='both'), ]
df2 <- df2[which(df2$lang!='er'), ]


data_wide <- dcast(df2, estimator ~ lang, value.var="P_rand")
data_wide <- data_wide[c('estimator', 'er', 'RE')]
