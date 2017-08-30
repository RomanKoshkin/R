library(jtrans)
estimator <- "CWnored"
# but the CLred is VERY skewed!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
x <- jtrans(df2[[estimator]], test = 'ad.test', exclude_original = TRUE, z_lim = c(0.25, 1.25))

# repeated measures ANOVA on normalized data (originally very skewed)
df2[[estimator]] <- x$transformed
df2.meanc <- summarySE(df2, measurevar=estimator, groupvars=c("subj","lang"))
CL.aov <- with(df2.meanc, aov(CWnored ~ lang + Error(subj)))
summary(CL.aov)


# this model is worse, it has larger residuals. The variance due to subject variation is included in the error
CL.aov <- with(df2.meanc, aov(CWnored ~ lang))
summary(CL.aov)

# Wilcoxon signed rank test (non-parametric version of the paired t-test)
estimator <- c("CWnored", "CLnored", "SYL", "CWred", "CLred")
newlabels <- c( "CW", "CL", "SYL", "CWall", "CLall")
for (i in c(1:5)){
df2.meanc <- summarySE(df2, measurevar=estimator[i], groupvars=c("subj","lang"))
q <- cast(df2.meanc, subj ~ lang, value = estimator[i])
print("#############")
print(newlabels[i])
print(wilcox.test(q$RE,q$er, paired=TRUE))
}


# plot interactions:               
interaction.plot(df2.meanc$lang, df2.meanc$subj, df2.meanc$CLred,  fun = mean, type="b", pch = c(2,4,6),
                 legend = "F", 
                 col = c(3,4,6), ylab = "Mean of Outcome", 
                 legend(4, 300, c("Same", "Different", "Control"), col = c(4,6,3),
                        text.col = "green4", lty = c(2, 1, 3), pch = c(4, 6, 2),
                        merge = TRUE, bg = 'gray90', font = 12))
