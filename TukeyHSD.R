# Tukey HSD and other things

require(nlme)         ## for lme()
require(multcomp)     ## for multiple comparison stuff
require(lme4)
require(Tukey)
require(ez)
win = 'mu_N1'
options(contrasts=c("contr.sum", "contr.poly"))
df2$load <- as.factor(df2$load)
df2.meanc <- summarySE(df2, measurevar=win, groupvars=c("subj","lang", "load"))


# the standard aov and ez produce the same F-values, but lme and lmer don't:
Aov.mod <- aov(mu_N1 ~ lang * load + Error(subj/(load * lang)), data = df2.meanc)
summary(Aov.mod)

interaction.plot(response = df2.meanc$mu_N1, x.factor = df2.meanc$load, trace.factor = df2.meanc$lang,
                 xlab = "load",
                 ylab = "voltage", 
                 pch = c(1:3, 0, letters))

lmer.mod <- lmer(mu_N1 ~ lang * load + (1|subj/load), data = df2.meanc)
anova(lmer.mod)

lme.mod <- lme(mu_N1 ~ lang * load, random = ~1|subj/(load/lang), data = df2.meanc)
anova(lme.mod)

# lm.mod <- lm(mu_N1 ~ lang * load, data = df2.meanc)
# summary(lm.mod)

summary(lme.mod)
summary(glht(lme.mod, linfct=mcp(load="Tukey")))

options(contrasts=c("contr.sum", "contr.poly"))
Aov.mod <- aov(mu_N1 ~ lang * load + Error(subj/(load * lang)), data = df2.meanc)
summary(Aov.mod)
drop1(Aov.mod, .~., test="F")

anova(lm(mu_N1 ~ lang * load, data = df2.meanc, type="III",
      contrasts=list(lang=contr.sum, load=contr.sum)))

SScomp <- (mean(df2$mu_N1[df2$load=="low"]) - mean(df2$mu_N1[df2$load=="high"]))^2
dfcomp <- 1
n <- 9 #5 subjects per group
SSerr <- 4.4 #read off the ANOVA table
dferr <- 16 # read off the ANOVA table
MSerr <- SSerr / dferr
Fcomp <- (n * SScomp/2)/MSerr
pcomp <- 1-pf(Fcomp, dfcomp, dferr)

qobs <- sqrt(2*Fcomp)
ngroups <- length(levels(as.factor(df2$load)))
ptk <- 1 - ptukey(qobs, ngroups, (ngroups - 1) * (n - 1))
ptk