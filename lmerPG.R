library("languageR")
library(nlme)
library(bbmle)
subjects = c('KOK','GRU', 'ELT','KOZ', 'POG', 'KOS', 'ROM', 'SHE', 'BUL')
languages = c('er', 'RE')
df2 <- read.csv("/Users/RomanKoshkin/Documents/R/dataframe_0.25-30Hz.csv")
df2 <- df2[df2$subj != 'a', ] # cleanup data
# df2 <- df2[df2$subj != "GRU", ]
estimator <- "CLred"
est <- df2[[estimator]]
bounds <- c(0.1, 0.9)
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
df2.meanc <- summarySE(df2, measurevar="mu_P1", groupvars=c("subj","lang", "load"))
df2.meanc$load <- as.factor(df2.meanc$load)

# Aov.mod <- aov(mu_N1 ~ lang * load + Error(subj/(load*lang)), data = df2.meanc)
# summary(Aov.mod)

# lme.mod <- lme(mu_N1 ~ lang * load, random = ~1|subj, data = df2.meanc)
# anova(lme.mod)

# mu_N1 depends on load and language (* for interaction also), 
# intercept varies by subject and language
# variance varies by subject
# https://stats.stackexchange.com/questions/58669/specifying-multiple-separate-random-effects-in-lme
# lme.null <- lme(mu_N1 ~ load * lang, random=~1|subj, data=df2.meanc)
# #wts=Initialize(varIdent(form=~1|subj), data=df2.meanc)
# lme.alt <- lme(mu_N1 ~ load * lang, random=~1|subj, data=df2.meanc, weights=~se)
# anova(lme.null, lme.alt)


# specifying random effects with random slopes and random intercepts:
lmeformula_null <- as.formula("mu_P1 ~ lang + (1|subj)")  #see Page 11 (Bates, 2010)
lmeformula_alt  <- as.formula("mu_P1 ~ lang + load + (1|subj)") # (1+load+lang|subj) + (0+load|lang)
mod.null = lmer(lmeformula_null, data=df2, REML=FALSE)
mod.alt = lmer(lmeformula_alt, data=df2, REML=FALSE) #, weights = 1/se
#anova(mod.null, mod.alt) # Likelihood ratio test to determine if the two models are significantly different
print(anova(mod.null, mod.alt)) # get p-values out of the LME model
coef(mod.alt) # check out the coefficients

# df2$load <- factor(df2$load, c("low", "mid", "high"), ordered = TRUE)
# xyplot(mu_N1 ~ load, groups=lang, type= c("p", "r"), data=df2)
# xtabs(~ lang + load, df2)
# 
# fm0 <- lm(mu_N1 ~ load,df2)
# fm1 <- lmer(mu_N1 ~ load + (1|subj), df2, REML=FALSE)
# AIC(fm0,fm1)
# rr <- simulate(~ load + (1|subj),
#                newparams=list(theta=0,beta=fixef(fm1), sigma=sigma(fm1)),
#                newdata=df2,
#                family="gaussian",
#                seed=2)[[1]]
# ss <- transform(df2,Reaction=rr)
# fm1Z <- update(fm1,data=ss)
# VarCorr(fm1Z)
# ##  Groups   Name        Std.Dev.
# ##  Subject  (Intercept)  0.000  
# ##  Residual             29.241
# fm0Z <- update(fm0,data=ss)
# all.equal(c(logLik(fm0Z)),c(logLik(fm1Z)))  ## TRUE
