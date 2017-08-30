library(lme4)

politeness=
  read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

which(is.na(politeness$frequency))
politeness <- politeness[! is.na(politeness$frequency), ]

boxplot(frequency ~ attitude*gender, col=c("white","lightgray"), politeness)

politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)
summary(politeness.model)

politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), data=politeness)
summary(politeness.model)