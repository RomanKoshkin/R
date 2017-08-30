
library(boot) 
anova.daten=data.frame(subject=sort(rep(1:10,2)), mz=rep(1:2,10), ort=sort(rep(1:2,10)),PHQ_Sum_score=rnorm(20,10,2))  #generate data 

summary(aov(PHQ_Sum_score~mz*ort+Error(subject/mz),data=anova.daten))



F_values <- function(formula, data1, indices) { 
  data2=subset(data1, data1$mz==2)  #subsetting data for each measuring time 
  data3=subset(data1, data1$mz==1) 
  data4 <- data3[indices,] # allows boot to select sample 
  subjekte=na.omit(data4$subject) 
  data5=rbind(data3[subjekte,], data2[subjekte,]) #combine data 
  data5$subject=factor(rep(1:length(subjekte),2)) #convert repeated subjects to unique subjects 
  fit=aov(formula,data=data5)                #fit model 
  return(c(summary(fit)[1][[1]][[1]]$`F value`, summary(fit)[2][[1]][[1]]$`F value`))     #return F-values 
} 


results <- boot(data=anova.daten, statistic=F_values,           
                R=10, formula=PHQ_Sum_score~mz*ort+Error(subject/mz))      #bootstrap 