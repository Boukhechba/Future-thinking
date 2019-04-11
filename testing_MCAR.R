library(tidyr)
library(dplyr)
library(mice)
library(pan)
library(mitml)
library(nlme)
library(lavaan)
library(mediation)
library(lme4)
library(ggplot2)
library(r2glmm)

library(Hmisc)

#Testing MCAR with demographics
d<-read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainDataDemogITT.csv")
total<-merge(d,x,by="participantId")



total$posind<-as.integer(as.logical(is.na(total$posExpBiasScale)))
total$negind<-as.integer(as.logical(is.na(total$negExpBiasScale)))
total$depind<-as.integer(as.logical(is.na(total$depressionScale)))
total$anxind<-as.integer(as.logical(is.na(total$anxietyScale)))
total$selfind<-as.integer(as.logical(is.na(total$selfEffScale)))
total$growind<-as.integer(as.logical(is.na(total$growthMindScale)))
total$optind<-as.integer(as.logical(is.na(total$optimismScale)))

total2<- aggregate(posind~participantId+age+genderId+race+ethnicity+maritalStat+education+educationGrp+employmentStat+employmentStatGrp+income+country+countryGrp,data=total,FUN=function(x) c(sum=sum(x)))


sink(file="C:/Users/mob3f/Documents/MindTrials Future Thinking/results/testing_MCAR.txt")
#age
cor.test(total2$posind, total2$age, method = c("pearson"))
#other demographics 
summary( aov(posind ~ genderId, data = total2))
summary( aov(posind ~ race, data = total2))
summary( aov(posind ~ maritalStat, data = total2))
summary( aov(posind ~ education, data = total2))
summary( aov(posind ~ educationGrp, data = total2))
summary( aov(posind ~ employmentStat, data = total2))
summary( aov(posind ~ employmentStatGrp, data = total2))
summary( aov(posind ~ income, data = total2))
summary( aov(posind ~ country, data = total2))
summary( aov(posind ~ countryGrp, data = total2))

sink()