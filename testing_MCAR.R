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


x <- read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainDataScales.csv")
p<-read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainAnalysisSamples.csv")
ITT<-subset(p, ittSample==1)
Completers<-subset(p, txCompSample==1)
ittx<-x[which(x$participantId %in% ITT$participantId), ]
total<-merge(d,ittx,by="participantId")



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
cor.test(total2$posind, total2$age, method = c("spearman"))

#other demographics 
kruskal.test (total2$posind,total2$genderId)
kruskal.test (total2$posind,total2$race)
kruskal.test (total2$posind,total2$maritalStat)
kruskal.test (total2$posind,total2$education)
kruskal.test (total2$posind,total2$educationGrp)
kruskal.test (total2$posind,total2$employmentStat)
kruskal.test (total2$posind,total2$employmentStatGrp)
kruskal.test (total2$posind,total2$income)
kruskal.test (total2$posind,total2$country)
kruskal.test (total2$posind,total2$countryGrp)

sink()