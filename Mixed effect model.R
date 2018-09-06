#Used packages
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

writeresults <- function(x,path){
  print(paste0("Current working dir: ",path))
  sink(file=path) 
  print(summary(lme(posExpBiasScale ~ condition+session_int+condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(negExpBiasScale ~ condition+session_int+condition*session_int, random = ~session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(depressionScale ~ condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(anxietyScale ~ condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(selfEffScale ~ condition+session_int+condition:session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(growthMindScale ~ condition+session_int+condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  print(summary(lme(optimismScale ~ condition+session_int+condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit)))
  sink()
}
preparedata<-function(x){
  x <- subset(x, scenarioIndex==40 | is.na(x$scenarioIndex))
  #Remove the following participants
  x<-x[which(!x$participantId %in% c(1307,138,200,392,412,453,495,496,577,582,627,634,788,942,961)), ]
  #Groupe Eligibility and Pretest into one session named baseline
  x$session <- gsub('Eligibility','Baseline',  x$session)
  x$session <- gsub('preTest','Baseline',  x$session)
  x$session <- factor(x$session, levels=c("Baseline", "firstSession","secondSession","thirdSession","fourthSession","PostFollowUp"))
  x$session_int <- as.integer(x$session)
  x<-aggregate(x=x[c("posExpBiasScale", "negExpBiasScale","depressionScale","anxietyScale","selfEffScale","growthMindScale","optimismScale")], by=list(participantId=x$participantId,session=x$session,condition=x$condition,session_int=x$session_int), mean, na.rm = TRUE)
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  x[is.nan(x)] <- NA
  return(x)
}
############################################################################################################
############################################################################################################
###Longitudunal analysis with 3 conditions

x <- read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTdataScored.csv")
x <- preparedata(x)
x$condition <- gsub('FIFTYFIFTYBLOCKED','FIFTYFIFTY',  x$condition)
x$condition <- gsub('FIFTYFIFTYRANDOM','FIFTYFIFTY',  x$condition)
x$condition <- gsub('POSITIVENEGATION','POSITIVE',  x$condition)

#Get a global view on our data
summary(x)
md.pattern(x)

#Multiple imputation  for multilevel data Using the pan package
fml <- posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale  ~ condition + session_int + condition*session_int + (1+session_int|participantId)
imp <- panImpute(x, formula=fml, n.burn=5000, n.iter=100, m=100)
x <- mitmlComplete(imp, 1)
#This si to summurize and plot the results of the imputation procedure
summary(imp)
plot(imp, trace="all", print="beta", pos=c(1,2))


x$condition <- factor(x$condition, levels=c( "NEUTRAL","FIFTYFIFTY","POSITIVE"))
writeresults(x,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/2conditions_vs_Neutral.txt')

x$condition <- factor(x$condition, levels=c( "FIFTYFIFTY","POSITIVE","NEUTRAL"))
writeresults(x,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/2conditions_vs_50_50.txt')


############################################################################################################
############################################################################################################
###Longitudunal analysis with 5 conditions

x <- read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTdataScored.csv")
x <- preparedata(x)
#Get a global view on our data
summary(x)
md.pattern(x)

#Multiple imputation  for multilevel data Using the pan package
fml <- posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale  ~ condition + session_int + condition*session_int + (1+session_int|participantId)
imp <- panImpute(x, formula=fml, n.burn=5000, n.iter=100, m=100)
x <- mitmlComplete(imp, 1)
#This si to summurize and plot the results of the imputation procedure
summary(imp)
plot(imp, trace="all", print="beta", pos=c(1,2))

x$condition <- factor(x$condition, levels=c("FIFTYFIFTYRANDOM","FIFTYFIFTYBLOCKED","POSITIVENEGATION","POSITIVE","NEUTRAL"))
writeresults(x,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/4conditions_vs_50_50Random.txt')

x$condition <- factor(x$condition, levels=c("POSITIVE","POSITIVENEGATION","NEUTRAL","FIFTYFIFTYRANDOM","FIFTYFIFTYBLOCKED"))
writeresults(x,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/4conditions_vs_Positive.txt')

#This is an example of who to use the mediation package

lmm1<-lmer(posExpBiasScale ~ condition*session_int + (1 +session_int|participantId), data = x,REML = FALSE)
lmm1m<-lmer(selfEffScale ~ condition*session_int +posExpBiasScale+ (1 +session_int|participantId), data = x,REML = FALSE)
summary(mediate( lmm1, lmm1m,sims = 500,boot=FALSE, treat = 'condition', mediator = 'posExpBiasScale'))

###Please ignore the rest for now  

#Mediation
x$condition <- factor(x$condition, levels=c( "NEUTRAL","POSITIVE","FIFTYFIFTY"))


summary(lmm1)
lmm1<-lmer(selfEffScale ~ condition*session_int + (1 +session_int|participantId), data = x,REML = FALSE)
summary(lmm1m)
#lmm1m<-lmer(optimismScale ~ condition*session_int +posExpBiasScale*session_int+ (1 +session_int|participantId), data = x,REML = FALSE)
#lmm1m<-lmer(growthMindScale ~ condition*session_int +posExpBiasScale*session_int+ (1 +session_int|participantId), data = x,REML = FALSE)





mediation.test(x$condition,x$posExpBiasScale,x$optimismScale)


lmm1<-lm(posExpBiasScale ~ condition , data = x)
lmm1m<-lm(selfEffScale ~ condition +posExpBiasScale, data = x)
summary(mediate( lmm1m, lmm1,sims = 500,boot=TRUE, treat = 'condition', mediator = 'posExpBiasScale'))

mediated.mlm <- boot( data=x, statistic=indirect.mlm, R=100, strata=x$participantId, 
                      y="selfEffScale", x="session_int", mediator="posExpBiasScale", group.id="participantId", 
                      between.m=F, uncentered.x=F )

indirect.mlm.summary( mediated.mlm )

set.seed(1234)
model <- ' # direct effect
             selfEffScale ~ c*condition:session_int
# mediator
posExpBiasScale ~ a*condition:session_int
selfEffScale ~ b*posExpBiasScale
# indirect effect (a*b)
ab := a*b
# total effect
total := c + (a*b)
'

x <- subset(x, condition!="FIFTYFIFTY")
x$condition <- factor(x$condition, levels=c( "POSITIVE","NEUTRAL"))
x$condition <- factor(x$condition, levels=c( "NEUTRAL","POSITIVE"))

fit <- lavaan::sem(model, data = x)
summary(fit)

temp<-mlma(y=selfEffScale, biny=FALSE, data1=x, x=condition, levelx=1, m=posExpBiasScale, c1r=1,level=participantId)


summary(lmm1)
summary(lmm1m)

summary(lme(selfEffScale ~ condition+session_int+condition:session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit))
summary(lme(selfEffScale ~ condition+session_int+condition:session_int+posExpBiasScale, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit))
#lmm1m<-lmer(growthMindScale ~ condition*session_int +posExpBiasScale*session_int+ (1 +session_int|participantId), data = x,REML = FALSE)
  
write.csv(lmm1, file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/posExpBiasScale.csv")
write.csv(data.frame(lmm2$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/negExpBiasScale.csv")
write.csv(data.frame(lmm3$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/depressionScale.csv")
write.csv(data.frame(lmm4$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/anxietyScale.csv")
write.csv(data.frame(lmm5$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/selfEffScale.csv")
write.csv(data.frame(lmm6$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/growthMindScale.csv")
write.csv(data.frame(lmm7$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/optimismScale.csv")

#Mediation analysis
x <- subset(x, !is.na(x$diffposExpBiasScale))
md.pattern(x)
imputed_Data <- mice(x, m=1, maxit = 200, method = 'pmm', seed = 500)
completeData <- complete(imputed_Data,1)
x<-completeData
x$n<-1
x<-spread(x, condition, n,fill = 0)
x$condition <- factor(x$condition, levels=c( "NEUTRAL","FIFTYFIFTY","POSITIVE"))
lmm1xy<-lme(diffdepressionScale ~ POSITIVE+session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML")
lmm1xm<-lme(diffposExpBiasScale ~ POSITIVE+session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML")
lmm1xmy<-lme(diffdepressionScale ~ POSITIVE+diffposExpBiasScale+session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML")
summary(lmm1xy)
summary(lmm1xm)
summary(lmm1xmy)

res<-mediate(lmm1xm, lmm1xmy, sims = 500, treat = "POSITIVE", mediator = "diffposExpBiasScale",robustSE = TRUE)

lmm1<-lme(anxietyScale ~ condition+session_int, random = ~1+session_int|participantId,control=ctrl, data=x[!is.na(x$diffanxietyScale)&!is.na(x$diffposExpBiasScale),], method="ML")
lmm1m<-lme(anxietyScale ~ condition+diffposExpBiasScale+session_int, random = ~1+session_int|participantId,control=ctrl, data=x[!is.na(x$diffanxietyScale)&!is.na(x$diffposExpBiasScale),], method="ML")
summary(lmm1)
summary(lmm1m)
res<-mediate(lmm1m, lmm1, sims = 500,boot=TRUE, treat = "condition", mediator = "diffposExpBiasScale")

lmm1<-lme(diffposExpBiasScale ~ condition+session_int, random = ~1+session_int|participantId,control=ctrl, data=x[!is.na(x$diffposExpBiasScale),], method="ML")
lmm1m<-lme(diffselfEffScale ~ condition+diffposExpBiasScale+session_int, random = ~1+session_int|participantId,control=ctrl, data=x[!is.na(x$diffselfEffScale)&!is.na(x$diffposExpBiasScale),], method="ML")
summary(lmm1)
summary(lmm1m)
res<-mediate(lmm1m, lmm1, sims = 500,boot=TRUE, treat = "condition", mediator = "diffposExpBiasScale")

sobel.lme(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffdepressionScale,grpid=x$participantId)
sobel(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffdepressionScale)
sobel(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffanxietyScale)
sobel(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffselfEffScale )
sobel(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffgrowthMindScale)
sobel(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffoptimismScale)

f <- function(x, name) {
last<-NA
for (row in 1:nrow(x)) {
  session <- x[row, "session_int"]
  variable  <- x[row, name]
  if (!is.na(last) & !is.na(variable)){
    diff <- variable -last
    print(paste("On",session,"and variable", variable, "the difference is", diff))
    x[row, paste("diff", name, sep="")]<-diff
  }else{
    print(paste("On",session,"and variable", variable, "the stock price was", last))
    x[row, paste("diff", name, sep="")]<-NA
  }
  if(!is.na(variable)){
    last <- variable
  }
  if(session==6){
    last<-NA
  }

}
return (x)
}






