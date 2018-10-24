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
  print(summary(lme(posExpBiasScale ~ condition, random = ~1+session_int|participantId,control=ctrl, data=z, method="ML")))
  print(summary(lme(negExpBiasScale ~ condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML")))
  #Delete imputed data for non measured sessions
  y <- subset(x, session_int!=2 & session_int!=4)
  unique(y$session_int)
  print(summary(lme(depressionScale ~ condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=y, method="ML")))
  print(summary(lme(anxietyScale ~ condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=y, method="ML")))
  print(summary(lme(selfEffScale ~ condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=y, method="ML")))
  print(summary(lme(growthMindScale ~ condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=y, method="ML")))
  print(summary(lme(optimismScale ~ condition*session_int, random = ~1+session_int|participantId,control=ctrl, data=y, method="ML")))
  sink()
}
writeresults2 <- function(x,path){
  print(paste0("Current working dir: ",path))
  sink(file=path) 
  print(summary(lme(posExpBiasScale ~ session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML")))
  print(summary(lme(negExpBiasScale ~ session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML")))
  #Delete imputed data for non measured sessions
  y <- subset(x, session_int!=2 & session_int!=4)
  unique(y$session_int)
  print(summary(lme(depressionScale ~ session_int, random = ~1+session_int|participantId,control=ctrl, data=y, method="ML")))
  print(summary(lme(anxietyScale ~ session_int, random = ~1+session_int|participantId,control=ctrl, data=y, method="ML")))
  print(summary(lme(selfEffScale ~ session_int, random = ~1+session_int|participantId,control=ctrl, data=y, method="ML")))
  print(summary(lme(growthMindScale ~ session_int, random = ~1+session_int|participantId,control=ctrl, data=y, method="ML")))
  print(summary(lme(optimismScale ~ session_int, random = ~1+session_int|participantId,control=ctrl, data=y, method="ML")))
  sink()
}
preparedata<-function(x){
  #x <- subset(x, scenarioIndex==40 | is.na(x$scenarioIndex))
  #Remove the following participants
  #x<-x[which(!x$participantId %in% c(1307,138,200,392,412,453,495,496,577,582,627,634,788,942,961)), ]
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

x <- read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainDataScales.csv")
x <- preparedata(x)
x$condition <- gsub('FIFTY_FIFTY_BLOCKED','FIFTYFIFTY',  x$condition)
x$condition <- gsub('FIFTY_FIFTY_RANDOM','FIFTYFIFTY',  x$condition)
x$condition <- gsub('POSITIVE_NEGATION','POSITIVE',  x$condition)
unique(x$condition)

#Get a global view on our data
summary(x)
md.pattern(x)

p<-read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainAnalysisSamples.csv")
ITT<-subset(p, ittSample==1)
completers<-subset(p, txCompSample==1)

#Multiple imputation  for multilevel data Using the pan package
fml <- posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale  ~ condition + session_int + condition*session_int + (1+session_int|participantId)
imp <- panImpute(x, formula=fml, n.burn=5000, n.iter=100, m=100)
impx <- mitmlComplete(imp, 1)
#This si to summurize and plot the results of the imputation procedure
#summary(imp)
#plot(imp, trace="all", print="beta", pos=c(1,2))



#ITT
ittx<-impx[which(impx$participantId %in% ITT$participantId), ]
# Treament phase
z <- subset(ittx, session_int!=6)

z$condition <- factor(z$condition, levels=c( "NEUTRAL","FIFTYFIFTY","POSITIVE"))
writeresults(z,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/2conditions_vs_Neutral.txt')

z$condition <- factor(z$condition, levels=c( "FIFTYFIFTY","POSITIVE","NEUTRAL"))
writeresults(z,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/2conditions_vs_50_50.txt')

c <- subset(z, condition=="NEUTRAL")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/Neutral.txt')

c <- subset(z, condition=="FIFTYFIFTY")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/FIFTYFIFTY.txt')

c <- subset(z, condition=="POSITIVE")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/POSITIVE.txt')

# Follow-up phase
y <- subset(ittx, session_int==5 |session_int==6)
y$condition <- factor(y$condition, levels=c( "NEUTRAL","FIFTYFIFTY","POSITIVE"))
writeresults(y,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/2conditions_vs_Neutral.txt')

y$condition <- factor(y$condition, levels=c( "FIFTYFIFTY","POSITIVE","NEUTRAL"))
writeresults(y,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/2conditions_vs_50_50.txt')

c <- subset(y, condition=="NEUTRAL")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/Neutral.txt')

c <- subset(y, condition=="FIFTYFIFTY")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/FIFTYFIFTY.txt')

c <- subset(y, condition=="POSITIVE")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/POSITIVE.txt')

#Completers
compx<-impx[which(impx$participantId %in% completers$participantId), ]

# Treament phase
z <- subset(compx, session_int!=6)

z$condition <- factor(z$condition, levels=c( "NEUTRAL","FIFTYFIFTY","POSITIVE"))
writeresults(z,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/2conditions_vs_Neutral.txt')

z$condition <- factor(z$condition, levels=c( "FIFTYFIFTY","POSITIVE","NEUTRAL"))
writeresults(z,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/2conditions_vs_50_50.txt')

c <- subset(z, condition=="NEUTRAL")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/Neutral.txt')

c <- subset(z, condition=="FIFTYFIFTY")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/FIFTYFIFTY.txt')

c <- subset(z, condition=="POSITIVE")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/POSITIVE.txt')

# Follow-up phase
y <- subset(compx, session_int==5 |session_int==6)
y$condition <- factor(y$condition, levels=c( "NEUTRAL","FIFTYFIFTY","POSITIVE"))
writeresults(y,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/2conditions_vs_Neutral.txt')

y$condition <- factor(y$condition, levels=c( "FIFTYFIFTY","POSITIVE","NEUTRAL"))
writeresults(y,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/2conditions_vs_50_50.txt')

c <- subset(y, condition=="NEUTRAL")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/Neutral.txt')

c <- subset(y, condition=="FIFTYFIFTY")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/FIFTYFIFTY.txt')

c <- subset(y, condition=="POSITIVE")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/POSITIVE.txt')


############################################################################################################
############################################################################################################
###Longitudunal analysis with 5 conditions



x <- read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainDataScales.csv")
x <- preparedata(x)

p<-read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainAnalysisSamples.csv")
ITT<-subset(p, ittSample==1)
completers<-subset(p, txCompSample==1)
ittx<-x[which(x$participantId %in% ITT$participantId), ]

ag <- aggregate(posExpBiasScale~session+condition,data=ittx,FUN=function(x) c(n = length(x),mean=mean(x),sd=sd(x)))
write.csv(do.call(data.frame,reshape(ag, idvar = "session", timevar = "condition", direction = "wide")), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Stats/posExpBiasScale.csv")

ag <- aggregate(negExpBiasScale~session+condition,data=ittx,FUN=function(x) c(n = length(x),mean=mean(x),sd=sd(x)))
write.csv(do.call(data.frame,reshape(ag, idvar = "session", timevar = "condition", direction = "wide")), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Stats/negExpBiasScale.csv")

ag <- aggregate(depressionScale~session+condition,data=ittx,FUN=function(x) c(n = length(x),mean=mean(x),sd=sd(x)))
write.csv(do.call(data.frame,reshape(ag, idvar = "session", timevar = "condition", direction = "wide")), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Stats/depressionScale.csv")

ag <- aggregate(anxietyScale~session+condition,data=ittx,FUN=function(x) c(n = length(x),mean=mean(x),sd=sd(x)))
write.csv(do.call(data.frame,reshape(ag, idvar = "session", timevar = "condition", direction = "wide")), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Stats/anxietyScale.csv")

ag <- aggregate(selfEffScale~session+condition,data=ittx,FUN=function(x) c(n = length(x),mean=mean(x),sd=sd(x)))
write.csv(do.call(data.frame,reshape(ag, idvar = "session", timevar = "condition", direction = "wide")), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Stats/selfEffScale.csv")

ag <- aggregate(growthMindScale~session+condition,data=ittx,FUN=function(x) c(n = length(x),mean=mean(x),sd=sd(x)))
write.csv(do.call(data.frame,reshape(ag, idvar = "session", timevar = "condition", direction = "wide")), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Stats/growthMindScale.csv")

ag <- aggregate(optimismScale~session+condition,data=ittx,FUN=function(x) c(n = length(x),mean=mean(x),sd=sd(x)))
write.csv(do.call(data.frame,reshape(ag, idvar = "session", timevar = "condition", direction = "wide")), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Stats/optimismScale.csv")



##
x$changepos<-x$posExpBiasScale-x$negExpBiasScale
base<-x[which(x$session_int==1),]
names(base)[names(base)=="changepos"] <- "changeposbase"
base<-base[,c("participantId", "changeposbase"),drop=FALSE]
other<-x[which(x$session_int>1),]
new<-merge(other,base)
new$ratio<-(new$changepos-new$changeposbase)/(new$changeposbase+8)
new<-new[which(!is.na(new$changepos)),]
atrisk<-new[which(new$ratio< -0.5 ),]
##



#Get a global view on our data
summary(x)
md.pattern(x)



#Multiple imputation  for multilevel data Using the pan package
fml <- posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale  ~ condition + session_int + condition*session_int + (1+session_int|participantId)
imp <- panImpute(x, formula=fml, n.burn=5000, n.iter=100, m=100)
impx <- mitmlComplete(imp, 1)

impx$changepos<-impx$posExpBiasScale-x$negExpBiasScale
base<-impx[which(impx$session_int==1),]
names(base)[names(base)=="changepos"] <- "changeposbase"
base<-base[,c("participantId", "changeposbase"),drop=FALSE]
other<-impx[which(impx$session_int>1),]
new<-merge(other,base)
new$ratio<-(new$changepos-new$changeposbase)/(new$changeposbase+8)
new<-new[which(!is.na(new$changepos)),]
atrisk<-new[which(new$ratio< -0.5 ),]
#This si to summurize and plot the results of the imputation procedure
#summary(imp)
#plot(imp, trace="all", print="beta", pos=c(1,2))

#specifying contrast
C1 <- c(-180/961,814/961,-146/961,-173/961,-315/961)
C2 <- c(-180/961,-147/961,815/961,-173/961,-315/961)
C3 <- c(-180/961,-147/961,-146/961,788/961,-315/961)
C4 <- c(-180/961,-147/961,-146/961,-173/961,646/961)
contrs <- cbind(C1,C2,C3,C4)

C0 <- c(1/5,1/5,1/5,1/5,1/5)
pos_half <- c(-1/2,-1/2,1/2,1/2,0)
ran_bloc <- c(0,0,-1/2,1/2,0)
pp_npp <- c(-1/2,1/2,0,0,0)
pos_neut <- c(1/2,1/2,0,0,-1)
half_neut <- c(0,0,1/2,1/2,-1)
contrs2 <- cbind(pos_half,ran_bloc,pp_npp,pos_neut,half_neut)

c0<-
pos_neut <- c((180+147)/(180+147+315),(180+147)/(180+147+315),0,0,-315/(180+147+315))
pos_half <- c((180+147)/(180+147+146+173),(180+147)/(180+147+146+173),-(146+173)/(180+147+146+173),-(146+173)/(180+147+146+173),0)
half_neut <- c(0,0,(146+173)/(146+173+315),(146+173)/(146+173+315),-315/(146+173+315))
bloc_ran <- c(0,0,146/(146+173),-173/(146+173),0)
pp_npp <- c(180/(180+147),-147/(180+147),0,0,0)
contrs5 <- cbind(pos_neut,pos_half,half_neut,half_neut,half_neut)
contrs6 <- cbind(bloc_ran,pp_npp )

C1 <- c(-1/5,-1/5,4/5,-1/5,-1/5)
C2 <- c(-1/5,-1/5,-1/5,4/5,-1/5)
C3 <- c(-1/5,-1/5,-1/5,-1/5,4/5)
C4 <- c(4/5,-1/5,-1/5,-1/5,-1/5)
contrs3 <- cbind(C1,C2,C3,C4)

contrasts(z$condition) <-contrs5
solve(t(contrs2))

participantId
a<-do.call(data.frame,aggregate(posExpBiasScale~condition,data=z[which(z$session_int==0),],FUN=function(x) c(n = length(x),mean=mean(x),sd=sd(x))))
do.call(data.frame,aggregate(posExpBiasScale~condition,data=z,FUN=function(x) c(n = length(x),mean=mean(x),sd=sd(x))))
mean(z[which(z$session_int==0),"posExpBiasScale"])
do.call(data.frame,aggregate(posExpBiasScale.mean~condition,data=a,FUN=function(x) c(n = length(x)/5,mean=mean(x),sd=sd(x))))
#ITT
ittx<-impx[which(impx$participantId %in% ITT$participantId), ]
mean(z$posExpBiasScale)
# Treament phase
z <- subset(ittx, session_int!=6)

mean<-(3.443493*(146/961)+3.244220*(173/961)+3.269841*(315/961)+3.425000*(180/961)+3.335034*(147/961))
mean<-(3.443493+3.244220+3.269841+3.425000+3.335034)/5
print(summary(lme(posExpBiasScale ~ condition*session_int, random = ~1+session_int|participantId, data=z, method="ML")))
print(anova(lme(posExpBiasScale ~ condition*session_int, random = ~1+session_int|participantId, data=z, method="ML")))
print(summary(lm(posExpBiasScale ~ condition*session_int, data=z)))
table(z$session_int)
print(summary(lmer(posExpBiasScale ~ condition*session_int + (1 +session_int|participantId), data = z,REML = FALSE)))
ittx$session_int <-ittx$session_int-1
z$session_int <- as.factor(z$session_int)
z$session_int <- as.ordered(z$session_int)
z$session_int <- as.integer(z$session_int)
unique(z$session_int)
contrasts(z$session_int)
z$session_int <-z$session_int-1
mean(z$posExpBiasScale)

a<-z$session_int
b<-rep(1,961*5)
X<-data.frame(a,b)
dim(X)
X<-as.matrix(X)
solve(t(X)%*%X)%*%t(X)%*%z$posExpBiasScale

z$session_int<-as.factor(z$session_int)
contrasts(z$session_int)
a<-rep(1,961*5)
b<-c(rep(0,961),rep(1,961),rep(0,961*3))
c<-c(rep(0,961*2),rep(1,961),rep(0,961*2))
d<-c(rep(0,961*3),rep(1,961),rep(0,961))
e<-c(rep(0,961*4),rep(1,961))
X<-data.frame(a,b,c,d,e)
X<-as.matrix(X)


solve(t(X)%*%X)%*%t(X)%*%z$posExpBiasScale

mean(z[which(z$session_int==2),"posExpBiasScale"])
mean(ittx[which(ittx$session_int==0),"posExpBiasScale"])
table(x$session_int)

unique(z$session_int)
z$condition <- factor(z$condition, levels=c("POSITIVE","POSITIVE_NEGATION","FIFTY_FIFTY_BLOCKED","FIFTY_FIFTY_RANDOM","NEUTRAL"))
z$condition <- factor(z$condition, levels=c("POSITIVE_NEGATION","FIFTY_FIFTY_BLOCKED","FIFTY_FIFTY_RANDOM","NEUTRAL","POSITIVE"))
contrasts(z$condition) <-contrs
contrasts(z$condition) <-contrs2
solve(t(contrs2))
contrasts(z$condition) <-contrs3

writeresults(z,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/4conditions_vs_50_50Random.txt')

z$condition <- factor(z$condition, levels=c("POSITIVE","POSITIVE_NEGATION","NEUTRAL","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"))
writeresults(z,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/4conditions_vs_Positive.txt')

z$condition <- factor(z$condition, levels=c("NEUTRAL","POSITIVE","POSITIVE_NEGATION","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"))
writeresults(z,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/4conditions_vs_NEUTRAL.txt')

c <- subset(z, condition=="POSITIVE_NEGATION")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/POSITIVE_NEGATION.txt')

c <- subset(z, condition=="POSITIVE")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/POSITIVE_ACTIVE.txt')

c <- subset(z, condition=="FIFTY_FIFTY_RANDOM")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/FIFTY_FIFTY_RANDOM.txt')

c <- subset(z, condition=="FIFTY_FIFTY_BLOCKED")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/FIFTY_FIFTY_BLOCKED.txt')

# Follow-up phase
y <- subset(ittx, session_int==5 |session_int==6)

y$condition <- factor(y$condition, levels=c("FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED","POSITIVE_NEGATION","POSITIVE","NEUTRAL"))
writeresults(y,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/4conditions_vs_50_50Random.txt')

c <- subset(y, condition=="FIFTY_FIFTY_BLOCKED"|condition=="FIFTY_FIFTY_RANDOM")
c$condition <- factor(y$condition, levels=c("FIFTY_FIFTY_BLOCKED","FIFTY_FIFTY_RANDOM"))
writeresults(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/50_50_blocked_vs_50_50Random.txt')

y$condition <- factor(y$condition, levels=c("POSITIVE","POSITIVE_NEGATION","NEUTRAL","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"))
writeresults(y,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/4conditions_vs_Positive.txt')


y$condition <- factor(y$condition, levels=c("NEUTRAL","POSITIVE","POSITIVE_NEGATION","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"))
writeresults(y,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/4conditions_vs_NEUTRAL.txt')

c <- subset(y, condition=="POSITIVE_NEGATION")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/POSITIVE_NEGATION.txt')

c <- subset(y, condition=="POSITIVE")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/POSITIVE_ACTIVE.txt')

c <- subset(y, condition=="FIFTY_FIFTY_RANDOM")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/FIFTY_FIFTY_RANDOM.txt')

c <- subset(y, condition=="FIFTY_FIFTY_BLOCKED")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/FIFTY_FIFTY_BLOCKED.txt')

#Completers
compx<-impx[which(impx$participantId %in% completers$participantId), ]
# Treament phase
z <- subset(compx, session_int!=6)

z$condition <- factor(z$condition, levels=c("FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED","POSITIVE_NEGATION","POSITIVE","NEUTRAL"))
writeresults(z,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/4conditions_vs_50_50Random.txt')

z$condition <- factor(z$condition, levels=c("POSITIVE","POSITIVE_NEGATION","NEUTRAL","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"))
writeresults(z,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/4conditions_vs_Positive.txt')

c <- subset(z, condition=="POSITIVE_NEGATION")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/POSITIVE_NEGATION.txt')

c <- subset(z, condition=="POSITIVE")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/POSITIVE_ACTIVE.txt')

c <- subset(z, condition=="FIFTY_FIFTY_RANDOM")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/FIFTY_FIFTY_RANDOM.txt')

c <- subset(z, condition=="FIFTY_FIFTY_BLOCKED")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/FIFTY_FIFTY_BLOCKED.txt')

# Follow-up phase
y <- subset(compx, session_int==5 |session_int==6)

y$condition <- factor(y$condition, levels=c("FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED","POSITIVE_NEGATION","POSITIVE","NEUTRAL"))
writeresults(y,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/4conditions_vs_50_50Random.txt')

y$condition <- factor(y$condition, levels=c("POSITIVE","POSITIVE_NEGATION","NEUTRAL","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"))
writeresults(y,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/4conditions_vs_Positive.txt')

c <- subset(y, condition=="POSITIVE_NEGATION")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/POSITIVE_NEGATION.txt')

c <- subset(y, condition=="POSITIVE")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/POSITIVE_ACTIVE.txt')

c <- subset(y, condition=="FIFTY_FIFTY_RANDOM")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/FIFTY_FIFTY_RANDOM.txt')

c <- subset(y, condition=="FIFTY_FIFTY_BLOCKED")
writeresults2(c,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/FIFTY_FIFTY_BLOCKED.txt')



#This is an example of how to use the mediation package

lmm1<-lmer(posExpBiasScale ~ condition*session_int + (1 +session_int|participantId), data = x,REML = FALSE)
lmm1m<-lmer(selfEffScale ~ condition*session_int +posExpBiasScale+ (1 +session_int|participantId), data = x,REML = FALSE)
summary(mediate( lmm1, lmm1m,sims = 500,boot=FALSE, treat = 'condition', mediator = 'posExpBiasScale'))

###Please ignore the rest for now  
# 
# #Mediation
# x$condition <- factor(x$condition, levels=c( "NEUTRAL","POSITIVE","FIFTYFIFTY"))
# 
# 
# summary(lmm1)
# lmm1<-lmer(selfEffScale ~ condition*session_int + (1 +session_int|participantId), data = x,REML = FALSE)
# summary(lmm1m)
# #lmm1m<-lmer(optimismScale ~ condition*session_int +posExpBiasScale*session_int+ (1 +session_int|participantId), data = x,REML = FALSE)
# #lmm1m<-lmer(growthMindScale ~ condition*session_int +posExpBiasScale*session_int+ (1 +session_int|participantId), data = x,REML = FALSE)
# 
# 
# 
# 
# 
# mediation.test(x$condition,x$posExpBiasScale,x$optimismScale)
# 
# 
# lmm1<-lm(posExpBiasScale ~ condition , data = x)
# lmm1m<-lm(selfEffScale ~ condition +posExpBiasScale, data = x)
# summary(mediate( lmm1m, lmm1,sims = 500,boot=TRUE, treat = 'condition', mediator = 'posExpBiasScale'))
# 
# mediated.mlm <- boot( data=x, statistic=indirect.mlm, R=100, strata=x$participantId, 
#                       y="selfEffScale", x="session_int", mediator="posExpBiasScale", group.id="participantId", 
#                       between.m=F, uncentered.x=F )
# 
# indirect.mlm.summary( mediated.mlm )
# 
# set.seed(1234)
# model <- ' # direct effect
#              selfEffScale ~ c*condition:session_int
# # mediator
# posExpBiasScale ~ a*condition:session_int
# selfEffScale ~ b*posExpBiasScale
# # indirect effect (a*b)
# ab := a*b
# # total effect
# total := c + (a*b)
# '
# 
# x <- subset(x, condition!="FIFTYFIFTY")
# x$condition <- factor(x$condition, levels=c( "POSITIVE","NEUTRAL"))
# x$condition <- factor(x$condition, levels=c( "NEUTRAL","POSITIVE"))
# 
# fit <- lavaan::sem(model, data = x)
# summary(fit)
# 
# temp<-mlma(y=selfEffScale, biny=FALSE, data1=x, x=condition, levelx=1, m=posExpBiasScale, c1r=1,level=participantId)
# 
# 
# summary(lmm1)
# summary(lmm1m)
# 
# summary(lme(selfEffScale ~ condition+session_int+condition:session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit))
# summary(lme(selfEffScale ~ condition+session_int+condition:session_int+posExpBiasScale, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML",na.action=na.omit))
# #lmm1m<-lmer(growthMindScale ~ condition*session_int +posExpBiasScale*session_int+ (1 +session_int|participantId), data = x,REML = FALSE)
#   
# write.csv(lmm1, file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/posExpBiasScale.csv")
# write.csv(data.frame(lmm2$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/negExpBiasScale.csv")
# write.csv(data.frame(lmm3$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/depressionScale.csv")
# write.csv(data.frame(lmm4$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/anxietyScale.csv")
# write.csv(data.frame(lmm5$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/selfEffScale.csv")
# write.csv(data.frame(lmm6$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/growthMindScale.csv")
# write.csv(data.frame(lmm7$tTable), file="C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/optimismScale.csv")
# 
# #Mediation analysis
# x <- subset(x, !is.na(x$diffposExpBiasScale))
# md.pattern(x)
# imputed_Data <- mice(x, m=1, maxit = 200, method = 'pmm', seed = 500)
# completeData <- complete(imputed_Data,1)
# x<-completeData
# x$n<-1
# x<-spread(x, condition, n,fill = 0)
# x$condition <- factor(x$condition, levels=c( "NEUTRAL","FIFTYFIFTY","POSITIVE"))
# lmm1xy<-lme(diffdepressionScale ~ POSITIVE+session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML")
# lmm1xm<-lme(diffposExpBiasScale ~ POSITIVE+session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML")
# lmm1xmy<-lme(diffdepressionScale ~ POSITIVE+diffposExpBiasScale+session_int, random = ~1+session_int|participantId,control=ctrl, data=x, method="ML")
# summary(lmm1xy)
# summary(lmm1xm)
# summary(lmm1xmy)
# 
# res<-mediate(lmm1xm, lmm1xmy, sims = 500, treat = "POSITIVE", mediator = "diffposExpBiasScale",robustSE = TRUE)
# 
# lmm1<-lme(anxietyScale ~ condition+session_int, random = ~1+session_int|participantId,control=ctrl, data=x[!is.na(x$diffanxietyScale)&!is.na(x$diffposExpBiasScale),], method="ML")
# lmm1m<-lme(anxietyScale ~ condition+diffposExpBiasScale+session_int, random = ~1+session_int|participantId,control=ctrl, data=x[!is.na(x$diffanxietyScale)&!is.na(x$diffposExpBiasScale),], method="ML")
# summary(lmm1)
# summary(lmm1m)
# res<-mediate(lmm1m, lmm1, sims = 500,boot=TRUE, treat = "condition", mediator = "diffposExpBiasScale")
# 
# lmm1<-lme(diffposExpBiasScale ~ condition+session_int, random = ~1+session_int|participantId,control=ctrl, data=x[!is.na(x$diffposExpBiasScale),], method="ML")
# lmm1m<-lme(diffselfEffScale ~ condition+diffposExpBiasScale+session_int, random = ~1+session_int|participantId,control=ctrl, data=x[!is.na(x$diffselfEffScale)&!is.na(x$diffposExpBiasScale),], method="ML")
# summary(lmm1)
# summary(lmm1m)
# res<-mediate(lmm1m, lmm1, sims = 500,boot=TRUE, treat = "condition", mediator = "diffposExpBiasScale")
# 
# sobel.lme(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffdepressionScale,grpid=x$participantId)
# sobel(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffdepressionScale)
# sobel(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffanxietyScale)
# sobel(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffselfEffScale )
# sobel(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffgrowthMindScale)
# sobel(pred=x$POSITIVE,med=x$diffposExpBiasScale,out=x$diffoptimismScale)
# 
# f <- function(x, name) {
# last<-NA
# for (row in 1:nrow(x)) {
#   session <- x[row, "session_int"]
#   variable  <- x[row, name]
#   if (!is.na(last) & !is.na(variable)){
#     diff <- variable -last
#     print(paste("On",session,"and variable", variable, "the difference is", diff))
#     x[row, paste("diff", name, sep="")]<-diff
#   }else{
#     print(paste("On",session,"and variable", variable, "the stock price was", last))
#     x[row, paste("diff", name, sep="")]<-NA
#   }
#   if(!is.na(variable)){
#     last <- variable
#   }
#   if(session==6){
#     last<-NA
#   }
# 
# }
# return (x)
# }
# 
# 
# 



