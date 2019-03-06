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
library(r2glmm)

writeresults <- function(imp,path,type,lev,ptype,hascondition){
  print(paste0("Current working dir: ",path))
  sink(file=path)
  lev<-c( "NEUTRAL","FIFTYFIFTY","POSITIVE")
  type<-'treatment'
  ptype<-Completers
  hascondition<-TRUE
  imp<-impList2
  for(outcome in list("posExpBiasScale","negExpBiasScale","depressionScale", "anxietyScale","selfEffScale","growthMindScale","optimismScale")){
    x<-imp    
    for(i in 1:length(x))
        {
          if(hascondition){x[[i]]$condition <- factor(x[[i]]$condition, levels=lev)}else{x[[i]]<-subset(x[[i]], condition==lev[1])}
          x[[i]] <- subset(x[[i]], participantId %in% ptype$participantId)
            #impx[which(impx$participantId %in% ITT$participantId), ]
          
            # if (type=='treatment'){
            #   x[[i]] <- subset(x[[i]], session_int!=5)
            #   if (outcome!="posExpBiasScale"& outcome!="negExpBiasScale"){x[[i]] <- subset(x[[i]], session_int!=1 & session_int!=3)}
            # }else{
            #   x[[i]] <- subset(x[[i]], session_int==4 |session_int==5)
            #   x[[i]]$session_int <- x[[i]]$session_int-4
            # }
          
        }
    if(hascondition){fml=as.formula(paste(outcome, "~condition*session_int"))}else{fml=as.formula(paste(outcome, "~session_int"))}
    print(fml)  
     # if ((outcome=="selfEffScale"&lev==c("POSITIVE_NEGATION"))|(outcome=="growthMindScale"&lev==c("FIFTY_FIFTY_BLOCKED"))){
     #   print(testEstimates(with(x, lme(fml, random = ~1|participantId,control=ctrl, method="ML")), var.comp=TRUE))
     # }else{
      m<-testEstimates(with(x, lme(fml, random = ~1+session_int|participantId,control=ctrl, method="ML")), var.comp=TRUE)
      Plot.Model.F.Linearity<-plot(resid(m),outcome)
       # }
      ctrl
      
      #unique(x[[100]]$session_int)
      print("----------------------------------------------------------------------------")
  }
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
  x$session_int <-x$session_int-1
  x<-aggregate(x=x[c("posExpBiasScale", "negExpBiasScale","depressionScale","anxietyScale","selfEffScale","growthMindScale","optimismScale")], by=list(participantId=x$participantId,session=x$session,condition=x$condition,session_int=x$session_int), mean, na.rm = TRUE)
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  x[is.nan(x)] <- NA
  return(x)
}

refactor <- function(x,lev){
  for(i in 1:length(x))
  {
    x[[i]]$condition <- factor(x[[i]]$condition, levels=lev)
  }
  return(x)
}

unique(x$session_int)
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
Completers<-subset(p, txCompSample==1)

#Multiple imputation  for multilevel data Using the pan package
fml <- posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale  ~ condition + session_int + condition*session_int + (1+session_int|participantId)
imp <- panImpute(x, formula=fml, n.burn=10000, n.iter=100, m=100, seed=1234)
impList <- mitmlComplete(imp, print="all")
#This si to summurize and plot the results of the imputation procedure
#summary(imp)
#plot(imp, trace="all", print="beta", pos=c(1,2))

#ITT
ittx<-impx[which(impx$participantId %in% ITT$participantId), ]
# Treament phase
#z <- subset(ittx, session_int!=5)

writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/2conditions_vs_Neutral.txt','treatment',c( "NEUTRAL","FIFTYFIFTY","POSITIVE"),ITT,TRUE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/2conditions_vs_50_50.txt','treatment',c( "FIFTYFIFTY","POSITIVE","NEUTRAL"),ITT,TRUE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/Neutral.txt','treatment',c( "NEUTRAL"),ITT,FALSE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/FIFTYFIFTY.txt','treatment',c("FIFTYFIFTY"),ITT,FALSE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/POSITIVE.txt','treatment',c("POSITIVE"),ITT,FALSE)

# Follow-up phase
#y <- subset(ittx, session_int==4 |session_int==5)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/2conditions_vs_Neutral.txt','follow-up',c( "NEUTRAL","FIFTYFIFTY","POSITIVE"),ITT,TRUE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/2conditions_vs_50_50.txt','follow-up',c( "FIFTYFIFTY","POSITIVE","NEUTRAL"),ITT,TRUE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/Neutral.txt','follow-up',c( "NEUTRAL"),ITT,FALSE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/FIFTYFIFTY.txt','follow-up',c("FIFTYFIFTY"),ITT,FALSE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/POSITIVE.txt','follow-up',c("POSITIVE"),ITT,FALSE)

#Completers

# Treament phase
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/2conditions_vs_Neutral.txt','treatment',c( "NEUTRAL","FIFTYFIFTY","POSITIVE"),Completers,TRUE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/2conditions_vs_50_50.txt','treatment',c( "FIFTYFIFTY","POSITIVE","NEUTRAL"),Completers,TRUE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/Neutral.txt','treatment',c( "NEUTRAL"),Completers,FALSE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/FIFTYFIFTY.txt','treatment',c("FIFTYFIFTY"),Completers,FALSE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/POSITIVE.txt','treatment',c("POSITIVE"),Completers,FALSE)

# Follow-up phase
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/2conditions_vs_Neutral.txt','follow-up',c( "NEUTRAL","FIFTYFIFTY","POSITIVE"),Completers,TRUE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/2conditions_vs_50_50.txt','follow-up',c( "FIFTYFIFTY","POSITIVE","NEUTRAL"),Completers,TRUE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/Neutral.txt','follow-up',c( "NEUTRAL"),Completers,FALSE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/FIFTYFIFTY.txt','follow-up',c("FIFTYFIFTY"),Completers,FALSE)
writeresults(impList,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/POSITIVE.txt','follow-up',c("POSITIVE"),Completers,FALSE)


############################################################################################################
############################################################################################################
###Longitudunal analysis with 5 conditions



x <- read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainDataScales.csv")
x <- preparedata(x)

p<-read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainAnalysisSamples.csv")
ITT<-subset(p, ittSample==1)
Completers<-subset(p, txCompSample==1)
ittx<-x[which(x$participantId %in% ITT$participantId), ]



#Get a global view on our data
summary(x)
md.pattern(x)



#Multiple imputation  for multilevel data Using the pan package
fml <- posExpBiasScale + negExpBiasScale + depressionScale + anxietyScale + selfEffScale + growthMindScale + optimismScale  ~ condition + session_int + condition*session_int + (1+session_int|participantId)
imp2 <- panImpute(x, formula=fml, n.burn=10000, n.iter=100, m=100, seed=1234)
impList2 <- mitmlComplete(imp2, print="all")



#ITT
ittx<-impx[which(impx$participantId %in% ITT$participantId), ]
# Treament phase

writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/4conditions_vs_50_50Random.txt','treatment',c( "FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED","NEUTRAL","POSITIVE_NEGATION","POSITIVE"),ITT,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/4conditions_vs_Positive.txt','treatment',c("POSITIVE","POSITIVE_NEGATION","NEUTRAL","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"),ITT,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/4conditions_vs_NEUTRAL.txt','treatment',c("NEUTRAL","POSITIVE","POSITIVE_NEGATION","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"),ITT,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/POSITIVE_NEGATION.txt','treatment',c("POSITIVE_NEGATION"),ITT,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/POSITIVE_ACTIVE.txt','treatment',c("POSITIVE"),ITT,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/FIFTY_FIFTY_RANDOM.txt','treatment',c("FIFTY_FIFTY_RANDOM"),ITT,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/ITT/FIFTY_FIFTY_BLOCKED.txt','treatment',c("FIFTY_FIFTY_BLOCKED"),ITT,FALSE)

# Follow-up phase

writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/4conditions_vs_50_50Random.txt','follow-up',c( "FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED","NEUTRAL","POSITIVE_NEGATION","POSITIVE"),ITT,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/4conditions_vs_Positive.txt','follow-up',c("POSITIVE","POSITIVE_NEGATION","NEUTRAL","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"),ITT,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/4conditions_vs_NEUTRAL.txt','follow-up',c("NEUTRAL","POSITIVE","POSITIVE_NEGATION","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"),ITT,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/POSITIVE_NEGATION.txt','follow-up',c("POSITIVE_NEGATION"),ITT,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/POSITIVE_ACTIVE.txt','follow-up',c("POSITIVE"),ITT,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/FIFTY_FIFTY_RANDOM.txt','follow-up',c("FIFTY_FIFTY_RANDOM"),ITT,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/ITT/FIFTY_FIFTY_BLOCKED.txt','follow-up',c("FIFTY_FIFTY_BLOCKED"),ITT,FALSE)


#Completers
# Treament phase

writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/4conditions_vs_50_50Random.txt','treatment',c( "FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED","NEUTRAL","POSITIVE_NEGATION","POSITIVE"),Completers,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/4conditions_vs_Positive.txt','treatment',c("POSITIVE","POSITIVE_NEGATION","NEUTRAL","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"),Completers,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/4conditions_vs_NEUTRAL.txt','treatment',c("NEUTRAL","POSITIVE","POSITIVE_NEGATION","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"),Completers,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/POSITIVE_NEGATION.txt','treatment',c("POSITIVE_NEGATION"),Completers,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/POSITIVE_ACTIVE.txt','treatment',c("POSITIVE"),Completers,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/FIFTY_FIFTY_RANDOM.txt','treatment',c("FIFTY_FIFTY_RANDOM"),Completers,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Treatment phase/Completers/FIFTY_FIFTY_BLOCKED.txt','treatment',c("FIFTY_FIFTY_BLOCKED"),Completers,FALSE)

# Follow-up phase

writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/4conditions_vs_50_50Random.txt','follow-up',c( "FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED","NEUTRAL","POSITIVE_NEGATION","POSITIVE"),Completers,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/4conditions_vs_Positive.txt','follow-up',c("POSITIVE","POSITIVE_NEGATION","NEUTRAL","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"),Completers,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/4conditions_vs_NEUTRAL.txt','follow-up',c("NEUTRAL","POSITIVE","POSITIVE_NEGATION","FIFTY_FIFTY_RANDOM","FIFTY_FIFTY_BLOCKED"),Completers,TRUE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/POSITIVE_NEGATION.txt','follow-up',c("POSITIVE_NEGATION"),Completers,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/POSITIVE_ACTIVE.txt','follow-up',c("POSITIVE"),Completers,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/FIFTY_FIFTY_RANDOM.txt','follow-up',c("FIFTY_FIFTY_RANDOM"),Completers,FALSE)
writeresults(impList2,'C://Users/mob3f/Documents/MindTrials Future Thinking/results/Longitudinal Outcome/Follow-up phase/Completers/FIFTY_FIFTY_BLOCKED.txt','follow-up',c("FIFTY_FIFTY_BLOCKED"),Completers,FALSE)





