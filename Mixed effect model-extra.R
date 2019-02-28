
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


impx$changepos<-impx$posExpBiasScale-x$negExpBiasScale
base<-impx[which(impx$session_int==1),]
names(base)[names(base)=="changepos"] <- "changeposbase"
base<-base[,c("participantId", "changeposbase"),drop=FALSE]
other<-impx[which(impx$session_int>1),]
new<-merge(other,base)
new$ratio<-(new$changepos-new$changeposbase)/(new$changeposbase+8)
new<-new[which(!is.na(new$changepos)),]
atrisk<-new[which(new$ratio< -0.5 ),]

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

