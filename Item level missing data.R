x <- read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainDataItemsScales.csv")

p<-read.csv("C://Users/mob3f/Documents/MindTrials Future Thinking/Clean data from Jeremy/FTmainAnalysisSamples.csv")
ITT<-subset(p, ittSample==1)
completers<-subset(p, txCompSample==1)

ittx<-x[which(x$participantId %in% ITT$participantId), ]

# anxietyScale = sum of nervous and worry
ittx_item<-subset(ittx,!is.na(ittx$anxietyScale))
count(subset(ittx_item,is.na(ittx_item$nervous)|is.na(ittx_item$worry)))/count(ittx_item)
# depressionScale = sum of pleasure and depressed
ittx_item<-subset(ittx,!is.na(ittx$depressionScale))
count(subset(ittx_item,is.na(ittx_item$pleasure)|is.na(ittx_item$depressed)))/count(ittx_item)
# posExpBiasScale = mean of shortRest, settleIn, consideredAdvancement, and financiallySecure
ittx_item<-subset(ittx,!is.na(ittx$posExpBiasScale))
count(subset(ittx_item,is.na(ittx_item$shortRest)|is.na(ittx_item$settleIn)|is.na(ittx_item$consideredAdvancement)|is.na(ittx_item$financiallySecure)))/count(ittx_item)
# negExpBiasScale = mean of verySick, offend, stuck, and ruining
ittx_item<-subset(ittx,!is.na(ittx$negExpBiasScale))
count(subset(ittx_item,is.na(ittx_item$verySick)|is.na(ittx_item$offend)|is.na(ittx_item$stuck)|is.na(ittx_item$ruining)))/count(ittx_item)
# growthMindScale = mean of learnRev, particularThinking, and alwaysChangeThinking
ittx_item<-subset(ittx,!is.na(ittx$growthMindScale))
count(subset(ittx_item,is.na(ittx_item$learnRev)|is.na(ittx_item$particularThinking)|is.na(ittx_item$alwaysChangeThinking)))/count(ittx_item)
# selfEffScale = mean of difficultTasks, performEffectively, and compared
ittx_item<-subset(ittx,!is.na(ittx$selfEffScale))
count(subset(ittx_item,is.na(ittx_item$difficultTasks)|is.na(ittx_item$performEffectively)|is.na(ittx_item$compared)))/count(ittx_item)
# optimismScale = mean of wrongWillRev and hardlyEverRev
ittx_item<-subset(ittx,!is.na(ittx$optimismScale))
count(subset(ittx_item,is.na(ittx_item$wrongWill)|is.na(ittx_item$hardlyEverRev)))/count(ittx_item)

summary(xx)
md.pattern(xx)