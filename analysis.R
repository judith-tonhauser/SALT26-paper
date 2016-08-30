# created by Judith Tonhauser
# code for generating the figures and analyses reported in 
# Tonhauser, Judith. (2016) Prosodic cues to presupposition projection. SALT 26 proceedings.


library(grid)
library(ggplot2)
theme_set(theme_bw())

library(ordinal)

library(dplyr)

# Set the path, load the data and helper functions

setwd('/Users/judith/Documents/current-research-topics/NSF-NAI/prop-att-experiments/5-prosody-factives/3-perception-1a/github-materials/')
d1 <- readRDS("d1.RData") # Experiment 1
d2 <- readRDS("d2.RData") # Experiment 2
d3 <- readRDS("d3.RData") # Experiment 3
source('helpers.R')

################## Figures #####################

#### Left panel of Figure 3
mean.by.worker <- d1 %>%
  dplyr::select(workerid,Response,prosody) %>%
  group_by(workerid,prosody) %>%
  summarise(Mean = mean(Response))
mean.by.worker

d1$Response <- as.numeric(d1$Response)
agr = aggregate(Response ~ prosody, data=d1, FUN="mean")
agr$CILow = aggregate(Response ~ prosody, data=d1, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ prosody, data=d1, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=prosody,y=Response),xpd=FALSE) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",fill="white",color="black",show_guide=F,position=dodge) +
  geom_point(data=mean.by.worker, aes(y=Mean), color="grey60",width = 1, height = 0) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) + 
  scale_x_discrete(breaks=c("H-Pred", "LH-Cont-CC"), labels=c("H* on predicate", "L+H* on content")) +
  #geom_line(data=mean.by.worker,aes(y=Mean,group=workerid))
  #theme(axis.text=element_text(size=16), axis.title=element_text(size=22))+
  xlab("Prosody") +
  ylab("Mean certainty rating") +
  scale_y_discrete(breaks=seq(1,7))


#### Right panel of Figure 3
mean.by.worker <- d2 %>%
  dplyr::select(workerid,Response,prosody) %>%
  group_by(workerid,prosody) %>%
  summarise(Mean = mean(Response))
mean.by.worker

d2$Response <- as.numeric(d2$Response)
agr = aggregate(Response ~ prosody, data=d2, FUN="mean")
agr$CILow = aggregate(Response ~ prosody, data=d2, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ prosody, data=d2, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=prosody,y=Response),xpd=FALSE) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",fill="white",color="black",show_guide=F,position=dodge) +
  geom_point(data=mean.by.worker, aes(y=Mean), color="grey60",width = 1, height = 0) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) + 
  scale_x_discrete(breaks=c("H-Pred", "LH-Pron-CC"), labels=c("H* on predicate", "L+H* on pronoun")) +
  #geom_line(data=mean.by.worker,aes(y=Mean,group=workerid))
  #theme(axis.text=element_text(size=16), axis.title=element_text(size=22))+
  xlab("Prosody") +
  ylab("Mean certainty rating") +
  scale_y_discrete(breaks=seq(1,7))

#### Figure 5
mean.by.worker <- d3 %>%
  dplyr::select(workerid,Response,prosody) %>%
  group_by(workerid,prosody) %>%
  summarise(Mean = mean(Response))
mean.by.worker

d3$Response <- as.numeric(d3$Response)
agr = aggregate(Response ~ prosody, data=d3, FUN="mean")
agr$CILow = aggregate(Response ~ prosody, data=d3, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ prosody, data=d3, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=prosody,y=Response),xpd=FALSE) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",fill="white",color="black",show_guide=F,position=dodge) +
  geom_point(data=mean.by.worker, aes(y=Mean), color="grey60",width = 1, height = 0) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) + 
  scale_x_discrete(breaks=c("downHstar", "Hstar"), labels=c("(L+)H* !H*", "H* (L+)H*")) +
  #geom_line(data=mean.by.worker,aes(y=Mean,group=workerid))
  #theme(axis.text=element_text(size=16), axis.title=element_text(size=22))+
  xlab("Prosody") +
  ylab("Mean certainty rating") +
  scale_y_discrete(breaks=seq(1,7))

############### Ordinal regression models  ################

# Experiment 1 
d1$Response <- as.factor(d1$Response)
d1$workerid <- as.factor(d1$workerid)
model <- clmm(Response ~ prosody + (1+prosody|workerid) + (1|utterance), data=d1)
summary(model)

# Experiment 2 
d2$Response <- as.factor(d2$Response)
d2$workerid <- as.factor(d2$workerid)
model <- clmm(Response ~ prosody + (1+prosody|workerid) + (1|utterance), data=d2)
summary(model)

# Experiment 3 
d3$Response <- as.factor(d3$Response)
d3$workerid <- as.factor(d3$workerid)
model <- clmm(Response ~ prosody + (1+prosody|workerid) + (1|utterance), data=d3)
summary(model)




