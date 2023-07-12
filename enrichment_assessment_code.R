rm(list=ls())
library(tidyverse)
library(rstatix)
library(nlme)
library(lmerTest)
library(lme4)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)


data<-read.csv("tort_scrtch.csv", sep = ",", header = T)
head(data)

##wilcox test to look for difference between exposed resting and 'anchored' resting
test<-wilcox.test(data$rest.enrich, data$rest.no.enrich, paired = TRUE, alternative = "two.sided")

test

t<-read.csv("acc_beh.csv", sep = ",", header = T)
head(t)
#format

t$treat[t$treat=="acc"]<-"1"
t$treat[t$treat=="no_acc"]<-"0"
t$beh<-as.factor(t$beh)
t$treat<-as.factor(t$treat)
t$turtle<-as.factor(t$turtle)
str(t)

levels(t$beh)

#run linear model to look at if ACC affects enrichment use. also provides variance across turtles.

f<-lmer(measurement~treat*beh+ (1|turtle), data=t)
summary(f)

#graphs#

library(ggplot2)

cleanup=theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background=element_blank(),
              axis.line=element_line(color = "black"))

#data in long format for lm
data_long <- gather(data, obj, measurement, acc_bs:totrest_noacc, factor_key=TRUE)
data_long

enrich<-data_long %>% filter(obj == 'tot_bs'| obj == 'pipe'| obj == 'b_stat'| obj == 'b_other'| obj == 'shelt_hid'| obj == 'shelt_other'| obj == 'shelt_stat'| obj == 'other')


enrich$turtle<-as.factor(enrich$turtle)
enrich$obj<-as.character(enrich$obj)
enrich$obj[enrich$obj== "tot_bs"]<- "Scratching with brush"
enrich$obj[enrich$obj== "pipe"]<- "Resting under pipe"
enrich$obj[enrich$obj== "b_stat"]<- "Resting under brush"
enrich$obj[enrich$obj== "b_other"]<- "Brush other"
enrich$obj[enrich$obj== "shelt_hid"]<- "Under shelter"
enrich$obj[enrich$obj== "shelt_other"]<- "Shelter other"
enrich$obj[enrich$obj== "shelt_stat"]<- "Resting at shelter"
enrich$obj[enrich$obj== "other"]<- "Other"

enrich$obj <- factor(enrich$obj, levels=c('Other', 'Resting under pipe','Under shelter', 'Shelter other' , 'Resting at shelter', 'Scratching with brush','Resting under brush','Brush other'))

levels(enrich$obj)


windowsFonts(Times=windowsFont("Times New Roman"))

ggplot(enrich, aes(fill=obj, y=measurement, x=turtle)) + 
  geom_bar(position="stack", stat="identity")+
  cleanup+
  coord_flip()+
  scale_fill_manual(name="Behaviour", values=c("gold","darkorange","firebrick1", "seagreen1","lightseagreen",
                                               "slateblue4","hotpink2","grey"), breaks=c('Brush other', 'Resting under brush', 'Scratching with brush', 'Resting at shelter', 'Shelter other','Under shelter', 'Resting under pipe','Other'))+
  scale_x_discrete(labels=c("3" = "1", "4" = "2",
                            "5" = "3", "6"="4", "7"="5", "8"="6"), name= "Turtle")+
  scale_y_continuous(name = "Behavioural allocations (%)", breaks=seq(0,100,10))+
  theme(text=element_text(family="Times", size=12)) 


##graph for exposed vs assisted resting

#remove unecessary columns from data
data_long %>% filter(obj == 'acc_shelt'| obj == 'no.acc_shelt'| obj == 'acc_bs'| obj == 'no.acc_bs'| obj== 'acc_bt'| obj == 'no.acc_bt'| obj == 'totrest_acc' | obj == 'totrest_noacc')

rest<-data_long %>% filter(obj == 'rest.enrich'| obj == 'rest.no.enrich')

###rest###

ggplot(rest, aes(y=measurement, x=obj, fill=obj)) + 
  geom_boxplot()+
  xlab("Resting") +
  ylab("Percent time")+
  scale_x_discrete(labels=c("rest.enrich" = "Assisted (under enrichment)",
                            "rest.no.enrich" = "Exposed"))+
  theme(legend.position="none") +
  scale_fill_manual(name="Behaviour", values=c("coral",
                                               "brown2"))+
  cleanup

