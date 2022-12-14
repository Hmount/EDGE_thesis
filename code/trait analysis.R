#### traits analysis ####

#### data and packages + cleaning ####
library(tidyverse)

CWM_sitedata <- read.csv("data/CWM_sitedata.csv") #trait data

trtdata<-CWM_sitedata[,c(1:13,27:37,39,41:48)] #just variables I will use
trtdata[,c(3:11,13,16:25,33)]<-log(trtdata[,c(3:11,13,16:25,33)]) #log focal and CWM traits 
trtdata[,12]<-log(abs(trtdata[,12])) #abs then log for TLP.x
trtdata <- trtdata %>% mutate(TLP_tran_fig = TLP_tran*-1,
                              TLP.x_fig = TLP.x*-1)  #make neg. to interpret


#### basic stats and relationships ####
# how many populations have data?
sum(complete.cases(NEWallsite[,c(6:16)])) #21 species have all rows
trtcnt <- rowSums(!is.na(NEWallsite[,c(6:16)])) #count for each row w/ traits filled in
sum(trtcnt == 0) #20 have no trait data 
sum(trtcnt > 5) #58 have more than 5 traits
(sum(trtcnt > 5)/sum(trtcnt >= 0))*100 #50% of data has more than 5 traits

# do CWMs differ between grasslands? by both traits? (yes and yes)
summary(aov(SLA.x ~ grassland_type, trtdata))
summary(aov(TLP.x ~ grassland_type, trtdata))
summary(mod<-lm(SLA.x~TLP.x*grassland_type, trtdata))
anova(mod)
#view:
ggplot(trtdata, aes(x=TLP.x, y=SLA.x, color=grassland_type))+ 
  geom_point()

# does intrinsicdiff change with CWM SLA alone or by grassland? (no)
ggplot(trtdata, aes(x=SLA.x, y=intrinsicdiff))+ 
  geom_point()
summary(lm(intrinsicdiff~SLA.x, trtdata))


#### modelling ####
# use multivariate linear model to see if responses are a function of focal traits* grassland + community CWM SLA and TLP
# run MOANOVA with significant model objects to test for significance of predictors to response interaction

summary(mod_la<-lm(cbind(intrinsicdiff, chrdiff)~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_la) 
summary(mod_ln<-lm(cbind(intrinsicdiff, chrdiff)~leafN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_ln) 
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~LDMC.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) #nothin
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~LTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) # not enough sites, exclude from analysis
summary(mod_sla<-lm(cbind(intrinsicdiff, chrdiff)~SLA.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_sla)
summary(mod_tlp<-lm(cbind(intrinsicdiff, chrdiff)~TLP_tran*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_tlp) 
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~height.y*grassland_type + SLA.x + TLP.x,data=trtdata)) #no
car::Anova(modt)
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~RTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) # not enough sites, exclude from analysis
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~SRL.y*grassland_type + SLA.x + TLP.x , data=trtdata)) #close
car::Anova(modt) 
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~rootdiam.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) 
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~rootN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) 


