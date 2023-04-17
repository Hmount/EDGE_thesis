#### traits analysis ####

#### data and packages + cleaning ####
library(tidyverse)

NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
CWM_sitedata <- read.csv("data/CWM_sitedata.csv") #CWM trait data
trtdata<-CWM_sitedata[,c(1,2,8,12)] #just CWM variables I will use
trtdata[,c(3)]<-log(trtdata[,c(3)]) #log CWM traits 
trtdata[,4]<-log(abs(trtdata[,4])) #abs then log for TLP.x
alldat <- merge(NEWallsite, trtdata, by = c("species", "grassland_type"))
alldat[,c(7:15,17)]<-log(alldat[,c(7:15,17)]) #log focal traits


#### basic stats and relationships ####
# how many populations have data?
sum(complete.cases(NEWallsite[,c(6:16)])) #20 species have all rows
trtcnt <- rowSums(!is.na(NEWallsite[,c(6:16)])) #count for each row w/ traits filled in
sum(trtcnt == 0) #12 have no trait data 
sum(trtcnt > 5) #59 have more than 5 traits
(sum(trtcnt > 5)/sum(trtcnt >= 0))*100 #52% of data has more than 5 traits

# are SLA and TLP correlated?
cor.test(alldat$SLA.x,alldat$TLP.x) # 48% corr
cor.test(alldat$SLA,alldat$TLP) # not corr
summary(mod<-lm(SLA.x~TLP.x*grassland_type, alldat))
sqrt(.9915)
anova(mod)
cor.test(alldat$SLA.x,alldat$SLA) # a little corr
cor.test(alldat$TLP.x,alldat$TLP) # pretty corr

#view:
ggplot(alldat, aes(x=TLP.x, y=SLA.x, color=grassland_type))+ 
  geom_point()

# do CWMs differ between grasslands? by both traits? 
summary(aov(SLA.x ~ grassland_type, alldat)) # Yes
ggplot(alldat, aes(y=SLA.x, x=grassland_type, color=grassland_type))+ 
  geom_boxplot()
summary(aov(TLP.x ~ grassland_type, alldat)) # Yes
ggplot(alldat, aes(y=TLP.x, x=grassland_type, color=grassland_type))+ 
  geom_boxplot()

#### modelling; traits as predictors of population growth ####
# models first included focal species traits, interacting with grassland, and the 
# additive effects of CWM SLA and TLP. 
# CWM were never signifigant (only )
# LDMC
summary(lm(invasionLDGRcon ~ LDMC*grassland_type+SLA.x+TLP.x, data=alldat)) #** 35%
anova(lm(invasionLDGRcon ~ LDMC*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, interaction
summary(lm(intrinsicLDGRchr ~ LDMC*grassland_type+SLA.x+TLP.x, data=alldat)) #* 20%
anova(lm(intrinsicLDGRchr ~ LDMC*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, interaction

# SLA
summary(lm(invasionLDGRcon ~ SLA*grassland_type+SLA.x+TLP.x, data=alldat)) #. 11%
anova(lm(invasionLDGRcon ~ SLA*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, TLP
summary(lm(intrinsicLDGRchr ~ SLA*grassland_type+SLA.x+TLP.x, data=alldat)) #nah

# LTD
summary(lm(invasionLDGRcon ~ LTD*grassland_type+SLA.x+TLP.x, data=alldat)) #nah
summary(lm(intrinsicLDGRchr ~ LTD*grassland_type+SLA.x+TLP.x, data=alldat)) #nah

# leafarea
summary(lm(invasionLDGRcon ~ leafarea*grassland_type+SLA.x+TLP.x, data=alldat)) #* 27%
anova(lm(invasionLDGRcon ~ leafarea*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, SLA, TLP
summary(lm(intrinsicLDGRchr ~ leafarea*grassland_type+SLA.x+TLP.x, data=alldat)) #nah

# leafN
summary(lm(invasionLDGRcon ~ leafN*grassland_type+SLA.x+TLP.x, data=alldat)) # nah
anova(lm(invasionLDGRcon ~ leafN*grassland_type+SLA.x+TLP.x, data=alldat)) # none
summary(lm(intrinsicLDGRchr ~ leafN*grassland_type+SLA.x+TLP.x, data=alldat)) #. 20%
anova(lm(intrinsicLDGRchr ~ leafN*grassland_type+SLA.x+TLP.x, data=alldat)) # focal, interaction

# TLP
summary(lm(invasionLDGRcon ~ TLP*grassland_type+SLA.x+TLP.x, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ TLP*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, interaction
summary(lm(intrinsicLDGRchr ~ TLP*grassland_type+SLA.x+TLP.x, data=alldat)) #. 19%
anova(lm(intrinsicLDGRchr ~ TLP*grassland_type+SLA.x+TLP.x, data=alldat)) #TLP, interaction

# rootN
summary(lm(invasionLDGRcon ~ rootN*grassland_type+SLA.x+TLP.x, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ rootN*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, TLP
summary(lm(intrinsicLDGRchr ~ rootN*grassland_type+SLA.x+TLP.x, data=alldat)) #. 27%
anova(lm(intrinsicLDGRchr ~ rootN*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, TLP

# rootdiam
summary(lm(invasionLDGRcon ~ rootdiam*grassland_type+SLA.x+TLP.x, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ rootdiam*grassland_type+SLA.x+TLP.x, data=alldat)) #TLP
summary(lm(intrinsicLDGRchr ~ rootdiam*grassland_type+SLA.x+TLP.x, data=alldat)) #nah
anova(lm(intrinsicLDGRchr ~ rootdiam*grassland_type+SLA.x+TLP.x, data=alldat)) #TLP

# SRL
summary(lm(invasionLDGRcon ~ SRL*grassland_type+SLA.x+TLP.x, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ SRL*grassland_type+SLA.x+TLP.x, data=alldat)) #TLP
summary(lm(intrinsicLDGRchr ~ SRL*grassland_type+SLA.x+TLP.x, data=alldat)) #nah
anova(lm(intrinsicLDGRchr ~ SRL*grassland_type+SLA.x+TLP.x, data=alldat)) #TLP

# RTD
summary(lm(invasionLDGRcon ~ RTD*grassland_type+SLA.x+TLP.x, data=alldat)) # nah
anova(lm(invasionLDGRcon ~ RTD*grassland_type+SLA.x+TLP.x, data=alldat)) # none
summary(lm(intrinsicLDGRchr ~ RTD*grassland_type+SLA.x+TLP.x, data=alldat)) # nah
anova(lm(intrinsicLDGRchr ~ RTD*grassland_type+SLA.x+TLP.x, data=alldat)) # none

#height
summary(lm(invasionLDGRcon ~ height*grassland_type+SLA.x+TLP.x, data=alldat)) #. 13%
anova(lm(invasionLDGRcon ~ height*grassland_type+SLA.x+TLP.x, data=alldat)) #TLP
summary(lm(intrinsicLDGRchr ~ height*grassland_type+SLA.x+TLP.x, data=alldat)) #nah
anova(lm(intrinsicLDGRchr ~ height*grassland_type+SLA.x+TLP.x, data=alldat)) #TLP


# These models do not include CWM but are better overall
# LDMC
summary(lm(invasionLDGRcon ~ LDMC*grassland_type, data=alldat)) #*** 27%
summary(lm(intrinsicLDGRchr ~ LDMC*grassland_type, data=alldat)) #*** 21%
# SLA (messy, weak relationship)
summary(lm(invasionLDGRcon ~ SLA*grassland_type, data=alldat)) #* 10%
summary(lm(intrinsicLDGRchr ~ SLA*grassland_type, data=alldat)) #nah
# LTD (little to no representation of driest sites)
summary(lm(invasionLDGRcon ~ LTD*grassland_type, data=alldat)) #* 16%
summary(lm(intrinsicLDGRchr ~ LTD*grassland_type, data=alldat)) #nah
# leafarea
summary(lm(invasionLDGRcon ~ leafarea*grassland_type, data=alldat)) # not sig
summary(lm(intrinsicLDGRchr ~ leafarea*grassland_type, data=alldat)) # not sig
# leafN
summary(lm(invasionLDGRcon ~ leafN*grassland_type, data=alldat)) # none
summary(lm(intrinsicLDGRchr ~ leafN*grassland_type, data=alldat)) #. 16%
# TLP
summary(lm(invasionLDGRcon ~ TLP*grassland_type, data=alldat)) #* 18%
summary(lm(intrinsicLDGRchr ~ TLP*grassland_type, data=alldat)) #** 21%
# rootN
summary(lm(invasionLDGRcon ~ rootN*grassland_type, data=alldat)) #nah
summary(lm(intrinsicLDGRchr ~ rootN*grassland_type, data=alldat)) #nah
# rootdiam
summary(lm(invasionLDGRcon ~ rootdiam*grassland_type, data=alldat)) #nah
summary(lm(intrinsicLDGRchr ~ rootdiam*grassland_type, data=alldat)) #nah
# SRL
summary(lm(invasionLDGRcon ~ SRL*grassland_type, data=alldat)) #nah
summary(lm(intrinsicLDGRchr ~ SRL*grassland_type, data=alldat)) #nah
# RTD
summary(lm(invasionLDGRcon ~ RTD*grassland_type, data=alldat)) #nah
summary(lm(intrinsicLDGRchr ~ RTD*grassland_type, data=alldat)) #nah
#height (more competitive to be tall in tallgrass and short in shortgrass)
summary(lm(invasionLDGRcon ~ height*grassland_type, data=alldat)) #* 14% 
summary(lm(intrinsicLDGRchr ~ height*grassland_type, data=alldat)) #nah



ggplot(alldat, aes(y=intrinsicLDGRchr, x=rootN, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")+
  ylim(-4,4)
#TLP
summary(lm(invasionLDGRcon ~ TLP*grassland_type, data=alldat)) #. 14% (not with CWM's)
anova(lm(invasionLDGRcon ~ TLP*grassland_type, data=traits)) #focal, grassland
ggplot(traits, aes(y=invasionLDGRcon, x=TLP, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ TLP*grassland_type, data=alldat)) #. 16% (* 23%)
anova(lm(intrinsicLDGRchr ~ TLP*grassland_type+SLA.x+TLP.x, data=alldat[-44,])) #focal, interaction (interaction, TLP)
ggplot(alldat, aes(y=intrinsicLDGRchr, x=TLP, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

#test
summary(lm(invasionLDGRcon ~ leafN*grassland_type, data=alldat)) #* 16%
ggplot(alldat, aes(y=invasionLDGRcon, x=leafN, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm", se=F)
summary(lm(intrinsicLDGRchr ~ leafN*grassland_type, data=alldat)) #* 16%
anova(lm(intrinsicLDGRchr ~ leafN*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, TLP
ggplot(alldat, aes(y=intrinsicLDGRchr, x=leafN, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")+
  ylim(-5,5)
summary(lm(invasionLDGRcon ~ height*grassland_type, data=alldat)) #* 31%
anova(lm(invasionLDGRcon ~ leafarea*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, sla, tlp
ggplot(alldat, aes(y=invasionLDGRcon, x=leafarea, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ leafarea*grassland_type+SLA.x+TLP.x, data=alldat)) #. 21%
ggplot(alldat, aes(y=intrinsicLDGRchr, x=leafarea, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRcon ~ SLA*grassland_type, data=traits)) #* 14%
anova(lm(invasionLDGRcon ~ leafarea*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, sla, tlp
ggplot(traits, aes(y=invasionLDGRcon, x=SLA, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ SLA*grassland_type, data=traits)) # no
ggplot(traits, aes(y=intrinsicLDGRchr, x=SLA, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

####
#### modelling; traits as predictors of population responses ####
# use multivariate linear model to see if responses are a function of focal traits* grassland + community CWM SLA and TLP
# run MOANOVA with significant model objects to test for significance of predictors to response interaction
summary(modt<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~LDMC *grassland_type + SLA.x + TLP.x, data=alldat)) #no
car::Anova(modt) #trait and interaction ***
summary(mod_ln<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~leafN *grassland_type + SLA.x + TLP.x, data=alldat)) #**
car::Anova(mod_ln) #nothin
summary(mod_la<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~leafarea *grassland_type + SLA.x + TLP.x, data=alldat)) #**
car::Anova(mod_la) #focal, grassland, cwms ***
summary(modt<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~LTD *grassland_type + SLA.x + TLP.x, data=alldat)) #no
car::Anova(modt) # not enough sites, exclude from analysis
summary(mod_sla<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~SLA *grassland_type + SLA.x + TLP.x, data=alldat)) #**
car::Anova(mod_sla) #only TLP, not great model
summary(mod_tlp<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~TLP*grassland_type + SLA.x + TLP.x, data=alldat)) #**
car::Anova(mod_tlp) #no?
summary(modt<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~height *grassland_type + SLA.x + TLP.x,data=alldat)) #no
car::Anova(modt) #nah, grassland and TLP
summary(modt<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~RTD *grassland_type + SLA.x + TLP.x, data=alldat)) #no
car::Anova(modt) # not enough sites, exclude from analysis
summary(modt<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~SRL *grassland_type + SLA.x + TLP.x , data=alldat)) #close
car::Anova(modt) #focal, TLP (univariate not sig tho)
summary(modt<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~rootdiam *grassland_type + SLA.x + TLP.x, data=alldat)) #no
car::Anova(modt) #nah, just TLP
summary(modt<-lm(cbind(intrinsicLDGRchr, invasionLDGRcon)~rootN *grassland_type + SLA.x + TLP.x, data=alldat)) #no
car::Anova(modt) #nah, just TLP

