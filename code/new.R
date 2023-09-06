## Redoing analysis using LDGR's

#### data and packages ####
library(tidyverse)
library(ggpubr)

NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))


#### relationship between growth rates ####
# this model (probably most interesting) shows the relationship between LDGR with low
# heterospecific density in drought (growth rate as effected by drought alone) and the 
# relationship between LDGR with average heterospecific density in ambient (growth 
# rate as effected by neighbors alone) ~~SHOULD THIS BE HIGH DENSISTY??~~
summary(lm(invasionLDGRcon~intrinsicLDGRchr, NEWallsite))
anova(m1<-lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type, NEWallsite)) #does differ by grassland
emmeans(m1, specs = pairwise~grassland_type) #but not super significant (potentially dry grasslands do not share the trend as much)
#plot
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()
# positive relationship suggests being better at living in drought is correlated with
# that species doing better at living with more neighbors
# below 1:1 line growth rate is higher in drought
# above 1:1 line growth rate is higher with neighbors

#interactive
library(plotly)
p1 <- ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, label=species))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()
ggplotly(p1)
pg1 <- ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=grass.forb, label=species))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()
ggplotly(pg1)
pg1 <- ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=grassland_type, label=species))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_abline()
ggplotly(pg1)

#view one species
onespecies <- NEWallsite %>% mutate(onespecies = ifelse(species=="Tragopogondubius","yes","no")) 
onespecies <- NEWallsite %>% filter(species == "Boutelouagracilis")
ggplot(onespecies, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=grassland_type))+
  geom_point()+
  geom_abline()
#it seems like as precipitation increases growth increases, but the same relationship is maintained

#subset species that have response estimates from multiple grasslands
multiples <- as.data.frame(table(NEWallsite$species))
multiples <- multiples%>%filter(Freq>=2) #get spp w/ 2+ observations
multiples #check
allmultiples <- NEWallsite %>% filter(species%in%multiples$Var1) #subset

p2<-ggplot(allmultiples, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=grassland_type, label=species))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()
ggplotly(p2)


# this model (also interesting) shows the relationship between LDGR with low
# heterospecific density in drought (growth rate as effected by drought alone) and the 
# relationship between LDGR with average heterospecific density in drought (growth 
# rate as effected by neighbors after accounting for drought) ~~SHOULD THIS BE HIGH DENSISTY??~~
summary(lm(invasionLDGRchr~intrinsicLDGRchr, NEWallsite))
anova(m1<-lm(invasionLDGRchr~intrinsicLDGRchr*grassland_type, NEWallsite)) #does differ by grassland
emmeans(m1, specs = pairwise~grassland_type) #but not super significant (potentially dry grasslands do not share the trend as much)
#plot
ggplot(NEWallsite, aes(y=invasionLDGRchr, x=intrinsicLDGRchr, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()
# positive relationship suggests being better at living in drought is correlated with
# that species doing better at living with more neighbors in drought
# below 1:1 line growth rate is higher without neighbors
# above 1:1 line growth rate is higher with neighbors

###this is the main plot to use difference in perfect condition verus abiotic verus biotic
summary(lm(invasionLDGRchr~intrinsicLDGRchr, NEWallsite)) #***
anova(lm(invasionLDGRchr~intrinsicLDGRchr*grassland_type, NEWallsite)) #no interaction, consistent
ggplot(NEWallsite, aes(y=invasionLDGRchr, x=intrinsicLDGRcon, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()

#### relationship between growth rate in drought with neighbors and traits ####
data <- alldat[-44,]
#Root N
summary(lm(invasionLDGRchr ~ rootN, data=alldat)) #**
ggplot(alldat, aes(y=invasionLDGRchr, x=rootN))+
  geom_point()+
  geom_smooth(method="lm")
#SRL
summary(lm(invasionLDGRchr ~ SRL*grassland_type+SLA.x+TLP.x, data=alldat[-53,])) #**
ggplot(alldat[-53,], aes(y=invasionLDGRchr, x=SRL))+
  geom_point()+
  geom_smooth(method="lm")
#Leaf N
summary(lm(invasionLDGRchr ~ leafN, data=alldat)) #**
ggplot(alldat, aes(y=invasionLDGRchr, x=leafN))+
  geom_point()+
  geom_smooth(method="lm")
#LDMC (lifespan and grassland type improves)
summary(lm(invasionLDGRchr ~ LDMC*lifespan, data=alldat)) #**
ggplot(alldat, aes(y=invasionLDGRchr, x=LDMC, color=lifespan))+
  geom_point()+
  geom_smooth(method="lm")
ldmcplot <- ggplot(alldat, aes(y=invasionLDGRchr, x=LDMC, color=grass.forb, label=species))+
  geom_point()+
  geom_smooth(method="lm")
ggplotly(ldmcplot)
#TLP (grassland type tho!)
summary(lm(invasionLDGRchr ~ TLP*grassland_type, data=alldat[-44,])) #**
ggplot(alldat[-44,], aes(y=invasionLDGRchr, x=TLP, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")


#rootdiam?
summary(lm(invasionLDGRchr ~ rootdiam, data=alldat)) #**
ggplot(alldat, aes(y=invasionLDGRchr, x=rootdiam))+
  geom_point()+
  geom_smooth(method="lm")
#LTD?
summary(lm(invasionLDGRchr ~ LTD, data=alldat)) #**
ggplot(alldat, aes(y=invasionLDGRchr, x=LTD))+
  geom_point()+
  geom_smooth(method="lm")



##tests
#effect of drought
summary(lm(intrinsicLDGRchr~intrinsicLDGRcon, NEWallsite)) #**
anova(lm(intrinsicLDGRchr~intrinsicLDGRcon*grassland_type, NEWallsite)) #no interaction, consistent
ggplot(NEWallsite, aes(y=intrinsicLDGRchr, x=intrinsicLDGRcon, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()
#effect of neighbors
summary(lm(invasionLDGRcon~intrinsicLDGRcon, NEWallsite)) #***
anova(lm(invasionLDGRcon~intrinsicLDGRcon*grassland_type, NEWallsite)) #***
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRcon, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()


#### making all of the possible plots/options ####
## option 1
## correlation between growth rate in ideal conditions and natural conditions (biotic and abiotic effects)
#both versus neither
summary(lm(invasionLDGRchr~intrinsicLDGRcon, NEWallsite)) #**
summary(lm(invasionLDGRchr~intrinsicLDGRcon, weights = weight2, NEWallsite[-106,]))
cor.test(NEWallsite$invasionLDGRchr,NEWallsite$intrinsicLDGRcon)
anova(lm(invasionLDGRchr~intrinsicLDGRcon*grassland_type, NEWallsite)) #no interaction, consistent
ggplot(NEWallsite, aes(y=invasionLDGRchr, x=intrinsicLDGRcon, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()
#LDMC (ideal conditions describes better???)
summary(lm(intrinsicLDGRcon ~ LDMC, data=alldat)) #**
ggplot(alldat, aes(y=intrinsicLDGRcon, x=LDMC))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRchr ~ LDMC, data=alldat)) #**
ggplot(alldat, aes(y=invasionLDGRchr, x=LDMC))+
  geom_point()+
  geom_smooth(method="lm")
#root N
summary(lm(intrinsicLDGRcon ~ rootN, data=alldat)) #**
ggplot(alldat, aes(y=intrinsicLDGRcon, x=rootN))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRchr ~ rootN, data=alldat)) #.
ggplot(alldat, aes(y=invasionLDGRchr, x=rootN))+
  geom_point()+
  geom_smooth(method="lm")
#rootdiam
summary(lm(intrinsicLDGRcon ~ rootdiam, data=alldat)) #.
ggplot(alldat, aes(y=intrinsicLDGRcon, x=rootdiam))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRchr ~ rootdiam, data=alldat)) #*
ggplot(alldat, aes(y=invasionLDGRchr, x=rootdiam))+
  geom_point()+
  geom_smooth(method="lm")
#TLP
summary(lm(intrinsicLDGRcon ~ TLP, data=alldat[-44,])) #*
ggplot(alldat[-53,], aes(y=intrinsicLDGRcon, x=TLP))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRchr ~ TLP, data=alldat[-44,])) #*
ggplot(alldat[-53,], aes(y=invasionLDGRchr, x=TLP))+
  geom_point()+
  geom_smooth(method="lm")

## option 2
## correlation between growth rate in under drought versus w/ neighbors
#related effects
summary(lm(invasionLDGRcon~intrinsicLDGRchr, NEWallsite)) #*
summary(lm(invasionLDGRcon~intrinsicLDGRchr, weights = weight2, NEWallsite[-106,])) #*
sqrt(0.3366)
cor.test(NEWallsite$invasionLDGRcon,NEWallsite$intrinsicLDGRchr)
anova(lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type, NEWallsite)) #*
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()
summary(lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type, weights = weight2, NEWallsite[-106,])) #*
anova(lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type, weights = weight2, NEWallsite[-106,])) #*
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=grass.forb))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  geom_abline()
#LDMC (ideal conditions describes better???)
summary(lm(invasionLDGRcon ~ LDMC, data=alldat)) #**
LDMCinvcon <- ggplot(alldat, aes(y=invasionLDGRcon, x=LDMC))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ LDMC, data=alldat)) #**
LDMCintchr <- ggplot(alldat, aes(y=intrinsicLDGRchr, x=LDMC))+
  geom_point()+
  geom_smooth(method="lm")
#root N
summary(lm(invasionLDGRcon ~ rootN, data=alldat)) #*
RNinvcon <- ggplot(alldat, aes(y=invasionLDGRcon, x=rootN))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ rootN, data=alldat)) #*
RNintchr <- ggplot(alldat, aes(y=intrinsicLDGRchr, x=rootN))+
  geom_point()+
  geom_smooth(method="lm")
#TLP
summary(lm(invasionLDGRcon ~ TLP, data=alldat[-44,])) #*
TLPinvcon <- ggplot(alldat[-44,], aes(y=invasionLDGRcon, x=TLP))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ TLP, data=alldat[-44,])) #*
TLPintchr <- ggplot(alldat[-44,], aes(y=intrinsicLDGRchr, x=TLP))+
  geom_point()+
  geom_smooth(method="lm")

library(patchwork)
(LDMCinvcon + LDMCintchr)/(RNinvcon + RNintchr)/(TLPinvcon + TLPintchr)
(LDMCinvcon/ LDMCintchr)|(RNinvcon/ RNintchr)|(TLPinvcon / TLPintchr)


#with CWM [TALK TO DANIEL ABOUT THESE MODELS!]
#LDMC
summary(lm(invasionLDGRcon ~ LDMC*grassland_type+SLA.x+TLP.x, data=alldat)) #** 35%
anova(lm(invasionLDGRcon ~ LDMC*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, interaction
ggplot(alldat, aes(y=invasionLDGRcon, x=LDMC, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ LDMC*grassland_type+SLA.x+TLP.x, data=alldat)) #* 25%
anova(lm(intrinsicLDGRchr ~ LDMC*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, interaction
ggplot(alldat, aes(y=intrinsicLDGRchr, x=LDMC, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
#root N
summary(lm(invasionLDGRcon ~ rootN*grassland_type+SLA.x+TLP.x, data=alldat)) # was sig. before other vars
ggplot(alldat, aes(y=invasionLDGRcon, x=rootN, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ rootN*grassland_type+SLA.x+TLP.x, data=alldat)) #* 27%
anova(lm(intrinsicLDGRchr ~ rootN*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, TLP
ggplot(alldat, aes(y=intrinsicLDGRchr, x=rootN, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
#TLP
summary(lm(invasionLDGRcon ~ TLP*grassland_type+SLA.x+TLP.x, data=alldat[-44,])) #* 18% (not with CWM's)
anova(lm(invasionLDGRcon ~ TLP*grassland_type+SLA.x+TLP.x, data=alldat[-44,])) #focal, grassland
ggplot(alldat[-44,], aes(y=invasionLDGRcon, x=TLP, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ TLP*grassland_type+SLA.x+TLP.x, data=alldat[-44,])) #* 23% (* 23%)
anova(lm(intrinsicLDGRchr ~ TLP*grassland_type+SLA.x+TLP.x, data=alldat[-44,])) #focal, interaction (interaction, TLP)
ggplot(alldat[-44,], aes(y=intrinsicLDGRchr, x=TLP, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

#test
summary(lm(invasionLDGRcon ~ leafN*grassland_type+SLA.x+TLP.x, data=alldat)) #ehh
ggplot(alldat, aes(y=invasionLDGRcon, x=leafN, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ leafN*grassland_type+SLA.x+TLP.x, data=alldat)) #*** 41%
anova(lm(intrinsicLDGRchr ~ leafN*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, TLP
ggplot(alldat, aes(y=intrinsicLDGRchr, x=leafN, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRcon ~ leafarea*grassland_type+SLA.x+TLP.x, data=alldat)) #* 31%
anova(lm(invasionLDGRcon ~ leafarea*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, sla, tlp
ggplot(alldat, aes(y=invasionLDGRcon, x=leafarea, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ leafarea*grassland_type+SLA.x+TLP.x, data=alldat)) #. 21%
ggplot(alldat, aes(y=intrinsicLDGRchr, x=leafarea, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

summary(con_anova <-aov(invasionLDGRcon ~ grassland_type, NEWallsite))
con_tuk <- TukeyHSD(con_anova) 
multcompView::multcompLetters4(con_anova, con_tuk) 
#Northern mixed and driest sites differ in growth rate with neighbors in grasslands
summary(con_anova <-aov(intrinsicLDGRchr ~ grassland_type, NEWallsite))
con_tuk <- TukeyHSD(con_anova) 
multcompView::multcompLetters4(con_anova, con_tuk) 
#no differences in grwoth rates in drought in grasslands??

### neighbors response in drought conditions: 
mod1 <- lm(invasionLDGRcon~intrinsicLDGRchr, weights = weight2, NEWallsite[-106,]) #*
summary(mod1) #trade-off
sqrt(summary(mod1)$adj.r.squared)#calculate r
#run second model w/ grassland
mod2 <- lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type, weights = weight2, NEWallsite[-106,]) #*
anova(mod2) 
#compare w/ anova
anova(mod1,mod2)


### OOPS with other dataframe
#with CWM [TALK TO DANIEL ABOUT THESE MODELS!]
traits <- NEWallsite
traits[,c(6:14,16)] <- log(traits[,c(6:14,16)])
traits[,15] <- log(abs(traits[,15]))
#LDMC
summary(lm(invasionLDGRcon ~ LDMC*grassland_type+SLA.x+TLP.x, data=alldat)) #** 18%
anova(lm(invasionLDGRcon ~ LDMC*grassland_type+SLA.x+TLP.x, data=alldat)) #focal
p1 <- ggplot(alldat, aes(y=invasionLDGRcon, x=LDMC, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRcon ~ LDMC*grassland_type, data=alldat)) #** 18%

summary(mod1<-lm(intrinsicLDGRchr ~ LDMC*grassland_type, data=alldat)) #** 20%
anova(lm(intrinsicLDGRchr ~ LDMC*grassland_type, data=traits)) #focal, interaction
p2 <- ggplot(alldat, aes(y=intrinsicLDGRchr, x=LDMC, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
#root N
summary(lm(invasionLDGRcon ~ rootN, data=alldat)) # was sig. before other vars
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=rootN, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
anova(testmod1,testmod2)
summary(lm(intrinsicLDGRchr ~ SRL*grassland_type, data=traits)) #
anova(lm(intrinsicLDGRchr ~ rootN*grassland_type+SLA.x+TLP.x, data=alldat)) #focal, TLP
ggplot(alldat, aes(y=intrinsicLDGRchr, x=SRL, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
#TLP
summary(lm(invasionLDGRcon ~ TLP*grassland_type, data=alldat)) #. 14% (not with CWM's)
anova(lm(invasionLDGRcon ~ TLP*grassland_type, data=traits)) #focal, grassland
ggplot(traits, aes(y=invasionLDGRcon, x=TLP, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ TLP*grassland_type, data=alldat)) #. 16% (* 23%)
anova(lm(intrinsicLDGRchr ~ TLP*grassland_type+SLA.x+TLP.x, data=alldat[-44,])) #focal, interaction (interaction, TLP)
ggplot(traits, aes(y=intrinsicLDGRchr, x=TLP, color=grassland_type))+
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




summary(lm(invasionLDGRcon ~ LTD*grassland_type, data=alldat)) #** 18%
p3 <- ggplot(alldat, aes(y=invasionLDGRcon, x=LTD, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

summary(mod1<-lm(intrinsicLDGRchr ~ LDMC*grassland_type, data=alldat)) #** 20%
anova(lm(intrinsicLDGRchr ~ LDMC*grassland_type, data=traits)) #focal, interaction
p4 <- ggplot(alldat, aes(y=intrinsicLDGRchr, x=LDMC, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

library(patchwork)
(ggplot(alldat, aes(y=invasionLDGRcon, x=LTD, color=grassland_type))+
    geom_point()+
    geom_smooth(method="lm") + 
  ggplot(alldat, aes(y=invasionLDGRcon, x=LTD, color=grassland_type))+
    geom_point()+
    geom_smooth(method="lm"))

/
  (p3 + p4)/
  (TLPinvcon + TLPintchr)

#### onld plot, but mat still use a####
#using ggpredict
library(ggeffects)
library(sjmisc)
LDMCmodA <- lm(invasionLDGRcon~LDMC*grassland_type, data=alldat)
preddata <- ggpredict(LDMCmodA, terms = c("LDMC", "grassland_type"))
mycols <- list("red", "rosybrown3", "skyblue2", "steelblue", "dark blue")
LDMCmodA_fig <- plot(preddata, add.data=T, colors = mycols, 
                     dot.alpha = .65, alpha = .5, limit.range = T,
                     show.legend = F, show.title = F)+
  facet_wrap(~group, nrow=1, labeller = label_wrap_gen(width = 5))+
  labs(x=" ", y= expression(italic(r)[rinvA]))+
  scale_x_continuous(n.breaks = 3)+
  theme_classic()+
  theme(strip.text = element_text(size=7))


LDMCmodD <- lm(intrinsicLDGRchr~LDMC*grassland_type, data=alldat)
preddata2 <- ggpredict(LDMCmodD, terms = c("LDMC", "grassland_type"))
LDMCmodD_fig <- plot(preddata2, add.data=T, colors = mycols, 
                     dot.alpha = .65, alpha = .5, limit.range = T,
                     show.legend = F, show.title = F)+
  facet_wrap(~group, nrow=1, labeller = label_wrap_gen(width = 5))+
  labs(x=" ", y= expression(italic(r)[intD]))+
  scale_x_continuous(n.breaks = 3)+
  theme_classic()+
  theme(strip.text = element_text(size=7))


#combine LDMC panel
LDMCpanel <- ggarrange(LDMCmodD_fig,LDMCmodA_fig, nrow=2, labels = c("a","b"))
LDMCpanel <- annotate_figure(LDMCpanel,bottom = "leaf dry matter content") #add axis label
LDMCpanel

#TLP
TLPmodA <- lm(invasionLDGRcon~TLP*grassland_type, data=alldat)
preddata3 <- ggpredict(TLPmodA, terms = c("TLP", "grassland_type"))
TLPmodA_fig <- plot(preddata3, add.data=T, colors = mycols, 
                    dot.alpha = .65, alpha = .5, limit.range = T,
                    show.legend = F, show.title = F)+
  facet_wrap(~group, nrow=1, labeller = label_wrap_gen(width = 5))+
  labs(x=" ", y= " ")+
  theme_classic()+
  theme(strip.text = element_text(size=7))


TLPmodD <- lm(intrinsicLDGRchr~TLP*grassland_type, data=alldat)
preddata4 <- ggpredict(TLPmodD, terms = c("TLP", "grassland_type"))
TLPmodD_fig <- plot(preddata4, add.data=T, colors = mycols, 
                    dot.alpha = .65, alpha = .5, limit.range = T,
                    show.legend = F, show.title = F)+
  facet_wrap(~group, nrow=1, labeller = label_wrap_gen(width = 5))+
  labs(x=" ", y= " ")+
  scale_x_continuous(n.breaks = 4)+
  theme_classic()+
  theme(strip.text = element_text(size=7))

#combine TLP panel
TLPpanel <- ggarrange(TLPmodD_fig,TLPmodA_fig, nrow=2,labels = c("c","d"))
TLPpanel <- annotate_figure(TLPpanel,bottom = "leaf turgor loss point") #add axis label
TLPpanel

#combine both trait panels
#trtfig <- ggarrange(LDMCpanel,TLPpanel, ncol=2)
trtfig <- ggarrange(LDMCpanel,TLPpanel, ncol=1)
trtfig

#export
#ggsave(trtfig, filename = "figures/trtfig.png", dpi=300, height = 4,width =7)
ggsave(trtfig, filename = "figures/trtfig.png", dpi=300, height = 8,width =6)





####
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
# LDMC not logged
summary(lm(intrinsicLDGRchr ~ LDMC*grassland_type, data=alldat)) #*** 20%
summary(lm(invasionLDGRcon ~ LDMC*grassland_type, data=alldat)) #*** 25%
# TLP
summary(lm(intrinsicLDGRchr ~ TLP*grassland_type, data=alldat)) #** 21%
summary(lm(invasionLDGRcon ~ TLP*grassland_type, data=alldat)) #* 18%


# SLA (messy, weak relationship)
summary(lm(intrinsicLDGRchr ~ SLA*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ SLA*grassland_type, data=alldat)) #* 11%
# LTD (little to no representation of driest sites)
summary(lm(intrinsicLDGRchr ~ LTD*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ LTD*grassland_type, data=alldat)) #* 16%
# leafarea
summary(lm(intrinsicLDGRchr ~ leafarea*grassland_type, data=alldat)) # not sig
summary(lm(invasionLDGRcon ~ leafarea*grassland_type, data=alldat)) # not sig
# leafN
summary(lm(intrinsicLDGRchr ~ leafN*grassland_type, data=alldat)) # nah
summary(lm(invasionLDGRcon ~ leafN*grassland_type, data=alldat)) # * 25

#height (more competitive to be tall in tallgrass and short in shortgrass)
summary(lm(intrinsicLDGRchr ~ height*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ height*grassland_type, data=alldat)) #* 14% 

# SRL
summary(lm(intrinsicLDGRchr ~ SRL*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ SRL*grassland_type, data=alldat)) #nah
# RTD
summary(lm(intrinsicLDGRchr ~ RTD*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ RTD*grassland_type, data=alldat)) #nah
# rootN
summary(lm(intrinsicLDGRchr ~ rootN*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ rootN*grassland_type, data=alldat)) #nah
# rootdiam
summary(lm(intrinsicLDGRchr ~ rootdiam*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ rootdiam*grassland_type, data=alldat)) #nah




##supp
##
#traits by site
trtsite <- read.csv("trtsite.csv") #data

## CHY
chytrts <- trtsite %>% filter(EDGE_site=="CHY") #26
#for site CHY how many populations have onsite (DB_HPG)
trtsite %>% filter(EDGE_site=="CHY") %>% 
  summarise(fromsite = (sum(!is.na(DB_WY))/26)*100) #80.8
#AS_MO and DB_CPER regional
trtsite %>% filter(EDGE_site=="CHY") %>% 
  summarise(fromsite = (sum(!is.na(AS_MO))/26)*100) #3.8
trtsite %>% filter(EDGE_site=="CHY") %>% 
  summarise(fromsite = (sum(!is.na(DB_CO))/26)*100) #7.7
#wider
trtsite %>% filter(EDGE_site=="CHY") %>% 
  summarise(fromsite = (sum(!is.na(TRY_AZ))/26)*100) #3.8

## SGS
sgstrts <- trtsite %>% filter(EDGE_site=="SGS") #17
#for site SGS how many populations have DB/CPER
trtsite %>% filter(EDGE_site=="SGS") %>% 
  summarise(fromsite = (sum(!is.na(DB_CO))/17)*100) #52.9
#wider
trtsite %>% filter(EDGE_site=="SGS") %>% 
  summarise(fromsite = (sum(!is.na(TRY_multiple))/17)*100) #5.9

## HYS
hystrts <- trtsite %>% filter(EDGE_site=="HYS") #29
#for site HYS how many populations have AS_HAYS
trtsite %>% filter(EDGE_site=="HYS") %>% 
  summarise(fromsite = (sum(!is.na(AS_KS))/29)*100) #41.4
#ST_KUT and TRY_KZ regional/ averages
trtsite %>% filter(EDGE_site=="HYS") %>% 
  summarise(fromsite = (sum(!is.na(ST_KS))/29)*100) #10.3
trtsite %>% filter(EDGE_site=="HYS") %>% 
  summarise(fromsite = (sum(!is.na(TRY_KS))/29)*100) #34.5

## KNZ
knztrts <- trtsite %>% filter(EDGE_site=="KNZ") #17
#for site KNZ how many populations have ST_KUT
trtsite %>% filter(EDGE_site=="KNZ") %>% 
  summarise(fromsite = (sum(!is.na(ST_KS))/17)*100) #35.3
#AS_HAYS and TRY_KZ and AF_iowa regional/ averages #60
trtsite %>% filter(EDGE_site=="KNZ") %>% 
  summarise(fromsite = (sum(!is.na(AS_KS))/17)*100) #17.6
trtsite %>% filter(EDGE_site=="KNZ") %>% 
  summarise(fromsite = (sum(!is.na(TRY_KS))/17)*100) #64.7
trtsite %>% filter(EDGE_site=="KNZ") %>% 
  summarise(fromsite = (sum(!is.na(AF_IA))/17)*100) #23.5
#TRY_CAN global 
(2/17)*100 #11.8

## SBL
SBLtrts <- trtsite %>% filter(EDGE_site=="SBU") #11
#for site SBL how many populations have sevLTER/blue
# RG/sev also good
trtsite %>% filter(EDGE_site=="SBU") %>% 
  summarise(fromsite = (sum(!is.na(SEV_NM))/11)*100) #63.63
trtsite %>% filter(EDGE_site=="SBU") %>% 
  summarise(fromsite = (sum(!is.na(RG_NM))/11)*100) #90.9
#regional sevLTER/black
trtsite %>% filter(EDGE_site=="SBU") %>% 
  summarise(fromsite = (sum(!is.na(SEV_NM))/11)*100) #30

## SBK
SBKtrts <- trtsite %>% filter(EDGE_site=="SBK") #13
#for site SBK how many populations have sevLTER/black
trtsite %>% filter(EDGE_site=="SBK") %>% 
  summarise(fromsite = (sum(!is.na(SEV_NM))) #18.2
            #regional sevLTER/blue and RG/sev and AS_NM and TRY_AZ
            trtsite %>% filter(EDGE_site=="SBK") %>% 
              summarise(fromsite = (sum(!is.na(RG_sev))/11)*100) #63.6
            trtsite %>% filter(EDGE_site=="SBK") %>% 
              summarise(fromsite = (sum(!is.na(sevLTER_blue))/11)*100) #36.4
            trtsite %>% filter(EDGE_site=="SBK") %>% 
              summarise(fromsite = (sum(!is.na(AS_NM))/11)*100) #18.18
            trtsite %>% filter(EDGE_site=="SBK") %>% 
              summarise(fromsite = (sum(!is.na(TRY_AZ))/11)*100) #27.3
            # global
            trtsite %>% filter(EDGE_site=="SBK") %>% 
              summarise(fromsite = (sum(!is.na(TRY_global))/11)*100) #9.1