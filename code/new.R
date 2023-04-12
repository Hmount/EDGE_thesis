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
