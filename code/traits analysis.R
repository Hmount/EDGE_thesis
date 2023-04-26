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
sum(complete.cases(NEWallsite[,c(6:16)])) #21 species have all rows
trtcnt <- rowSums(!is.na(NEWallsite[,c(6:16)])) #count for each row w/ traits filled in
sum(trtcnt == 0) #11 have no trait data 
sum(trtcnt > 5) #59 have more than 5 traits
(sum(trtcnt > 5)/sum(trtcnt >= 0))*100 #52% of data has more than 5 traits


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
summary(lm(invasionLDGRcon ~ LDMC*grassland_type, data=alldat)) #*** 25%
summary(lm(intrinsicLDGRchr ~ LDMC*grassland_type, data=alldat)) #*** 19%
# SLA (messy, weak relationship)
summary(lm(invasionLDGRcon ~ SLA*grassland_type, data=alldat)) #* 11%
summary(lm(intrinsicLDGRchr ~ SLA*grassland_type, data=alldat)) #nah
# LTD (little to no representation of driest sites)
summary(lm(invasionLDGRcon ~ LTD*grassland_type, data=alldat)) #* 16%
summary(lm(intrinsicLDGRchr ~ LTD*grassland_type, data=alldat)) #nah
# leafarea
summary(lm(invasionLDGRcon ~ leafarea*grassland_type, data=alldat)) # not sig
summary(lm(intrinsicLDGRchr ~ leafarea*grassland_type, data=alldat)) # not sig
# leafN
summary(lm(invasionLDGRcon ~ leafN*grassland_type, data=alldat)) # * 25
summary(lm(intrinsicLDGRchr ~ leafN*grassland_type, data=alldat)) # nah
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


