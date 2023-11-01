#### traits analysis ####

#### data and packages + cleaning ####
library(tidyverse)

NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
alldat <- NEWallsite

#### basic stats and relationships ####
# how many populations have data?
sum(complete.cases(NEWallsite[,c(5:15)])) #21 species have all rows
trtcnt <- rowSums(!is.na(NEWallsite[,c(5:16=5)])) #count for each row w/ traits filled in
sum(trtcnt == 0) #11 have no trait data 
sum(trtcnt > 5) #59 have more than 5 traits
(sum(trtcnt > 5)/sum(trtcnt >= 0))*100 #52% of data has more than 5 traits


#### modelling; traits as predictors of population growth ####
# These models do not include CWM but are better overall
# LDMC not logged
summary(lm(intrinsicLDGRchr ~ LDMC*grassland_type, data=alldat)) #*** 19%
summary(lm(invasionLDGRcon ~ LDMC*grassland_type, data=alldat)) #*** 25%
# TLP
summary(lm(intrinsicLDGRchr ~ TLP*grassland_type, data=alldat)) #** 21%
summary(lm(invasionLDGRcon ~ TLP*grassland_type, data=alldat)) #* 18%


# SLA (messy, weak relationship)
summary(lm(intrinsicLDGRchr ~ SLA*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ SLA*grassland_type, data=alldat)) #** 19%
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
summary(lm(invasionLDGRcon ~ SRL*grassland_type, data=alldat)) #* 19%
# RTD
summary(lm(intrinsicLDGRchr ~ RTD*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ RTD*grassland_type, data=alldat)) #nah
# rootdiam
summary(lm(intrinsicLDGRchr ~ rootdiam*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ rootdiam*grassland_type, data=alldat)) #nah
# rootN
summary(lm(intrinsicLDGRchr ~ rootN*grassland_type, data=alldat)) #nah
summary(lm(invasionLDGRcon ~ rootN*grassland_type, data=alldat)) #nah


#### anova output to report THIS IS IN THE PAPER
# LDMC not logged
anova(lm(intrinsicLDGRchr ~ LDMC*grassland_type, data=alldat))
anova(lm(invasionLDGRcon ~ LDMC*grassland_type, data=alldat)) 
# TLP
anova(lm(intrinsicLDGRchr ~ TLP*grassland_type, data=alldat))
anova(lm(invasionLDGRcon ~ TLP*grassland_type, data=alldat)) 


# SLA (messy, weak relationship)
anova(lm(intrinsicLDGRchr ~ SLA*grassland_type, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ SLA*grassland_type, data=alldat)) #* 11%
# LTD (little to no representation of driest sites)
anova(lm(intrinsicLDGRchr ~ LTD*grassland_type, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ LTD*grassland_type, data=alldat)) #* 16%
# leafarea
anova(lm(intrinsicLDGRchr ~ leafarea*grassland_type, data=alldat)) # not sig
anova(lm(invasionLDGRcon ~ leafarea*grassland_type, data=alldat)) # not sig
# leafN
anova(lm(intrinsicLDGRchr ~ leafN*grassland_type, data=alldat)) # nah
anova(lm(invasionLDGRcon ~ leafN*grassland_type, data=alldat)) # * 25

#height (more competitive to be tall in tallgrass and short in shortgrass)
anova(lm(intrinsicLDGRchr ~ height*grassland_type, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ height*grassland_type, data=alldat)) #* 14% 

# SRL
anova(lm(intrinsicLDGRchr ~ SRL*grassland_type, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ SRL*grassland_type, data=alldat)) #nah
# RTD
anova(lm(intrinsicLDGRchr ~ RTD*grassland_type, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ RTD*grassland_type, data=alldat)) #nah
# rootdiam
anova(lm(intrinsicLDGRchr ~ rootdiam*grassland_type, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ rootdiam*grassland_type, data=alldat)) #nah
# rootN
anova(lm(intrinsicLDGRchr ~ rootN*grassland_type, data=alldat)) #nah
anova(lm(invasionLDGRcon ~ rootN*grassland_type, data=alldat)) #nah


hist(alldat$TLP)
hist(alldat$LDMC)
