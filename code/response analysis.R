#### Analysis of population growth in different conditions ####
# testing for a relationship between popualtion growth with minumum neigbors in drought
# and and popualtion growth with higher neghbor cover in ambient conditions. Also exploring 
# how the relationship differs across a percipitation gradient of communities.
# Relationship in OLS suggests drought tolerecnce effects competition resistemce, but also
# check results in Major axis (MA) which does not assume a causul relationship. The results 
# are the same?
# r as the stat I report for the correlation, 
# LRT and ANOVA's used to show small differences in grasslands.

#### data and packages ####
library(tidyverse)
library(ggpubr)
library(emmeans)

NEWallsite <- read.csv("data/all_pop_data.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))


#### How are drought tolerence and competetive resistence related? ####
# using lm function to incorporate weights and compare model w/ and w/out grassland,
# but calculating r to report in paper (same as cor)
# (removing row 110 for -inf)

## Model in words: this model shows the relationship between LDGR with low
# heterospecific density in drought (proxy for drought tolerence) and the 
# relationship between LDGR with average heterospecific density in ambient 
# (proxy for competetion tolerence)
summary(m1 <- lm(invasionLDGRcon~intrinsicLDGRchr, weights = weight2, NEWallsite[-110,])) #*** 34.5%
sqrt(summary(m1)$adj.r.squared) #calculate r
# positive relationship suggests being better at living in drought is correlated with
# that species doing better at living with more neighbors in drought
# below 1:1 line growth rate is higher without neighbors
# above 1:1 line growth rate is higher with neighbors

## including grassland?
summary(m2 <-lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type, weights = weight2, NEWallsite[-110,])) #*** 43% 
sqrt(summary(m2)$adj.r.squared) #calculate r
anova(m2) #grassland interaction is very sig., but 
library(emmeans)
emmeans(m2, specs = pairwise~grassland_type) #contrasts are weak, but driest differ from
# northern shortgrass (+ N mixed)
#(potentially dry grasslands do not share the trend as much?)

## compare models
anova(m1,m2) # model with grassland is better



#### Supporting information ####
#### re-do analysis with Standard Major Axis regression ####
# RJG suggested to use SMA regression instead of OLS to avoid causal relationships. While SMA accounts
# for uncertainty in our measurements of both variables, I was able to give specific measurements of 
# uncertainty using OLS and the SE of previous models. For this benefit, and because the final results
# are qualitatively similar, we report with OLS.
library(smatr) #for major axis regression
# re-run model of response to neighbors and response to drought as major axis regression
# uncertainty is incorporated. cannot use weights because uncertainty is needed for both sides.
test <- sma(invasionLDGRcon~intrinsicLDGRchr,NEWallsite[-110,]) # weights=weight2, ) 
summary(test)
sqrt(test$r2[[1]]) # .55 correlation
plot(test)
#other model is slightly better

test1 <- sma(invasionLDGRcon~intrinsicLDGRchr+grassland_type,NEWallsite[-110,])
summary(test1)
sqrt(test1$r2[[1]]) # .05 correlation
plot(test1)
#grassland does not improve?


#### Do grasslands generally differ significantly in population growth? ####
# Does population growth with neighbor cover in ambient differ between grasslands?
summary(aov(invasionLDGRcon ~ grassland_type, NEWallsite)) # No

# Does population growth with low neighbor cover in drought differ between grasslands? 
summary(aov(intrinsicLDGRchr ~ grassland_type, NEWallsite)) # No


## including grassland?
summary(m3 <-lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type*lifespan, weights = weight2, NEWallsite[-110,])) #*** 43% 
sqrt(summary(m3)$adj.r.squared) #calculate r
anova(m3) #grassland interaction is very sig., but 
library(emmeans)
emmeans(m2, specs = pairwise~grassland_type) #contrasts are weak, but driest differ from
# northern shortgrass (+ N mixed)
#(potentially dry grasslands do not share the trend as much?)

## compare models
anova(m2,m3) # model with grassland is better
