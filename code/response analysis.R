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
