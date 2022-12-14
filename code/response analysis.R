#### Analysis of population responses to different conditions ####
# testing for a trade-off in responses to drought and neighbors in different conditions
# checked OLS results against Major axis (MA) as the analysis with same results. 
# r as the stat I report for correlation, ANOVA's to show lack of difference between 
# grassland types.

#### data and packages ####
library(tidyverse)
library(ggpubr)

NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))


#### first, how correlated are measures of the same response in different conditions? ####
#compare response to drought when alone and with neighbors
cor.test(NEWallsite$intrinsicdiff, NEWallsite$invasiondiff) #* 53% pos. cor
#not direct 1:1, no relationships w/ grassland or life history traits
ggplot(NEWallsite, aes(x=intrinsicdiff, y=invasiondiff, color=grassland_type))+
  geom_point()
#compare response to neighbors when in ambient and in drought
cor.test(NEWallsite$condiff, NEWallsite$chrdiff) #* 40% pos. cor
#even less direct 1:1, no relationships w/ grassland or life history traits
ggplot(NEWallsite, aes(x=condiff, y=chrdiff, color=grassland_type))+
  geom_point()
# response to drought is slightly more concerted, but both vary more than expected
# in different conditions


#### How do responses to drought relate to responses to neighbors in diff. conditions? ####
# using lm function to incorporate weights and compare model w/ and w/out grassland,
# but calculating r to report in paper (same as cor):
# (removing row 106 for -inf)
### neighbors response in ambient conditions: 
intcon <- lm(condiff~intrinsicdiff, weights=weight2, NEWallsite[-106,]) #model with weights 
summary(intcon) #uncorrelated -> the pure response to neighbors and to drought are not related 
#(not a physiological trade-off, but an expressed trade-off?)
sqrt(summary(intcon)$adj.r.squared)#calculate r
#run second model w/ grassland
intcon2_fig <- lm(condiff~intrinsicdiff*grassland_type, weights=weight2, NEWallsite[-106,]) #old finding 
summary(intcon2_fig) 
#compare w/ anova
anova(intcon,intcon2_fig)

### neighbors response in drought conditions: 
intchr <- lm(chrdiff~intrinsicdiff, weights=weight2, NEWallsite[-106,]) #new main finding
anova(intchr) #trade-off
sqrt(summary(intchr)$adj.r.squared)#calculate r
#run second model w/ grassland
intchr2 <- lm(chrdiff~intrinsicdiff*grassland_type, weights=weight2, NEWallsite[-106,]) #old finding 
anova(intchr2) 
#compare w/ anova
anova(intchr,intchr2)

#### Supporting information ####
#### re-do analysis with Standard Major Axis regression ####
# RJG suggested to use SMA regression instead of OLS to avoid causal relationships. While SMA accounts
# for uncertainty in our measurements of both variables, I was able to give specific measuremnts of 
# uncertainty using OLS and the SE of previous models. For this benefit, and because the final results
# are qualitativley similar, we report with OLS.
library(tidyverse)
library(smatr) #for major axis regression

alldat <- read.csv("allsite_new.csv")

# re-run model of response to neighbors and response to drought as major axis regression
#uncertainty is incorporated. cannot use weights because uncertainty is needed for both sides.
test <- sma(chrdiff~intrinsicdiff,NEWallsite[-106,]) # weights=weight2, ) 
summary.sma(test)
sqrt(test$r2[[1]]) #correlation
plot(test)

test1 <- sma(chrdiff~intrinsicdiff+grassland_type,NEWallsite[-106,])
anova(test,test1)


#### ANOVA of each response ~ grassland ####
### population responses to drought in different grasslands:
summary(aov(intrinsicdiff ~ grassland_type, NEWallsite)) #no difference

### neighbors response in ambient conditions in different grasslands: 
summary(con_anova <-aov(condiff ~ grassland_type, NEWallsite)) #sig. difference
con_tuk <- TukeyHSD(con_anova) #southern-mixed vs. two driest sites
multcompView::multcompLetters4(con_anova, con_tuk) #letters for each site

### neighbors response in drought conditions in different grasslands: 
summary(aov(chrdiff ~ grassland_type, NEWallsite)) #no difference
