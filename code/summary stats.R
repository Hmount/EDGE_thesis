#### Summary stats ####
# examine how population growth rates respond to drought conditions and neighbor abundance in 
# in different grasslands and see how these responses relate to one another across a 
# precipitation gradient.


#### load in data and packages ####
library(tidyverse)
library(ggpubr)
NEWallsite <- read.csv("data/all_pop_data.csv")
#ensure this data is re-leveled with grassland_type facets along precipitation gradient 
NEWallsite <- NEWallsite %>%
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
all <- read.csv("data/EDGE_covers.csv")
all <- all %>% mutate(grassland_type = ifelse(site %in% "CHY","Northern Mixed",
                                              ifelse(site %in% "HYS","Southern Mixed",
                                                     ifelse(site %in% "KNZ","Tallgrass",
                                                            ifelse(site %in% "SGS","Northern Shortgrass",
                                                                   ifelse(site %in% "sev.blue","Southern Shortgrass","Desert")))))) 
all <- all %>%
  mutate(grassland_type = fct_relevel(grassland_type, 
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))

#### how do growth rates vary by condition (histograms) and across grasslands? (anova/boxplots) ####
# histograms of all calculated growth rate measures show non-normal distribution and have a 
# right/positive skew (see 'code/figures.R' file for histograms). This indicates most species are 
# maintaining or growing in pop. size which is consistent with expectations. 

#ANOVA of calculated growth rate measures: no differences in growth rates by
# grassland type. (corresponding boxplots in 'code/figures.R')
summary(aov(intrinsicLDGRchr ~ grassland_type, data = NEWallsite)) #no difference
summary(aov(invasionLDGRcon ~ grassland_type, data = NEWallsite)) #no difference
## grasslands don't differ in fitness to eother dirver


#### How does precipitation effect cover? ####
# how does cover vary across samples (year, trt, plot , etc.)? Quite a bit:
# corresponding figures in 'code/figures.R', Figure 3
#summarizing quadrat-level cover
allsum <- all %>% group_by(grassland_type, year, trt, plot, subplot) %>% summarize(totalcov = sum(cover))
#relevel new df
allsum <- allsum %>%
  mutate(grassland_type = fct_relevel(grassland_type, 
                                      "Desert", "Southern Shortgrass","Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
write.csv(allsum, "data/allsum_quadrat.csv", row.names = F) #save as csv if needed

#is cover significantly different between grasslands? (yes, each is uniquely different)
#corresponds to Figure 3 in 'code/figures.R'
summary(anova_cov <- aov(totalcov ~ grassland_type, data = allsum)) #anova sig. 
tuk_cov <- TukeyHSD(anova_cov) #each site is significantly different
multcompView::multcompLetters4(anova_cov, tuk_cov) #letters for each site

# does the precipitation gradient coincide with gradient of competition for light? (yes)
#use linear regression to assess changes along increasing cover
#corresponds to Figure 3 in 'code/figures.R'
summary(lm_covsum <- lm(totalcov ~ grassland_type, data = allsum)) #sig. pos., 66% 


#### does average neighbor cover effect estimates of response to neighbors in either condition? ####
# overall, no we aren't not concerned that these differences effected estimates. 
# corresponding figures in 'code/figures.R', Figure 3

#sum all the cover for each plot/ year/ grassland/ species (bounding measures between 0 and 100/300)
#growth with neighbors
allothermeans <- all %>%
  filter(trt=="con") %>%
  group_by(grassland_type, species, plot, year, subplot) %>%
  summarize(meanother = mean(other)) %>% ungroup 
covergrasslandcon <- full_join(NEWallsite, allothermeans, by=c("grassland_type", "species"))#merge dataframes
#make a model
summary(m1<-lm(invasionLDGRcon~meanother, covergrasslandcon)) 
anova(m1)
#repeat for growth in drought
allothermeans <- all %>%
  filter(trt=="chr")%>%
  group_by(grassland_type, species, plot, year, subplot) %>%
  summarize(meanother = mean(other)) %>% ungroup 
covergrasslandchr <- full_join(NEWallsite, allothermeans, by=c("grassland_type", "species"))


# The mean density of cover is significantly negatively related to the focal species 
# growth rates in drought AND with neighbor cover, implying a size-density relationship. 
# However, because both models have the same relationship, these effects are unlkiley to 
# be greatly effecting our modelling results. 
# (increases in cover = decreases in GR responses drought and to neighbors)
