#### Supporting information ####

library(tidyverse)
library(ggpubr)

NEWallsite <- read.csv("data/all_pop_data.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))


#### do pop.responses differ by basic life history traits? ####
# neither fitness under drought nor with neighbors differs by between life history.
# response to nieghbors in ambient does differ on the grasslands by lifespan, life form, and photo-type 

#response to drought
NEWallsite <- NEWallsite %>% mutate(lifespan.binary = ifelse(lifespan %in% "Short-lived","0", "1")) #make binary
NEWallsite$lifespan.binary <- as.factor(NEWallsite$lifespan.binary)

var.test(NEWallsite$intrinsicLDGRchr~NEWallsite$lifespan)
t.test(NEWallsite$intrinsicLDGRchr~NEWallsite$lifespan, var.equal = T) #no diff
lifeplot1 <- ggplot(NEWallsite, aes(fill=lifespan, x=intrinsicLDGRchr, y=lifespan))+
  geom_boxplot() +
  labs(y=" ", x=expression(italic(r)[intD]))+
  theme_minimal()+
  theme(legend.position = "none")

var.test(NEWallsite$invasionLDGRcon~NEWallsite$lifespan)
t.test(NEWallsite$invasionLDGRcon~NEWallsite$lifespan, var.equal = T) #no diff
lifeplot2 <- ggplot(NEWallsite, aes(fill=lifespan, x=invasionLDGRcon, y=lifespan))+
  geom_boxplot() +
  labs(y=" ", x=expression(italic(r)[rinvA]))+
  theme_minimal()+
  theme(legend.position = "none")

responsep1 <- ggarrange(lifeplot1, lifeplot2, ncol=2, labels = c("a","b"))
responsep1 <- annotate_figure(responsep1, left = "Lifespan")
responsep1


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




#### additional ####

#compare response models
main <- lm(invasionLDGRcon~intrinsicLDGRchr, weights=weight2, NEWallsite[-110,]) #new main finding
anova(main) #trade-off
sqrt(summary(main)$adj.r.squared)#calculate r
#run second model w/ grassland
main.lifespan <- lm(invasionLDGRcon~intrinsicLDGRchr*lifespan.binary, weights=weight2, NEWallsite[-110,]) #old finding 
anova(main.lifespan) 
sqrt(summary(main.lifespan)$adj.r.squared)#calculate r
#compare w/ anova
anova(main,main.lifespan)
#plot
corrlifespan <- ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=lifespan))+
  geom_point(alpha=.5, shape=16)+
  geom_abline()+
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  labs(y=expression(italic(r)[rinvA]), x = expression(italic(r)[intD]), color="Lifespan")+
  theme_classic()

responsep2 <- ggarrange(responsep1, corrlifespan, nrow=2, heights = c(.5,1.5), labels = c("","c"))
responsep2


