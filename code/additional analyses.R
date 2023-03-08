#### additional analyses ####
# These analyses were done, but did not make it into the final paper:
# do pop.responses differ by basic life history traits? (no)

library(tidyverse)
library(ggpubr)

NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))


#### do pop.responses differ by basic life history traits? ####
# response to drought may differ slightly by lifespan, but non-sig.
# response to neighbors in drought does not differ by any life history (interesting)
# response to nieghbors in ambient does differ on the grasslands by lifespan, life form, and photo-type 

#response to drought
NEWallsite <- NEWallsite %>% mutate(lifespan.binary = ifelse(lifespan %in% "Short-lived","0", "1")) #make binary
NEWallsite$lifespan.binary <- as.factor(NEWallsite$lifespan.binary)
var.test(NEWallsite$intrinsicdiff~NEWallsite$lifespan)
t.test(NEWallsite$intrinsicdiff~NEWallsite$lifespan, var.equal = T) #annuals have more positive responses to drought than perennials
ggplot(NEWallsite, aes(fill=lifespan, y=intrinsicdiff))+
  geom_boxplot() +
  theme_minimal()

var.test(NEWallsite$chrdiff~NEWallsite$lifespan)
t.test(NEWallsite$chrdiff~NEWallsite$lifespan, var.equal = T) #no diff

var.test(NEWallsite$condiff~NEWallsite$lifespan)
t.test(NEWallsite$condiff~NEWallsite$lifespan, var.equal = T) #no diff

#compare response models
main <- lm(chrdiff~intrinsicdiff, weights=weight2, NEWallsite[-106,]) #new main finding
anova(main) #trade-off
sqrt(summary(intchr)$adj.r.squared)#calculate r
#run second model w/ grassland
main.lifespan <- lm(chrdiff~intrinsicdiff*lifespan.binary, weights=weight2, NEWallsite[-106,]) #old finding 
anova(main.lifespan) 
#compare w/ anova
anova(main,main.lifespan)




#response to neighbors drought
anova(lm(chrdiff~lifespan*grassland_type,data=NEWallsite))#nah
anova(lm(chrdiff~grass.forb*grassland_type,data=NEWallsite))#nah
anova(lm(chrdiff~Photosynthesis*grassland_type,data=NEWallsite[-c(125,136),]))#nah

#response to neighbors ambient
anova(lm(condiff~lifespan*grassland_type,data=NEWallsite))#grassland
anova(lm(condiff~grass.forb*grassland_type,data=NEWallsite))#all!
summary(lm(condiff~grass.forb*grassland_type,data=NEWallsite))#all!
ggplot(NEWallsite, aes(y=condiff, x=Photosynthesis))+
  geom_boxplot() +
  facet_wrap(~grassland_type, scales = "free", nrow=1)+
  labs(y=" ", x="Photosynthetic pathway")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
anova(lm(condiff~Photosynthesis*grassland_type,data=NEWallsite[-c(125,136),]))#grassland



#### test if difference in traits (dissimilarity from CWM) improved fitness (it did not) ####
#find difference between CWM and focal traits
test <- trtdata %>% mutate(traitdiff = SLA.x-SLA.y)
#plot
ggplot(test, aes(y=chrdiff, x=traitdiff))+#, color=grassland_type))+ 
  geom_point()+
  geom_smooth(method="lm")
### pop responses as a function of trait difference
summary(lm(chrdiff~traitdiff, test))

#### ensure that ratio correlations are not mathematically inevitable)
#make simulated data
rinvA <- runif(100, -1,4)
rinvD <- runif(100, -1,4)
rintA <- runif(100, -1,4)
rintD <- runif(100, -1,4)

#find differences
intdif <- rinvA - 
invdif
condif
chrdif

#compare correlations amoung random variables
cor.test()

