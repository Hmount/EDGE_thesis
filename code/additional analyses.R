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

#### ensure that ratio correlations are not mathematically inevitable
#make simulated data
rinvA <- runif(100, -1,4) #invasion GR in ambient
rinvD <- runif(100, -1,4) #invasion GR in drought
rintA <- runif(100, -1,4) #intrinsic GR in ambient
rintD <- runif(100, -1,4) #intrinsic GR in drought

#find differences
intdif <- rintD - rintA #response to drought without neighbors
invdif <- rinvD - rinvA #response to drought with neighbors
condif <- rinvA - rintA #response to neighbors in ambient
chrdif <- rinvD - rintD #response to neighbors in drought

#make into one table
simdat <- data.frame(intdif,invdif,condif,chrdif)

#compare correlations among random variables
cor(simdat)

#in a model
summary(lm(condif~intdif, simdat)) 
summary(lm(chrdif~intdif, simdat)) 

ggplot(simdat, aes(y=condif, x=intdif))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(simdat, aes(y=chrdif, x=intdif))+
  geom_point()+
  geom_smooth(method = "lm")

summary(lm(condif~chrdif, simdat)) 
summary(lm(invdif~intdif, simdat)) 



summary(lm(invasionLDGRcon~intrinsicLDGRchr, NEWallsite))
plot(invasionLDGRcon~intrinsicLDGRchr, NEWallsite)
anova(m1<-lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type, NEWallsite))
emmeans(m1, specs = pairwise~grassland_type)
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

summary(lm(invasionLDGRchr~intrinsicLDGRchr, NEWallsite))
plot(invasionLDGRchr~intrinsicLDGRchr, NEWallsite)

summary(lm(invasionLDGRcon~intrinsicLDGRcon, NEWallsite))
plot(invasionLDGRchr~intrinsicLDGRchr, NEWallsite)
anova(m2 <- lm(invasionLDGRcon~intrinsicLDGRcon*grassland_type, NEWallsite))
library(emmeans)
emmeans(m2, specs = pairwise~grassland_type)
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRcon, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

summary(lm(invasionLDGRchr~invasionLDGRcon, NEWallsite))
plot(invasionLDGRchr~intrinsicLDGRchr, NEWallsite)

summary(lm(intrinsicLDGRchr~intrinsicLDGRcon, NEWallsite))
plot(invasionLDGRchr~intrinsicLDGRchr, NEWallsite)

summary(lm(intrinsicLDGRcon~invasionLDGRchr, NEWallsite))
plot(intrinsicLDGRcon~invasionLDGRchr, NEWallsite)


##with daniel
summary(lm(intrinsicLDGRchr~intrinsicLDGRcon, NEWallsite))
plot(intrinsicLDGRchr~intrinsicLDGRcon, NEWallsite)
abline(0,1)
ggplot(NEWallsite, aes(y=intrinsicLDGRchr, x=intrinsicLDGRcon, color=lifespan))+
  geom_point()+
  geom_smooth(method="lm")


summary(lm(invasionLDGRcon~intrinsicLDGRchr, NEWallsite))
plot(invasionLDGRcon~intrinsicLDGRchr, NEWallsite)
abline(0,1)

anova(m1<-lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type, NEWallsite))
emmeans(m1, specs = pairwise~grassland_type)
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()


summary(lm(invasionLDGRchr~intrinsicLDGRchr, NEWallsite))
plot(invasionLDGRchr~intrinsicLDGRchr, NEWallsite)
abline(0,1)
ggplot(NEWallsite, aes(y=invasionLDGRchr, x=intrinsicLDGRchr, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_abline()
