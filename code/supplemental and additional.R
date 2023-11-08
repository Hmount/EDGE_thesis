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





