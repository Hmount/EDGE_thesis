library(tidyverse)
library(ggpubr)

NEWallsite <- read.csv("data/allsite_new.csv") #data
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



##
#traits by site
trtsite <- read.csv("trtsite.csv") #data

## CHY
chytrts <- trtsite %>% filter(EDGE_site=="CHY") #26, but 1 spp has NA so 25
#for site CHY how many populations have onsite (DB_HPG)
trtsite %>% filter(EDGE_site=="CHY") %>% 
  summarise(fromsite = (sum(!is.na(DB_HPG))/25)*100) #84%
#AS_MO and DB_CPER regional
trtsite %>% filter(EDGE_site=="CHY") %>% 
  summarise(fromsite = (sum(!is.na(AS_MO))/25)*100) #4
trtsite %>% filter(EDGE_site=="CHY") %>% 
  summarise(fromsite = (sum(!is.na(DB_CPER))/25)*100) #8
#wider
trtsite %>% filter(EDGE_site=="CHY") %>% 
  summarise(fromsite = (sum(!is.na(TRY_AZ))/25)*100) #4

## SGS
sgstrts <- trtsite %>% filter(EDGE_site=="SGS") #17, but 7 spp has NA so 10
#for site SGS how many populations have DB/CPER
trtsite %>% filter(EDGE_site=="SGS") %>% 
  summarise(fromsite = (sum(!is.na(DB_CPER))/10)*100) #90
#wider
trtsite %>% filter(EDGE_site=="SGS") %>% 
  summarise(fromsite = (sum(!is.na(TRY_.all))/10)*100) #10

## HYS
hystrts <- trtsite %>% filter(EDGE_site=="HYS") #29, but 4 spp has NA so 25
#for site HYS how many populations have AS_HAYS
trtsite %>% filter(EDGE_site=="HYS") %>% 
  summarise(fromsite = (sum(!is.na(AS_HAYS))/25)*100) #48
#ST_KUT and TRY_KZ regional/ averages
trtsite %>% filter(EDGE_site=="HYS") %>% 
  summarise(fromsite = (sum(!is.na(ST_KUT))/25)*100) #12
trtsite %>% filter(EDGE_site=="HYS") %>% 
  summarise(fromsite = (sum(!is.na(TRY_KZ))/25)*100) #40

## KNZ
knztrts <- trtsite %>% filter(EDGE_site=="KNZ") #17, but 2 spp has NA so 15
#for site KNZ how many populations have ST_KUT
trtsite %>% filter(EDGE_site=="KNZ") %>% 
  summarise(fromsite = (sum(!is.na(ST_KUT))/15)*100) #40
#AS_HAYS and TRY_KZ and AF_iowa regional/ averages #60
trtsite %>% filter(EDGE_site=="KNZ") %>% 
  summarise(fromsite = (sum(!is.na(AS_HAYS))/15)*100) #20
trtsite %>% filter(EDGE_site=="KNZ") %>% 
  summarise(fromsite = (sum(!is.na(TRY_KZ))/15)*100) #73.3
trtsite %>% filter(EDGE_site=="KNZ") %>% 
  summarise(fromsite = (sum(!is.na(AF_Iowa))/15)*100) #26.7

## SBL
SBLtrts <- trtsite %>% filter(EDGE_site=="SBU") #11, but 1 spp has NA so 10
#for site SBL how many populations have sevLTER/blue
  # RG/sev also good
trtsite %>% filter(EDGE_site=="SBU") %>% 
  summarise(fromsite = (sum(!is.na(sevLTER_blue))/10)*100) #60
trtsite %>% filter(EDGE_site=="SBU") %>% 
  summarise(fromsite = (sum(!is.na(RG_sev))/10)*100) #100
#regional sevLTER/black
trtsite %>% filter(EDGE_site=="SBU") %>% 
  summarise(fromsite = (sum(!is.na(sevLTER_black))/10)*100) #30

## SBK
SBKtrts <- trtsite %>% filter(EDGE_site=="SBK") #13, but 2 spp has NA so 11
#for site SBK how many populations have sevLTER/black
trtsite %>% filter(EDGE_site=="SBK") %>% 
  summarise(fromsite = (sum(!is.na(sevLTER_black))/11)*100) #18.2
#regional sevLTER/blue and RG/sev and AS_NM and TRY_AZ
trtsite %>% filter(EDGE_site=="SBK") %>% 
  summarise(fromsite = (sum(!is.na(RG_sev))/11)*100) #63.6
trtsite %>% filter(EDGE_site=="SBK") %>% 
  summarise(fromsite = (sum(!is.na(sevLTER_blue))/11)*100) #36.4
trtsite %>% filter(EDGE_site=="SBK") %>% 
  summarise(fromsite = (sum(!is.na(AS_NM))/11)*100) #18.18
trtsite %>% filter(EDGE_site=="SBK") %>% 
  summarise(fromsite = (sum(!is.na(TRY_AZ))/11)*100) #27.3
# global
trtsite %>% filter(EDGE_site=="SBK") %>% 
  summarise(fromsite = (sum(!is.na(TRY_global))/11)*100) #9.1
