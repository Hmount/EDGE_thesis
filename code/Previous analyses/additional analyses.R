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


#### from redo
#total cover at each grassland does not have a relationship with pop. responses to neighbors:
#for response to neighbors in ambient
concov <- ggplot(covergrasslandcon,aes(x=meanother, y=invasionLDGRcon, color=grassland_type))+
  #geom_point()+
  geom_smooth(method="lm")+
  geom_hline(aes(yintercept=0), linetype="dotted")+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(y=expression(italic(r)[rinvA]), x = "Interspecific cover (%)")+
  theme(legend.position="none")
#for response to neighbors in drought
chrcov <- ggplot(covergrasslandchr,aes(x=meanother, y=intrinsicLDGRchr, color=grassland_type))+
  #geom_point()+
  geom_smooth(method="lm")+
  geom_hline(aes(yintercept=0), linetype="dotted")+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(y=expression(italic(r)[rintD]), x = "Interspecific cover (%)")+
  theme(legend.position="none")

##combine
#figure together
covfig <- ggarrange(gcover,chrcov,concov, nrow=3, ncol=1, common.legend = F, 
                    labels = c("i","j","k"), label.x = .9)

#all combined summary stats figure for paper
summaryfig <- ggarrange(histboxcombo,covfig,nrow=1, ncol=2, widths = c(2,1.5))
#add legend
forlegend <- ggplot(covergrassland,aes(x=meanother, y=condiff, color=grassland_type))+
  geom_point()+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(color="Grassland type")+
  theme(legend.direction = "horizontal")+
  guides(colour = guide_legend(nrow = 1))
combinedlegend <- as_ggplot(extract_legend(forlegend))
summaryfig <- ggarrange(summaryfig,combinedlegend,nrow=2, ncol=1, heights = c(2,.15))
summaryfig

#export
ggsave(summaryfig, filename = "figures/summaryfig.jpg", dpi=300, height = 8,width =9)



###other figures
library(visreg)
LDMCmodA <- lm(invasionLDGRcon~LDMC*grassland_type, data=alldat)
summary(LDMCmodA) 
anova(LDMCmodA)
trtfig <- visreg(LDMCmodA, xvar="LDMC", by="grassland_type", rug=F, 
                 partial=F,xlab= "log(leaf dry matter content)", ylab=" ", 
                 line=list(col="black"), gg=T)+
  geom_rug(sides="b")+
  theme(axis.title.y.left=element_blank())+
  coord_cartesian(ylim = c(-3, 4))+
  theme_classic()

LDMCmodD <- lm(intrinsicLDGRchr~LDMC*grassland_type, data=alldat)
summary(LDMCmodD) 
anova(LDMCmodD)
trtfig2 <- visreg(LDMCmodD, xvar="LDMC", by="grassland_type", rug=F, 
                  partial=F,xlab= "log(leaf dry matter content)", ylab=" ", 
                  line=list(col="black"), gg=T)+
  geom_rug(sides="b")+
  theme(axis.title.y.left=element_blank())+
  coord_cartesian(ylim = c(-3, 4))+
  theme_classic()



## is it fucked up?
CHY <- cover %>% 
  filter(site == "CHY" & year != "2017" & trt != "int") %>% # filter dataset 
  group_by(species) %>%
  mutate(cnt = n()) %>%
  filter(cnt >= 50) %>%
  ungroup()
##ANCOVA across different treatments: extract LDGR, slopes. produce multipanel plot.
spp =  unique(CHY$species) # list unique spp in CHY dataset
##ANCOVA 
ancova = list()
csc = list()
coc = list()
##create dataframe to for ancova outputs
NEWalldat <- data.frame(species = NA, intrinsicLDGRchrOLD=NA,intrinsicLDGRchr=NA,invasionLDGRcon=NA, invasionLDGRconOLD=NA,weight=NA,
                        effectN=NA, effectND=NA)
##loop for running ancova on each species and storing and manipulating outputs
for (l in 1:length(spp)){
  CHYsp <-  CHY[CHY$species == spp[l],]
  CHYsp$trt <- relevel(as.factor(CHYsp$trt), ref = "con")
  ancova[[l]] <-lm(log_lambda~log_cover*trt + log_other*trt, data=CHYsp)
  ##finds minimum for intrinsic growth rates
  Cmin <- CHYsp %>% filter(trt=="chr")
  mmcov <- min(Cmin$log_cover)
  Cmin2 <- CHYsp %>% filter(trt=="chr")
  mmcr <- min(Cmin2$log_other)
  ##finds mean for invasion
  Csub <- CHYsp %>% filter(trt=="con")
  mco <- mean(Csub$log_other)
  Csub2 <- CHYsp %>% filter(trt=="con")
  mcov <- mean(Csub2$log_cover)
  ##assigns species names to column one in the order they were looped
  NEWalldat[l,1] <- spp[l]
  ##assigns the list of coef values to the proper column in the order of the loop
  NEWalldat[l,2] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mmcr*coef(ancova[[l]])[4]) + (mmcr*coef(ancova[[l]])[6]) #find growth rate when cover intra = 0 and inter=min in drought (non-zero betas)
  NEWalldat[l,3] <- coef(ancova[[l]])[1] +(mmcov*coef(ancova[[l]])[2]) + (mmcov*coef(ancova[[l]])[5]) + coef(ancova[[l]])[3] + (mmcr*coef(ancova[[l]])[4]) + (mmcr*coef(ancova[[l]])[6]) #find growth rate when cover intra = 0 and inter=min in drought (non-zero betas)
  NEWalldat[l,4] <- coef(ancova[[l]])[1] +(mcov*coef(ancova[[l]])[2])+ (mco*coef(ancova[[l]])[4]) #find growth rate when cover intra = 0 and inter=mean in ambient
  NEWalldat[l,5] <- coef(ancova[[l]])[1] + (mco*coef(ancova[[l]])[4]) #find growth rate when cover intra = 0 and inter=mean in ambient
  NEWalldat[l,6] <- summary(ancova[[l]])$sigma #se for weighting estimates
}

teest <- NEWalldat
teest <- NEWalldat %>% mutate(weight2 = 1/weight)
teest <- NEWallsite %>% filter(grassland_type=="Northern Mixed")
summary(tmod <-lm(invasionLDGRcon~intrinsicLDGRchr, weights = weight2, teest)) #*** 43% 
sqrt(summary(tmod)$adj.r.squared) #calculate r
anova(tmod) 
