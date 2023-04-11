#### data wrangling ####
# Take EDGE data, clean, and calculate annual population growth/ lambda (DHA wrote this). For each 
# grassland calculate population growth rates in different conditions. Combine into main dataframe
# and clean to make neat and usable downstream in analysis. Add trait data for master trait data.
# Create Figure S1 of population cover in relation to intra- and inter-specific cover.

#### data ####
# read CSV from Dave code to calculate lambda
cover <- read.csv("data/cover.csv") 

#library loading
library(tidyverse)
library(gridExtra)

#### calculating growth rates ####
# For each site the loop subsets and cleans, runs ancova and stores results, and create figures

##Cheyenne (CHY)
##subset by site, remove 2017 N/A rows + intense treatment + sp W/ less than 50 observations
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
NEWalldat <- data.frame(species = NA, intrinsicLDGRcon=NA,intrinsicLDGRchr=NA,invasionLDGRcon=NA, invasionLDGRchr=NA,weight=NA,
                        effectN=NA, effectND=NA)
##loop for running ancova on each species and storing and manipulating outputs
for (l in 1:length(spp)){
  CHYsp <-  CHY[CHY$species == spp[l],]
  CHYsp$trt <- relevel(as.factor(CHYsp$trt), ref = "con")
  ancova[[l]] <-lm(log_lambda~log_cover*trt + log_other*trt, data=CHYsp)
  ##finds minimum for intrinsic growth rates
  Cmin <- CHYsp %>% filter(trt=="con")
  mmco <- min(Cmin$log_other)
  Cmin2 <- CHYsp %>% filter(trt=="chr")
  mmcr <- min(Cmin2$log_other)
  ##finds mean for invasion
  Csub <- CHYsp %>% filter(trt=="con")
  mco <- mean(Csub$log_other)
  Csub2 <- CHYsp %>% filter(trt=="chr")
  mcr <- mean(Csub2$log_other)
  ##assigns species names to column one in the order they were looped
  NEWalldat[l,1] <- spp[l]
  ##assigns the list of coef values to the proper column in the order of the loop
  NEWalldat[l,2] <- coef(ancova[[l]])[1] + (mmco*coef(ancova[[l]])[4]) #find growth rate when cover intra = 0 and inter=min in ambient
  NEWalldat[l,3] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mmcr*coef(ancova[[l]])[4]) + (mmcr*coef(ancova[[l]])[6]) #find growth rate when cover intra = 0 and inter=min in drought (non-zero betas)
  NEWalldat[l,4] <- coef(ancova[[l]])[1] + (mco*coef(ancova[[l]])[4]) #find growth rate when cover intra = 0 and inter=mean in ambient
  NEWalldat[l,5] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6]) #find growth rate when cover intra = 0 and inter=mean in drought (non-zero betas)
  NEWalldat[l,6] <- summary(ancova[[l]])$sigma #se for weighting estimates
  ##assigns the betas to the proper column in the order of the loop
  NEWalldat[l,7] <- (mco*coef(ancova[[l]])[4]) #get effect of neighbors when cover intra = 0 and inter=mean in ambient
  NEWalldat[l,8] <- (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6]) #get effect of neighbors when cover intra = 0 and inter=mean in drought
#  NEWalldat[l,9] <- (mmco*coef(ancova[[l]])[4]) #get effect of drought when cover intra = 0 and inter=min in ambient
#  NEWalldat[l,10] <- (mmcr*coef(ancova[[l]])[4]) + (mmcr*coef(ancova[[l]])[6]) #get effect of neighbors when cover intra = 0 and inter=min in drought
  # create a figure for each species with intra- and inter-specific cover (for Figure S1)
  csc[[l]] <- ggplot(CHYsp, aes(x=log_cover, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none",
                                 axis.title.y = element_blank(),
                                 axis.title.x = element_blank(),
                                 plot.margin=unit(c(.5,0,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
  coc[[l]] <- ggplot(CHYsp, aes(x=log_other, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", 
                                 axis.title.y = element_blank(), 
                                 axis.title.x = element_blank(), 
                                 plot.margin=unit(c(.5,1,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
}
##add columns to calculate the difference in LDGR btwn con and chr
NEWalldat$intrinsicdiff = NEWalldat$intrinsicLDGRchr - NEWalldat$intrinsicLDGRcon
NEWalldat$invasiondiff = NEWalldat$invasionLDGRchr - NEWalldat$invasionLDGRcon


## Hays (HYS)
##subset by site, remove 2017 N/A rows + intense treatment + sp W/ less than 50 observations
HYS <- cover %>% 
  filter(site == "HYS" & year != "2017" & trt != "int") %>% # filter dataset 
  group_by(species) %>%
  mutate(cnt = n()) %>%
  filter(cnt >= 50) %>%
  ungroup() %>%
  filter(species != "Drabareptans") # always 0's in cover, cannot run analysis/graph

##list unique spp in HYS dataset
spp =  unique(HYS$species)
##ANCOVA 
ancova = list()
csh = list()
coh = list()
##create dataframe to for ancova outputs
NEWallHYS <- data.frame(species = NA, intrinsicLDGRcon=NA,intrinsicLDGRchr=NA,invasionLDGRcon=NA, invasionLDGRchr=NA, weight=NA,
                        effectN=NA, effectND=NA)
##loop for running ancova on each species and storing and manipulating outputs
for (l in 1:length(spp)){
  HYSsp <-  HYS[HYS$species == spp[l],]
  HYSsp$trt <- relevel(as.factor(HYSsp$trt), ref = "con")
  ancova[[l]] <-lm(log_lambda~log_cover*trt + log_other*trt, data=HYSsp)
  ##finds minimum for intrinsic growth rates
  Hmin <- HYSsp %>% filter(trt=="con")
  mmco <- min(Hmin$log_other)
  Hmin2 <- HYSsp %>% filter(trt=="chr")
  mmcr <- min(Hmin2$log_other)
  ##finds mean for invasion
  Hsub <- HYSsp %>% filter(trt=="con")
  mco <- mean(Hsub$log_other)
  Hsub2 <- HYSsp %>% filter(trt=="chr")
  mcr <- mean(Hsub2$log_other)
  ##assigns species names to column one in the order they were looped
  NEWallHYS[l,1] <- spp[l] 
  ##assigns the list of coef values to the proper column in the order of the loop
  NEWallHYS[l,2] <- coef(ancova[[l]])[1] + (mmco*coef(ancova[[l]])[4])
  NEWallHYS[l,3] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mmcr*coef(ancova[[l]])[4]) + + (mmcr*coef(ancova[[l]])[6])#add beta 4 and 6
  NEWallHYS[l,4] <- coef(ancova[[l]])[1] + (mco*coef(ancova[[l]])[4])
  NEWallHYS[l,5] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mcr*coef(ancova[[l]])[4]) + + (mcr*coef(ancova[[l]])[6])
  NEWallHYS[l,6] <- summary(ancova[[l]])$sigma
  ##assigns the betas to the proper column in the order of the loop
  NEWallHYS[l,7] <- (mco*coef(ancova[[l]])[4]) #get effect of neighbors when cover intra = 0 and inter=mean in ambient
  NEWallHYS[l,8] <- (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6]) #get effect of neighbors when cover intra = 0 and inter=mean in drought
  # create a figure for each species with intra- and inter-specific cover (for Figure S1)
  csh[[l]] <- ggplot(HYSsp, aes(x=log_cover, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", plot.margin=unit(c(.5,0,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
  coh[[l]] <- ggplot(HYSsp, aes(x=log_other, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), plot.margin=unit(c(.5,1,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
}
##add columns to calculate the difference in LDGR btwn con and chr
NEWallHYS$intrinsicdiff = NEWallHYS$intrinsicLDGRchr - NEWallHYS$intrinsicLDGRcon
NEWallHYS$invasiondiff = NEWallHYS$invasionLDGRchr - NEWallHYS$invasionLDGRcon


## Konza
##subset by site, remove 2017 N/A rows + intense treatment + sp W/ less than 50 observations
KNZ <- cover %>% 
  filter(site == "KNZ" & year != "2017" & trt != "int") %>% # filter dataset 
  group_by(species) %>%
  mutate(cnt = n()) %>%
  filter(cnt >= 50) %>%
  ungroup()

##list unique spp in KNZ dataset
spp =  unique(KNZ$species)
##ANCOVA 
ancova = list()
csk = list()
cok = list()
##create dataframe to for ancova outputs
NEWallKNZ <- data.frame(species = NA, intrinsicLDGRcon=NA,intrinsicLDGRchr=NA,invasionLDGRcon=NA, invasionLDGRchr=NA,weight=NA,
                        effectN=NA, effectND=NA)
##loop for running ancova on each species and storing and manipulating outputs
for (l in 1:length(spp)){
  KNZsp <-  KNZ[KNZ$species == spp[l],]
  KNZsp$trt <- relevel(as.factor(KNZsp$trt), ref = "con")
  ancova[[l]] <-lm(log_lambda~log_cover*trt + log_other*trt, data=KNZsp)
  ##finds minimum for intrinsic growth rates
  Kmin <- KNZsp %>% filter(trt=="con")
  mmco <- min(Kmin$log_other)
  Kmin2 <- KNZsp %>% filter(trt=="chr")
  mmcr <- min(Kmin2$log_other)
  ##finds mean for invasion
  Ksub <- KNZsp %>% filter(trt=="con")
  mco <- mean(Ksub$log_other)
  Ksub2 <- KNZsp %>% filter(trt=="chr")
  mcr <- mean(Ksub2$log_other)
  ##assigns species names to column one in the order they were looped
  NEWallKNZ[l,1] <- spp[l]
  ##assigns the list of coef values to the proper column in the order of the loop
  NEWallKNZ[l,2] <- coef(ancova[[l]])[1] + (mmco*coef(ancova[[l]])[4]) 
  NEWallKNZ[l,3] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mmcr*coef(ancova[[l]])[4]) + (mmcr*coef(ancova[[l]])[6])
  NEWallKNZ[l,4] <- coef(ancova[[l]])[1] + (mco*coef(ancova[[l]])[4])
  NEWallKNZ[l,5] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6])
  NEWallKNZ[l,6] <- summary(ancova[[l]])$sigma
  ##assigns the betas to the proper column in the order of the loop
  NEWallKNZ[l,7] <- (mco*coef(ancova[[l]])[4]) #get effect of neighbors when cover intra = 0 and inter=mean in ambient
  NEWallKNZ[l,8] <- (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6]) #get effect of neighbors when cover intra = 0 and inter=mean in drought
  # create a figure for each species with intra- and inter-specific cover (for Figure S1)
  csk[[l]] <- ggplot(KNZsp, aes(x=log_cover, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", plot.margin=unit(c(.5,0,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
  cok[[l]] <- ggplot(KNZsp, aes(x=log_other, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), plot.margin=unit(c(.5,1,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
}
##add columns to calculate the difference in LDGR btwn con and chr
NEWallKNZ$intrinsicdiff = NEWallKNZ$intrinsicLDGRchr - NEWallKNZ$intrinsicLDGRcon
NEWallKNZ$invasiondiff = NEWallKNZ$invasionLDGRchr - NEWallKNZ$invasionLDGRcon 


## SGS 
##subset by site, remove 2017 N/A rows + intense treatment + sp W/ less than 50 observations
SGS <- cover %>% 
  filter(site == "SGS" & year != "2017" & trt != "int") %>% # filter dataset 
  group_by(species) %>%
  mutate(cnt = n()) %>%
  filter(cnt >= 50) %>%
  ungroup()%>%
  filter(species != "Chenopodiumleptophyllum") ##lambda always 0 or 1's in cover

##list unique spp in SGS dataset
spp =  unique(SGS$species)
##ANCOVA 
ancova = list()
css = list()
cos = list()
##create dataframe to for ancova outputs
NEWallSGS <- data.frame(species = NA, intrinsicLDGRcon=NA,intrinsicLDGRchr=NA,invasionLDGRcon=NA, invasionLDGRchr=NA,weight=NA,
                        effectN=NA, effectND=NA)
##loop for running ancova on each species and storing and manipulating outputs
for (l in 1:length(spp)){
  SGSsp <-  SGS[SGS$species == spp[l],]
  SGSsp$trt <- relevel(as.factor(SGSsp$trt), ref = "con")
  ancova[[l]] <-lm(log_lambda~log_cover*trt + log_other*trt, data=SGSsp)
  ##finds minimum for intrinsic growth rates
  Smin <- SGSsp %>% filter(trt=="con")
  mmco <- min(Smin$log_other)
  Smin2 <- SGSsp %>% filter(trt=="chr")
  mmcr <- min(Smin2$log_other)
  ##finds mean for invasion
  Ssub <- SGSsp %>% filter(trt=="con")
  mco <- mean(Ssub$log_other)
  Ssub2 <- SGSsp %>% filter(trt=="chr")
  mcr <- mean(Ssub2$log_other)
  ##assigns species names to column one in the order they were looped
  NEWallSGS[l,1] <- spp[l]
  ##assigns the list of coef values to the proper column in the order of the loop
  NEWallSGS[l,2] <- coef(ancova[[l]])[1] + (mmco*coef(ancova[[l]])[4])
  NEWallSGS[l,3] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mmcr*coef(ancova[[l]])[4]) + (mmcr*coef(ancova[[l]])[6])
  NEWallSGS[l,4] <- coef(ancova[[l]])[1] + (mco*coef(ancova[[l]])[4])
  NEWallSGS[l,5] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6])
  NEWallSGS[l,6] <- summary(ancova[[l]])$sigma
  ##assigns the betas to the proper column in the order of the loop
  NEWallSGS[l,7] <- (mco*coef(ancova[[l]])[4]) #get effect of neighbors when cover intra = 0 and inter=mean in ambient
  NEWallSGS[l,8] <- (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6]) #get effect of neighbors when cover intra = 0 and inter=mean in drought
  # create a figure for each species with intra- and inter-specific cover (for Figure S1)
  css[[l]] <- ggplot(SGSsp, aes(x=log_cover, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", plot.margin=unit(c(.5,0,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
  cos[[l]] <- ggplot(SGSsp, aes(x=log_other, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), plot.margin=unit(c(.5,1,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
}
##add columns to calculate the difference in LDGR btwn con and chr
NEWallSGS$intrinsicdiff = NEWallSGS$intrinsicLDGRchr - NEWallSGS$intrinsicLDGRcon
NEWallSGS$invasiondiff = NEWallSGS$invasionLDGRchr - NEWallSGS$invasionLDGRcon 


##NMblue (sevietta)
##subset by site, remove 2017 N/A rows + sp W/ less than 50 observations
SBL <- cover %>% 
  filter(site == "sev.blue" & year != "2017") %>%   #filter dataset 
  group_by(species) %>%
  mutate(cnt = n()) %>%
  filter(cnt >= 50) %>%
  ungroup() %>%
  filter(species != "BOER4" & surv != 0) ## 0's in lambda make NA's here, remove
##decimals in cover give negatives, +1 to all values at this site
#SBL$cover <- SBL$cover+1
#SBL$log_cover <- log(SBL$cover)
##replace species codes with Latin names
library(stringr)
SBL$species = str_replace(SBL$species,"SPORO","Sporobolussp.")
SBL$species = str_replace(SBL$species,"EUEX4","Euphorbiaexstipulata")
SBL$species = str_replace(SBL$species,"GUSA2","Gutierreziasarothrae")
SBL$species = str_replace(SBL$species,"MUAR2","Muhlenbergiaarenicola")
SBL$species = str_replace(SBL$species,"CHLA10","Chamaesycelata")
SBL$species = str_replace(SBL$species,"HODR","Hoffmannseggiadrepanocarpa")
SBL$species = str_replace(SBL$species,"LIPU4","Linumpuberulum")
SBL$species = str_replace(SBL$species,"CHSE7","Chamaesyceserrula")
SBL$species = str_replace(SBL$species,"KAPA","Kallstroemiaparviflora")
SBL$species = str_replace(SBL$species,"PLJA","Pleuraphisjamesii")
SBL$species = str_replace(SBL$species,"ASTRA","Astragalussp.")
SBL$species = str_replace(SBL$species,"ALIN","Allioniaincarnata")
SBL$species = str_replace(SBL$species,"SPPO6","Sphaeralceapolychroma")
SBL$species = str_replace(SBL$species,"OECA10","Oenotheracaespitosa")
SBL$species = str_replace(SBL$species,"PLPA2","Plantagopatagonica")
SBL$species = str_replace(SBL$species,"CRCR3","Cryptanthacrassisepala")
SBL$species = str_replace(SBL$species,"MAPIP","Machaerantherapinnatifida")
SBL$species = str_replace(SBL$species,"LEFE","Lesquerellafendleri")
SBL$species = str_replace(SBL$species,"DEPI","Descurainiapinnata")
SBL$species = str_replace(SBL$species,"PHCR","Phaceliacrenulata")
SBL$species = str_replace(SBL$species,"ASFE2","Astragalusfeensis")
SBL$species = str_replace(SBL$species,"IPPU4","Ipomopsispumila")

##list unique spp in SBL dataset
spp =  unique(SBL$species)
##ANCOVA 
ancova = list()
cssb = list()
cosb = list()
##create dataframe to for ancova outputs
NEWallSBL <- data.frame(species = NA, intrinsicLDGRcon=NA,intrinsicLDGRchr=NA,invasionLDGRcon=NA,invasionLDGRchr=NA,weight=NA,
                        effectN=NA, effectND=NA)
##loop for running ancova on each species and storing and manipulating outputs
for (l in 1:length(spp)){
  SBLsp <-  SBL[SBL$species == spp[l],]
  SBLsp$trt <- relevel(as.factor(SBLsp$trt), ref = "con")
  ancova[[l]] <-lm(log_lambda~log_cover*trt + log_other*trt, data=SBLsp)
  ##finds minimum for intrinsic growth rates
  SBLmin <- SBLsp %>% filter(trt=="con")
  mmco <- min(SBLmin$log_other)
  SBLmin2 <- SBLsp %>% filter(trt=="chr")
  mmcr <- min(SBLmin2$log_other)
  ##finds mean for invasion
  SBLsub <- SBLsp %>% filter(trt=="con")
  mco <- mean(SBLsub$log_other)
  SBLsub2 <- SBLsp %>% filter(trt=="chr")
  mcr <- mean(SBLsub2$log_other)
  ##assigns species names to column one in the order they were looped
  NEWallSBL[l,1] <- spp[l]
  ##assigns the list of coef values to the proper column in the order of the loop
  NEWallSBL[l,2] <- coef(ancova[[l]])[1] + (mmco*coef(ancova[[l]])[4])
  NEWallSBL[l,3] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mmcr*coef(ancova[[l]])[4]) + (mmcr*coef(ancova[[l]])[6])
  NEWallSBL[l,4] <- coef(ancova[[l]])[1] + (mco*coef(ancova[[l]])[4])
  NEWallSBL[l,5] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6])
  NEWallSBL[l,6] <- summary(ancova[[l]])$sigma
  ##assigns the betas to the proper column in the order of the loop
  NEWallSBL[l,7] <- (mco*coef(ancova[[l]])[4]) #get effect of neighbors when cover intra = 0 and inter=mean in ambient
  NEWallSBL[l,8] <- (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6]) #get effect of neighbors when cover intra = 0 and inter=mean in drought
  # create a figure for each species with intra- and inter-specific cover (for Figure S1)
  cssb[[l]] <- ggplot(SBLsp, aes(x=log_cover, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", plot.margin=unit(c(.5,0,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
  cosb[[l]] <- ggplot(SBLsp, aes(x=log_other, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), plot.margin=unit(c(.5,1,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
}
##add columns to calculate the difference in LDGR btwn con and chr
NEWallSBL$intrinsicdiff = NEWallSBL$intrinsicLDGRchr - NEWallSBL$intrinsicLDGRcon 
NEWallSBL$invasiondiff = NEWallSBL$invasionLDGRchr - NEWallSBL$invasionLDGRcon 


##NMblack
SBK <- cover %>% 
  filter(site == "sev.black" & year != "2017") %>%   #filter dataset
  group_by(species) %>%
  mutate(cnt = n()) %>%
  filter(cnt >= 50) %>%
  ungroup() %>%
  filter(species != "HYFIC" & surv != 0) ## 0's in lambda make NA's here, remove
##decimals in cover give negatives, +1 to all values at this site
#SBK$cover <- SBK$cover+1
#SBK$log_cover <- log(SBK$cover)
##replace species codes with latin names
SBK$species = str_replace(SBK$species,"SPORO","Sporobolussp.")
SBK$species = str_replace(SBK$species,"EUEX4","Euphorbiaexstipulata")
SBK$species = str_replace(SBK$species,"BOER4","Boutelouaeriopoda")
SBK$species = str_replace(SBK$species,"MUAR2","Muhlenbergiaarenicola")
SBK$species = str_replace(SBK$species,"CHAL11","Chamaesycealbomarginata")
SBK$species = str_replace(SBK$species,"ALIN","Allioniaincarnata")
SBK$species = str_replace(SBK$species,"KAPA","Kallstroemiaparviflora")
SBK$species = str_replace(SBK$species,"ASTRA","Astragalussp.")
SBK$species = str_replace(SBK$species,"SPPO6","Sphaeralceapolychroma")
SBK$species = str_replace(SBK$species,"OECA10","Oenotheracaespitosa")
SBK$species = str_replace(SBK$species,"PLPA2","Plantagopatagonica")
SBK$species = str_replace(SBK$species,"DEPI","Descurainiapinnata")
SBK$species = str_replace(SBK$species,"ASFE2","Astragalusfeensis")
SBK$species = str_replace(SBK$species,"ESVIV","Escobariavivipara")
SBK$species = str_replace(SBK$species,"BOGR2","Boutelouagracilis")
SBK$species = str_replace(SBK$species,"GACO5","Gauracoccinea")
SBK$species = str_replace(SBK$species,"GLBI2","Glandulariabipinnatifida")
SBK$species = str_replace(SBK$species,"CHSES","Chamaesyceserpyllifolia")
SBK$species = str_replace(SBK$species,"ECIN2","Echinomastusintertextus")
SBK$species = str_replace(SBK$species,"SEBA3","Sennabauhinioides")
SBK$species = str_replace(SBK$species,"STPA4","Stephanomeriapauciflora")
SBK$species = str_replace(SBK$species,"SPHA","Sphaeralceahastulata")
SBK$species = str_replace(SBK$species,"LOPL2","Lotusplebeius")
SBK$species = str_replace(SBK$species,"LEDED","Lepidiumdensiflorum")
SBK$species = str_replace(SBK$species,"ALMA4","Alliummacropetalum")
SBK$species = str_replace(SBK$species,"ARAD","Aristidaadscensionis")
SBK$species = str_replace(SBK$species,"SATR12","Salsolatragus")
SBK$species = str_replace(SBK$species,"DAPU7","Dasyochloapulchella")
SBK$species = str_replace(SBK$species,"ZIGR","Zinniagrandiflora")

##list unique spp in SBK dataset
spp =  unique(SBK$species)
##ANCOVA 
ancova = list()
cssbk = list()
cosbk = list()
##create dataframe to for ancova outputs
NEWallSBK <- data.frame(species = NA, intrinsicLDGRcon=NA,intrinsicLDGRchr=NA,invasionLDGRcon=NA,invasionLDGRchr=NA,weight=NA,
                        effectN=NA, effectND=NA)
##loop for running ancova on each species and storing and manipulating outputs
for (l in 1:length(spp)){
  SBKsp <-  SBK[SBK$species == spp[l],]
  SBKsp$trt <- relevel(as.factor(SBKsp$trt), ref = "con")
  ancova[[l]] <-lm(log_lambda~log_cover*trt + log_other*trt, data=SBKsp)
  ##finds minimum for intrinsic growth rates
  SBKmin <- SBKsp %>% filter(trt=="con")
  mmco <- min(SBKmin$log_other)
  SBKmin2 <- SBKsp %>% filter(trt=="chr")
  mmcr <- min(SBKmin2$log_other)
  ##finds mean for invasion
  SBKsub <- SBKsp %>% filter(trt=="con")
  mco <- mean(SBKsub$log_other)
  SBKsub2 <- SBKsp %>% filter(trt=="chr")
  mcr <- mean(SBKsub2$log_other)
  ##assigns species names to column one in the order they were looped
  NEWallSBK[l,1] <- spp[l]
  ##assigns the list of coef values to the proper column in the order of the loop
  NEWallSBK[l,2] <- coef(ancova[[l]])[1] + (mmco*coef(ancova[[l]])[4])
  NEWallSBK[l,3] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mmcr*coef(ancova[[l]])[4]) + (mmcr*coef(ancova[[l]])[6])
  NEWallSBK[l,4] <- coef(ancova[[l]])[1] + (mco*coef(ancova[[l]])[4])
  NEWallSBK[l,5] <- coef(ancova[[l]])[1] + coef(ancova[[l]])[3] + (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6])
  NEWallSBK[l,6] <- summary(ancova[[l]])$sigma
  ##assigns the betas to the proper column in the order of the loop
  NEWallSBK[l,7] <- (mco*coef(ancova[[l]])[4]) #get effect of neighbors when cover intra = 0 and inter=mean in ambient
  NEWallSBK[l,8] <- (mcr*coef(ancova[[l]])[4]) + (mcr*coef(ancova[[l]])[6]) #get effect of neighbors when cover intra = 0 and inter=mean in drought
  # create a figure for each species with intra- and inter-specific cover (for Figure S1)
  cssbk[[l]] <- ggplot(SBKsp, aes(x=log_cover, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", plot.margin=unit(c(.5,0,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
  cosbk[[l]] <- ggplot(SBKsp, aes(x=log_other, y=log_lambda, color = trt)) + geom_point() + geom_smooth(method="lm") + 
    facet_wrap(~species) + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), plot.margin=unit(c(.5,1,.5,.5), "cm")) + scale_color_manual(values = c("blue", "red"))
}
##add columns to calculate the difference in LDGR btwn con and chr
NEWallSBK$intrinsicdiff = NEWallSBK$intrinsicLDGRchr - NEWallSBK$intrinsicLDGRcon
NEWallSBK$invasiondiff = NEWallSBK$invasionLDGRchr - NEWallSBK$invasionLDGRcon 


## Bind all site data together
all <- bind_rows(CHY, HYS, KNZ, SGS, SBL, SBK) ##all raw site data
write.csv(all, "data/allraw.csv", row.names = F)

NEWalldata <- bind_rows(NEWalldat, NEWallHYS, NEWallKNZ, NEWallSGS, NEWallSBL, NEWallSBK, .id = "source") ##all calculated site data

## find response to neighbors
NEWalldata$condiff = NEWalldata$invasionLDGRcon - NEWalldata$intrinsicLDGRcon #in ambient
NEWalldata$chrdiff = NEWalldata$invasionLDGRchr - NEWalldata$intrinsicLDGRchr #in drought

write.csv(NEWalldata, file='data/alldata.csv', row.names = F) #make csv
write.csv(NEWalldata, file='data/alldata_redo.csv', row.names = F) #make csv

## Figure S1 shows the relationships modelled above, see 'figurs.R' script.


#### Adding trait data and editing dataframe ####
#load in master trait data with values from TRY, DB, AS, ST and 
MasterTrait <- read.csv("data/MasterTrait.csv")

##filter down to site level
CHYmastertrait <- MasterTrait %>%
  filter(site == "CHY")
NEWalldat <- NEWalldata %>%
  filter(source == "1")
##combine LDGR data by site to traits
CHYtrait <- merge(CHYmastertrait, NEWalldat, by.x = "Species", by.y = "species", all = F)

#other sites:
##HYS
HYSmastertrait <- MasterTrait %>%
  filter(site == "HYS")
NEWallHYS <- NEWalldata %>%
  filter(source == "2")
##combine LDGR data by site to traits
HYStrait <- merge(HYSmastertrait, NEWallHYS, by.x = "Species", by.y = "species", all = F)

##KNZ
KNZmastertrait <- MasterTrait %>%
  filter(site == "KNZ")
NEWallKNZ <- NEWalldata %>%
  filter(source == "3")
##combine LDGR data by site to traits
KNZtrait <- merge(KNZmastertrait, NEWallKNZ, by.x = "Species", by.y = "species", all = F)

##SGS
SGSmastertrait <- MasterTrait %>%
  filter(site == "SGS")
NEWallSGS <- NEWalldata %>%
  filter(source == "4")
##combine LDGR data by site to traits
SGStrait <- merge(SGSmastertrait, NEWallSGS, by.x = "Species", by.y = "species", all = F)

##SBL
SBLmastertrait <- MasterTrait %>%
  filter(site == "SBU")
NEWallSBL <- NEWalldata %>%
  filter(source == "5")
##combine LDGR data by site to traits
SBLtrait <- merge(SBLmastertrait, NEWallSBL, by.x = "Species", by.y = "species", all = F)

##SBK
SBKmastertrait <- MasterTrait %>%
  filter(site == "SBK")
NEWallSBK <- NEWalldata %>%
  filter(source == "6")
##combine LDGR data by site to traits
SBKtrait <- merge(SBKmastertrait, NEWallSBK, by.x = "Species", by.y = "species", all = F)

##recombine all sites 
NEWallsite <- bind_rows(CHYtrait, HYStrait, KNZtrait, SGStrait, SBLtrait, SBKtrait) #make dataframe
#NEWallsite[,19:22] <- round(NEWallsite[,19:22], digits = 6) #reduce decimal places if wanted

#adjust weights so more certain estimates have higher number/certainty
NEWallsite$weight2 <- 1/NEWallsite$weight



#### clean ####
##add grassland type as column
NEWallsite <- NEWallsite %>% mutate(grassland_type = ifelse(site %in% "CHY","Northern Mixed",
                                                            ifelse(site %in% "HYS","Southern Mixed",
                                                                   ifelse(site %in% "KNZ","Tallgrass",
                                                                          ifelse(site %in% "SGS","Northern Shortgrass",
                                                                                 ifelse(site %in% "SBU","Great Plains Shortgrass","Chihuahuan Desert"))))))

##reduce shrubs into forbs, remove old column
NEWallsite <- NEWallsite %>% mutate(grass.forb = ifelse(grass.forb.shrub %in% c("S", "F"),"Other", "Grass"), grass.forb.shrub=NULL)
##combine annual and biennial
NEWallsite <- NEWallsite %>% mutate(lifespan = ifelse(annual.Perennial %in% c("Annual", "Biennial"),"Short-lived", "Perennial"), grass.forb.shrub=NULL)

#edit column names to make them easy to write out
NEWallsite <- NEWallsite %>%
  rename(SLA = SLA..cm2.g..cm2.g.1.,
         LTD = Leaf.tissue.density..gcm.3.,
         leafN = Leaf.N....,
         leafarea = Leaf.area..cm2.,
         LDMC =LDMC..g.g.,
         TLP = tugor.loss.point,
         SRL = SRL..mm.g.,
         rootN = Root.N..,
         RTD = Root.tissue.density.gcm.3.,
         rootdiam = Root.diameter..mm.,
         height = max.height.mm.,
         species = Species
  )

#make TLP positive so it can be logged with everything else
NEWallsite <- NEWallsite %>% mutate(TLP_tran = abs(TLP))
#make negative again for any figures/ interpretation 
NEWallsite <- NEWallsite %>% mutate(TLP_tran2 = log(TLP_tran)*-1) 


#relevel to view grassland_type facets along precipitation gradient 
NEWallsite <- NEWallsite %>%
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Chihuahuan Desert", "Great Plains Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))


#save
write.csv(NEWallsite, file='data/allsite_new.csv', row.names = F) #make csv

#clean environment, save final data frame only
rm(list=setdiff(ls(), "NEWallsite"))
