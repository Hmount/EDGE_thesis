#### calculate CWM ####
# CWM for each population is average trait weighted by neighbor abundance (which is a 
# subset of the community) not including the trait value of the focal species. 

library(tidyverse)
library(FD)

#merge cover data and trait data
#change site names to grassland to match when merging dataframes and clean up trait names
MasterTrait <- read.csv("data/MasterTrait.csv") #trait data
MasterTraitNew <- MasterTrait %>% mutate(grassland_type = ifelse(site %in% "CHY","Northern Mixed",
                                                                 ifelse(site %in% "HYS","Southern Mixed",
                                                                        ifelse(site %in% "KNZ","Tallgrass",
                                                                               ifelse(site %in% "SGS","Northern Shortgrass",
                                                                                      ifelse(site %in% "SBU","Great Plains Shortgrass","Chihuahuan Desert"))))))

all<- read.csv("data/allraw.csv")#raw site data
allnew <- all %>% mutate(grassland_type = ifelse(site %in% "CHY","Northern Mixed",
                                                 ifelse(site %in% "HYS","Southern Mixed",
                                                        ifelse(site %in% "KNZ","Tallgrass",
                                                               ifelse(site %in% "SGS","Northern Shortgrass",
                                                                      ifelse(site %in% "sev.blue","Great Plains Shortgrass","Chihuahuan Desert"))))))

#edit column names to make them easy to write out
MasterTraitNew <- MasterTraitNew %>%
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
         height = max.height.mm.
  )
#make TLP positive so it can be logged with everything else
MasterTraitNew <- MasterTraitNew %>% mutate(TLP_tran = abs(TLP))
#make negative again for figures
MasterTraitNew <- MasterTraitNew %>% mutate(TLP_tran2 = log(TLP_tran)*-1)  #make neg. to interpret

#find relative coverage of each subplot with traits
allnewsum0 <- allnew %>% group_by(grassland_type, species, year,trt,plot) %>% #group and order variables 
  summarise(abscov = sum(cover)) #sum absolute cover of each species/ grassland/ year/ trt/ plot (quadrat/subplot cover)
#average for populations
allnewsum <- allnewsum0 %>% group_by(grassland_type, species) %>% #group and order variables 
  summarise(meanabscov = mean(abscov))

#merge
awt <- merge(allnewsum, MasterTraitNew, by.x=c("species", "grassland_type"), by.y=c("Species", "grassland_type"))

awt1 <- awt %>% 
  group_by(grassland_type) %>%
  mutate(cov_height = sum(meanabscov[!is.na(height)]),
         cov_SLA = sum(meanabscov[!is.na(SLA)]),
         cov_SRL = sum(meanabscov[!is.na(SRL)]),
         cov_LTD = sum(meanabscov[!is.na(LTD)]),
         cov_RTD = sum(meanabscov[!is.na(RTD)]),
         cov_LDMC = sum(meanabscov[!is.na(LDMC)]),
         cov_leafarea = sum(meanabscov[!is.na(leafarea)]),
         cov_rootdiam = sum(meanabscov[!is.na(rootdiam)]),
         cov_TLP = sum(meanabscov[!is.na(TLP)]),
         cov_rootN = sum(meanabscov[!is.na(rootN)]),
         cov_leafN = sum(meanabscov[!is.na(leafN)]),
         cov_totalsum = sum(meanabscov)
  ) 

#find relative cover community cover with trait X - focal spp in each grassland
awt1 <- awt1 %>% 
  group_by(grassland_type, species) %>%
  mutate(relcov_height = ((cov_height-meanabscov)/cov_totalsum) * 100,
         relcov_SLA = ((cov_SLA-meanabscov)/cov_totalsum) * 100,
         relcov_SRL = ((cov_SRL-meanabscov)/cov_totalsum) * 100,
         relcov_LTD = ((cov_LTD-meanabscov)/cov_totalsum) * 100,
         relcov_RTD = ((cov_RTD-meanabscov)/cov_totalsum) * 100,
         relcov_LDMC = ((cov_LDMC-meanabscov)/cov_totalsum) * 100,
         relcov_leafarea = ((cov_leafarea-meanabscov)/cov_totalsum) * 100,
         relcov_rootdiam = ((cov_rootdiam-meanabscov)/cov_totalsum) * 100,
         relcov_rootN = ((cov_rootN-meanabscov)/cov_totalsum) * 100,
         relcov_leafN = ((cov_leafN-meanabscov)/cov_totalsum) * 100
  )


# functcomp function does not work with intraspecific variation, so a model for each 
# grassland is made individually with a loop 
# (five are looped, then tallgrass is done alone because it had a different number of traits)
awt1.5 <- awt1 %>% filter(grassland_type!="Tallgrass") #and southern shortgrass/sev blue
gland <- unique(awt1.5$grassland_type) #create grassland list
CWM <- data.frame() #create dataframe to fill
CWMtemp <- data.frame()
CWMsite <- data.frame()
for (i in 1:length(gland)){
  trtemp <- awt1.5[,c(1,2,9:19)] %>% filter(grassland_type==gland[i]) %>% column_to_rownames(var ="species")
  covtemp <- awt1.5[,c(1,2,3)] %>% filter(grassland_type==gland[i])
  spp <- unique(rownames(trtemp))
  for (j in 1:length(spp)){
    trtemp2 <- trtemp[-j,]  
    trtemp2 <- as.matrix(trtemp2[,-1])
    covtemp2 <- covtemp[-j,] %>% pivot_wider(names_from = species, values_from = meanabscov) %>% column_to_rownames(var ="grassland_type")
    covtemp2 <- as.matrix(covtemp2)
    CWMtemp <- functcomp(trtemp2,covtemp2)
    CWMtemp$species <- spp[j]
    CWMtemp$grassland_type <- gland[i]
    CWMsite <- rbind(CWMsite, CWMtemp)
  }
}
awt1.1 <- awt1 %>% filter(grassland_type=="Tallgrass") #subset out tallgrass
gland <- unique(awt1.1$grassland_type) #create grassland list
#for Tallgrass (missing leafN)
trtemp <- awt1[,c(1,2,9:19)] %>% filter(grassland_type==gland[1]) %>% column_to_rownames(var ="species")
trtemp <- trtemp[,-12]
covtemp <- awt1[,c(1,2,3)] %>% filter(grassland_type==gland[1])
spp <- unique(rownames(trtemp))
for (j in 1:length(spp)){
  trtemp2 <- trtemp[-j,]  
  trtemp2 <- as.matrix(trtemp2[,-1])
  covtemp2 <- covtemp[-j,] %>% pivot_wider(names_from = species, values_from = meanabscov) %>% column_to_rownames(var ="grassland_type")
  covtemp2 <- as.matrix(covtemp2)
  CWMtemp <- functcomp(trtemp2,covtemp2)
  CWMtemp$species <- spp[j]
  CWMtemp$grassland_type <- gland[1]
  CWMtemp$leafN <- NA
  CWMsite <- bind_rows(CWMsite, CWMtemp)
}
#merge dataframes 
CWMs2 <- merge(CWMsite, awt1[,c(1,2,3,34:43)], by=c("grassland_type", "species")) 

#filter out CWM values for spp-site whose relative cover for that trait is less than 75%
#do this for each trait and recombine dataframes to fill NA where CWM is inappropriate for % cover
CWMs2$height <- replace(CWMs2$height, CWMs2$relcov_height < 75, NA)
CWMs2$SLA <- replace(CWMs2$SLA, CWMs2$relcov_SLA < 75, NA)
CWMs2$SRL <- replace(CWMs2$SRL, CWMs2$relcov_SRL < 75, NA)
CWMs2$LTD <- replace(CWMs2$LTD, CWMs2$relcov_LTD < 75, NA)
CWMs2$RTD <- replace(CWMs2$RTD, CWMs2$relcov_RTD < 75, NA)
CWMs2$leafarea <- replace(CWMs2$leafarea, CWMs2$relcov_leafarea < 75, NA)
CWMs2$rootdiam <- replace(CWMs2$rootdiam, CWMs2$relcov_rootdiam < 75, NA)
CWMs2$LDMC <- replace(CWMs2$LDMC, CWMs2$relcov_LDMC < 75, NA)
CWMs2$rootN <- replace(CWMs2$rootN, CWMs2$relcov_rootN < 75, NA)
CWMs2$leafN <- replace(CWMs2$leafN, CWMs2$relcov_leafN < 75, NA)
CWMs2$TLP <- replace(CWMs2$TLP, CWMs2$relcov_TLP < 75, NA)


NEWallsite <- read.csv("data/allsite_new.csv") #data
CWM_sitedata <- merge(CWMs2, NEWallsite[,c(1:16,22:31)], by=c("species", "grassland_type"))# trait.x = CWM  #NEWallsite[,c(1,4:15,24,27,28,19:31)],
awt1 <- awt1 %>% mutate(relcov_total = 100-(meanabscov/cov_totalsum * 100)) #total relative community cover pg grassland
CWM_sitedata <- merge(CWM_sitedata, awt1[c(1,2,44)], by=c("species", "grassland_type"))

#relevel to view grassland_type facets along precipitation gradient 
CWM_sitedata <- CWM_sitedata %>%
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Chihuahuan Desert", "Northern Shortgrass", #no southern shortgrass, not enough data
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))


#save dataframe
write.csv(CWM_sitedata, file='data/CWM_sitedata.csv', row.names = F) #make csv


#code to determine coverage of each trait per grassland (summarized below)
CWM_sitedata %>%
  group_by(grassland_type) %>%
  summarise(total_non_na = sum(!is.na(leafN.x)))
# all six = SLA, TLP,
# three = SRL, leafN, rootdiam, height, LDMC
# two = rootN, LTD, leafarea
# one = RTD


