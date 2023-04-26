NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
CWM_sitedata <- read.csv("data/CWM_sitedata.csv") #CWM trait data
trtdata<-CWM_sitedata[,c(1:13,48)] #just variables I will use
trtdata[,c(3:11)]<-log(trtdata[,c(3:11)]) #log CWM traits 
trtdata[,12]<-log(abs(trtdata[,12])) #abs then log for TLP.x
alldat <- merge(NEWallsite, trtdata, by = c("species", "grassland_type"))
alldat[,c(7:15,17)]<-log(alldat[,c(7:15,17)]) #log focal traits


#### Adding sev traits ####
sevtraits <- read.csv("data/sev333_plant_traits.csv")
sevtraits <- sevtraits %>% mutate(species = str_c(genus_new, species_new))
sevtraits <- sevtraits %>% select(species, site, PhotoPath, FunctionalGroup, LifeHistory, ldmc, srl, sla)
species <- NEWallsite %>% filter(grassland_type=="Southern Shortgrass"|
                                   grassland_type=="Desert")
specieslist <- species$species
subsev <- sevtraits %>% filter(species %in% specieslist) 
subsev <- subsev %>% filter(site=="core_blue"|site=="core_black")
subsev$species <- as.factor(subsev$species)
subsevldmc <- aggregate(subsev$ldmc, by=list(subsev$species, subsev$site), FUN=mean, na.rm=T)
subsevldmc <- subsevldmc %>% rename("species"="Group.1",
                                    "site"="Group.2",
                                    "ldmc"="x")
subsevsrl <- aggregate(subsev$srl, by=list(subsev$species,subsev$site), FUN=mean, na.rm=T)
subsevsrl <- subsevsrl %>% rename("species"="Group.1",
                                  "site"="Group.2",
                                    "srl"="x")
test <- subsevsrl %>% mutate(srl_m = srl*1000) #make proper units
subsevtrt <- merge(subsevldmc,subsevsrl, by=c("species","site"))
subsevsla <- aggregate(subsev$sla, by=list(subsev$species,subsev$site), FUN=mean, na.rm=T)
subsevsla <- subsevsla %>% rename("species"="Group.1",
                                  "site"="Group.2",
                                  "sla"="x")
#### adding tallgrass traits ####
species <- read.csv("data/sppnames.csv")
talltraits <- read.csv("data/tallgrass_PlantTraits2018.csv")
talltraits <- merge(talltraits, species[,c(1,3)], by.x="Full.sp.", by.y="species_under") #join

#how many
species <- NEWallsite %>% filter(grassland_type=="Tallgrass")
specieslist <- species$species
subtall <- talltraits %>% filter(namestogether %in% specieslist) 
subsev <- subsev %>% filter(site=="core_blue"|site=="core_black")
subsev$species <- as.factor(subsev$species)
unique(talltraits$namestogether)





#### looking at models more in depth ####
#GR w/ N in ambient predicted by GR w/o N in drought 
#how responses to abiotic and biotic relate? 
#does differ by grassland
summary(lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type, NEWallsite))
anova(lm(invasionLDGRcon~intrinsicLDGRchr*grassland_type, NEWallsite))
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

#GR w/ N in ambient predicted by GR w/o N in ambient
#is your GR depressed by neighbors?
#does differ by grassland
summary(lm(invasionLDGRcon~intrinsicLDGRcon*grassland_type, NEWallsite))
anova(lm(invasionLDGRcon~intrinsicLDGRcon*grassland_type, NEWallsite))
ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRcon, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

#GR w/ N in drought predicted by GR w/o N in drought
#effect of biotic stress during abiotic stress? 
#does NOT differ by grassland
summary(lm(invasionLDGRchr~intrinsicLDGRchr*grassland_type, NEWallsite))
anova(lm(invasionLDGRchr~intrinsicLDGRchr*grassland_type, NEWallsite))
ggplot(NEWallsite, aes(y=invasionLDGRchr, x=intrinsicLDGRchr, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

#GR w/ N in drought predicted by GR w/ N in ambient
#effect of abiotic stress during biotic stress? 
#slightly differs by grassland
summary(lm(invasionLDGRchr~invasionLDGRcon*grassland_type, NEWallsite))
anova(lm(invasionLDGRchr~invasionLDGRcon*grassland_type, NEWallsite))
ggplot(NEWallsite, aes(y=invasionLDGRchr, x=invasionLDGRcon, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

#GR w/o N in drought predicted by GR w/o N in ambient
#is your GR depressed by drought?  
#does NOT differ by grassland
summary(lm(intrinsicLDGRchr~intrinsicLDGRcon*grassland_type, NEWallsite))
anova(lm(intrinsicLDGRchr~intrinsicLDGRcon*grassland_type, NEWallsite))
ggplot(NEWallsite, aes(y=intrinsicLDGRchr, x=intrinsicLDGRcon, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
