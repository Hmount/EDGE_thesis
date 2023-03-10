NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
CWM_sitedata <- read.csv("data/CWM_sitedata.csv") #trait data
trtdata<-CWM_sitedata[,c(1:13,48)] #just variables I will use
trtdata[,c(3:11)]<-log(trtdata[,c(3:11)]) #log CWM traits 
trtdata[,12]<-log(abs(trtdata[,12])) #abs then log for TLP.x
alldat <- merge(NEWallsite, trtdata, by = c("species", "grassland_type"))
alldat[,c(7:15,17)]<-log(alldat[,c(7:15,17)]) #log focal traits

# traits and LDGR's
summary(lm(intrinsicLDGRcon ~ leafarea*site + SLA.x + TLP.x, data=alldat))
summary(lm(intrinsicLDGRchr ~ leafarea*site + SLA.x + TLP.x, data=alldat))
summary(lm(invasionLDGRcon ~ leafarea*site + SLA.x + TLP.x, data=alldat)) #**
summary(lm(invasionLDGRchr ~ leafarea*site + SLA.x + TLP.x, data=alldat)) #**

summary(lm(intrinsicLDGRcon ~ SLA*grassland_type + SLA.x + TLP.x, data=alldat))
summary(lm(intrinsicLDGRchr ~ SLA*grassland_type + SLA.x + TLP.x, data=alldat)) #*
summary(lm(invasionLDGRcon ~ SLA*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRchr ~ SLA*grassland_type + SLA.x + TLP.x, data=alldat)) #***

summary(lm(intrinsicLDGRcon ~ leafN*grassland_type + SLA.x + TLP.x, data=alldat))
summary(lm(intrinsicLDGRchr ~ leafN*grassland_type + SLA.x + TLP.x, data=alldat)) #**
summary(lm(invasionLDGRcon ~ leafN*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRchr ~ leafN*grassland_type + SLA.x + TLP.x, data=alldat)) #**

summary(lm(intrinsicLDGRcon ~ SRL*grassland_type + SLA.x + TLP.x, data=alldat))
summary(lm(intrinsicLDGRchr ~ SRL*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRcon ~ SRL*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRchr ~ SRL*grassland_type + SLA.x + TLP.x, data=alldat)) #*

summary(lm(intrinsicLDGRcon ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #**
summary(lm(intrinsicLDGRchr ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #*
summary(lm(invasionLDGRcon ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #**
summary(lm(invasionLDGRchr ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #***

summary(lm(intrinsicLDGRcon ~ LTD*grassland_type + SLA.x + TLP.x, data=alldat))
summary(lm(intrinsicLDGRchr ~ LTD*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRcon ~ LTD*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRchr ~ LTD*grassland_type + SLA.x + TLP.x, data=alldat)) #**

summary(lm(intrinsicLDGRcon ~ rootN*grassland_type + SLA.x + TLP.x, data=alldat))
summary(lm(intrinsicLDGRchr ~ rootN*grassland_type + SLA.x + TLP.x, data=alldat)) #*
summary(lm(invasionLDGRcon ~ rootN*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRchr ~ rootN*grassland_type + SLA.x + TLP.x, data=alldat)) #**

summary(lm(intrinsicLDGRcon ~ rootdiam*grassland_type + SLA.x + TLP.x, data=alldat))
summary(lm(intrinsicLDGRchr ~ rootdiam*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRcon ~ rootdiam*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRchr ~ rootdiam*grassland_type + SLA.x + TLP.x, data=alldat)) #**

summary(lm(intrinsicLDGRcon ~ RTD*grassland_type + SLA.x + TLP.x, data=alldat))
summary(lm(intrinsicLDGRchr ~ RTD*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRcon ~ RTD*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRchr ~ RTD*grassland_type + SLA.x + TLP.x, data=alldat))

summary(lm(intrinsicLDGRcon ~ height*grassland_type + SLA.x + TLP.x, data=alldat))
summary(lm(intrinsicLDGRchr ~ height*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRcon ~ height*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRchr ~ height*grassland_type + SLA.x + TLP.x, data=alldat)) #**

summary(lm(intrinsicLDGRcon ~ TLP_tran.y*grassland_type + SLA.x + TLP.x, data=alldat))
summary(lm(intrinsicLDGRchr ~ TLP_tran.y*grassland_type + SLA.x + TLP.x, data=alldat)) #*
summary(lm(invasionLDGRcon ~ TLP_tran.y*grassland_type + SLA.x + TLP.x, data=alldat)) 
summary(lm(invasionLDGRchr ~ TLP_tran.y*grassland_type + SLA.x + TLP.x, data=alldat)) #*


##now examine ANOVA tables 
anova(lm(invasionLDGRcon ~ leafarea*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, SLA, TLP
anova(lm(invasionLDGRchr ~ leafarea*grassland_type + SLA.x + TLP.x, data=alldat)) #grassland, tlp, interaction

anova(lm(intrinsicLDGRchr ~ SLA*grassland_type + SLA.x + TLP.x, data=alldat)) #TLP
anova(lm(invasionLDGRchr ~ SLA*grassland_type + SLA.x + TLP.x, data=alldat)) #grassland, SLA, TLP

anova(lm(intrinsicLDGRchr ~ leafN*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, TLP
anova(lm(invasionLDGRchr ~ leafN*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, grassland, TLP

anova(lm(invasionLDGRchr ~ SRL*grassland_type + SLA.x + TLP.x, data=alldat)) #TLP

anova(lm(intrinsicLDGRcon ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, interaction
anova(lm(intrinsicLDGRchr ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, interaction
anova(lm(invasionLDGRcon ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, interaction
anova(lm(invasionLDGRchr ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, SLA, TLP, interaction

anova(lm(invasionLDGRchr ~ LTD*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, grassland, TLP

anova(lm(intrinsicLDGRchr ~ rootN*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, TLP
anova(lm(invasionLDGRchr ~ rootN*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, TLP

anova(lm(invasionLDGRchr ~ rootdiam*grassland_type + SLA.x + TLP.x, data=alldat)) #TLP, interaction
 
anova(lm(invasionLDGRchr ~ height*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, interaction

anova(lm(intrinsicLDGRchr ~ TLP_tran.y*grassland_type + SLA.x + TLP.x, data=alldat)) #TLP, interaction
anova(lm(invasionLDGRchr ~ TLP_tran.y*grassland_type + SLA.x + TLP.x, data=alldat)) #TLP, interaction
        

##remaining deceant models
anova(lm(invasionLDGRcon ~ leafarea*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, SLA, TLP
anova(lm(invasionLDGRchr ~ leafarea*grassland_type + SLA.x + TLP.x, data=alldat)) #grassland, tlp, interaction

anova(lm(invasionLDGRchr ~ SLA*grassland_type + SLA.x + TLP.x, data=alldat)) #grassland, SLA, TLP

anova(lm(intrinsicLDGRchr ~ leafN*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, TLP
anova(lm(invasionLDGRchr ~ leafN*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, grassland, TLP

anova(lm(intrinsicLDGRcon ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, interaction
ggplot(alldat, aes(y=intrinsicLDGRcon, x=LDMC, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
anova(lm(intrinsicLDGRchr ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, interaction
ggplot(alldat, aes(y=intrinsicLDGRchr, x=LDMC, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
anova(lm(invasionLDGRcon ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, interaction
ggplot(alldat, aes(y=invasionLDGRcon, x=LDMC, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")
anova(lm(invasionLDGRchr ~ LDMC*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, SLA, TLP, interaction
ggplot(alldat, aes(y=invasionLDGRchr, x=LDMC, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

anova(lm(invasionLDGRchr ~ LTD*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, grassland, TLP

anova(lm(intrinsicLDGRchr ~ rootN*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, TLP
anova(lm(invasionLDGRchr ~ rootN*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, TLP

anova(lm(invasionLDGRchr ~ rootdiam*grassland_type + SLA.x + TLP.x, data=alldat)) #TLP, interaction

anova(lm(invasionLDGRchr ~ height*grassland_type + SLA.x + TLP.x, data=alldat)) #focal, interaction
ggplot(alldat, aes(y=invasionLDGRchr, x=SLA, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")

anova(lm(intrinsicLDGRchr ~ TLP_tran.y*grassland_type + SLA.x + TLP.x, data=alldat)) #TLP, interaction
anova(lm(invasionLDGRchr ~ TLP_tran.y*grassland_type + SLA.x + TLP.x, data=alldat)) #TLP, interaction


#what about just traits
summary(lm(intrinsicLDGRcon ~ leafN, data=alldat))
summary(lm(intrinsicLDGRchr ~ leafN, data=alldat)) #**
ggplot(alldat, aes(y=intrinsicLDGRchr, x=leafN))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRcon ~ leafN, data=alldat)) #*
summary(lm(invasionLDGRchr ~ leafN, data=alldat)) #*

summary(lm(intrinsicLDGRcon ~ SRL, data=alldat))
summary(lm(intrinsicLDGRchr ~ SRL, data=alldat))
summary(lm(invasionLDGRcon ~ SRL, data=alldat)) 
summary(lm(invasionLDGRchr ~ SRL, data=alldat)) #*

summary(lm(intrinsicLDGRcon ~ LDMC, data=alldat)) #***
ggplot(alldat, aes(y=intrinsicLDGRcon, x=LDMC))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ LDMC, data=alldat)) #***
ggplot(alldat, aes(y=intrinsicLDGRchr, x=LDMC))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRcon ~ LDMC, data=alldat)) #***
ggplot(alldat, aes(y=invasionLDGRcon, x=LDMC))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRchr ~ LDMC, data=alldat)) #***
ggplot(alldat, aes(y=invasionLDGRchr, x=LDMC))+
  geom_point()+
  geom_smooth(method="lm")

summary(lm(intrinsicLDGRcon ~ LTD, data=alldat)) #**
ggplot(alldat, aes(y=intrinsicLDGRcon, x=LTD))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ LTD, data=alldat)) #*
summary(lm(invasionLDGRcon ~ LTD, data=alldat)) #**
ggplot(alldat, aes(y=invasionLDGRcon, x=LTD))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(invasionLDGRchr ~ LTD, data=alldat)) #**
ggplot(alldat, aes(y=invasionLDGRchr, x=LTD))+
  geom_point()+
  geom_smooth(method="lm")

summary(lm(intrinsicLDGRcon ~ rootN, data=alldat)) #**
ggplot(alldat, aes(y=intrinsicLDGRcon, x=rootN))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(intrinsicLDGRchr ~ rootN, data=alldat)) #*
summary(lm(invasionLDGRcon ~ rootN, data=alldat)) #*
summary(lm(invasionLDGRchr ~ rootN, data=alldat)) 

summary(lm(intrinsicLDGRcon ~ rootdiam, data=alldat))
summary(lm(intrinsicLDGRchr ~ rootdiam, data=alldat)) 
summary(lm(invasionLDGRcon ~ rootdiam, data=alldat)) 
summary(lm(invasionLDGRchr ~ rootdiam, data=alldat)) #*






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
