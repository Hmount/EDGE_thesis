#### traits analysis ####



#### basic stats ####
#how many populations have data?
sum(complete.cases(NEWallsite[,c(6:16)])) #21 species have all rows
trtcnt <- rowSums(!is.na(NEWallsite[,c(6:16)])) #count for each row w/ traits filled in
sum(trtcnt == 0) #20 have no trait data 
sum(trtcnt > 5) #58 have more than 5 traits
(sum(trtcnt > 5)/sum(trtcnt >= 0))*100 #50% of data has more than 5 traits


#### modelling ####
#clean:
CWM_sitedata<-CWM_sitedata[,c(1:13,26,28:43,45:48)] #just variables I will use
CWM_sitedata[,c(3:11,13,15:25)]<-log(CWM_sitedata[,c(3:11,13,15:25)]) #log focal and CWM traits 
CWM_sitedata[,12]<-log(abs(CWM_sitedata[,12])) #diff abs then log for TLP

##model responses as a function of focal traits* grassland + community CWM for SLA and TLP + total relative cover by grassland growth rate
#exploratory plot


#some relationships:
#CWM are clearly different at the different grasslands 
ggplot(CWM_sitedata, aes(x=TLP.x, y=SLA.x, color=grassland_type))+ 
  geom_point()
summary(mod<-lm(SLA.x~TLP.x*grassland_type, CWM_sitedata))
anova(mod)
summary(aov(SLA.x ~ grassland_type, CWM_sitedata))

#intrinsic diff is not related to CWM SLA alone or by grassland
ggplot(CWM_sitedata, aes(x=SLA.x, y=intrinsicdiff))+ 
  geom_point()
summary(lm(intrinsicdiff~SLA.x, CWM_sitedata))

#mean of other per species per each species-site
meancov_local <- allothermeans %>% group_by(grassland_type, species) %>% summarize(meancov_local = mean(meanother))
#find relative coverage of site with trait coverage
allnewsum <- allnew %>% group_by(grassland_type, species) %>% #group and order variables 
  summarise(abscov = sum(cover))
CWM_sitedata1 <- merge(CWM_sitedata, allnewsum, by=c("species","grassland_type"))


###unneeded?
#now response to drought by trait
summary(mod_la<-lm(intrinsicdiff~leafarea.y*grassland_type + SLA.x + TLP.x + abscov, data=CWM_sitedata1)) #** 36.5%
summary(mod_la<-lm(intrinsicdiff~leafarea.y*grassland_type + TLP.x + meancov_local, data=CWM_sitedata1)) #** 36.5%
anova(mod_la)
summary(modt<-lm(intrinsicdiff~leafN.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(intrinsicdiff~LDMC.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(intrinsicdiff~LTD.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #nah
anova(modt)
summary(modt<-lm(intrinsicdiff~TLP_tran*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #nah
anova(modt)
summary(modt<-lm(intrinsicdiff~SLA.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(intrinsicdiff~height.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(intrinsicdiff~RTD.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(intrinsicdiff~rootN.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(mod_srl<-lm(intrinsicdiff~SRL.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #** 30%
anova(mod_srl)
summary(mod_rd<-lm(intrinsicdiff~rootdiam.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #** 30%
anova(modt)#TLP.x and rootdiam:grassland (sig. as expected!)
summary(mod_rd<-lm(intrinsicdiff~rootdiam.y*grassland_type  + TLP.x + meancov_local, data=CWM_sitedata1))

#now response to neighbors in drought
summary(modt<-lm(chrdiff~leafarea.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(mod_ln<-lm(chrdiff~leafN.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #** 32%
anova(mod_ln) #SLA.x + grassland"leafN
summary(modt<-lm(chrdiff~LDMC.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(chrdiff~LTD.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #nah
anova(modt)
summary(modt<-lm(chrdiff~TLP_tran*grassland_type + SLA.x + TLP.x, data=CWM_sitedata1)) #nah?
anova(modt)
summary(mod_sla<-lm(chrdiff~SLA.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #** 15% (so nah)
anova(mod_sla)#grassland+SLA.x
summary(modt<-lm(chrdiff~height.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(mod_rtd<-lm(chrdiff~RTD.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #** 40% (no now?)
anova(mod_rtd) #SLA.x
summary(modt<-lm(chrdiff~rootN.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(chrdiff~SRL.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(chrdiff~rootdiam.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #nah
anova(modt)



#now response to neighbors in ambient
summary(modt<-lm(condiff~leafarea.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(mod_ln<-lm(condiff~leafN.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #* 26%
anova(mod_ln) #SLA.x + grassland"leafN
summary(modt<-lm(condiff~LDMC.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(condiff~LTD.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #nah
anova(modt)
summary(modt<-lm(condiff~TLP_tran*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #nah?
anova(modt)
summary(mod_sla<-lm(condiff~SLA.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #** nah
summary(modt<-lm(condiff~height.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(mod_rtd<-lm(condiff~RTD.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #nah
summary(modt<-lm(condiff~rootN.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(condiff~SRL.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #na
anova(modt)
summary(modt<-lm(condiff~rootdiam.y*grassland_type + SLA.x + TLP.x + meancov_local, data=CWM_sitedata1)) #nah
anova(modt)



#response = response to drought
#leaf area
modD_la<-lm(intrinsicdiff~leafarea.y*grassland_type + SLA.x + TLP.x + abscov, data=CWM_sitedata1) #full model
summary(modD_la) #** 36.5%
anova(modD_la) #* trait, grassland, interaction, TLP
#modD_la<-lm(intrinsicdiff~leafarea.y*grassland_type + SLA.x, data=CWM_sitedata1)
modD_la<-lm(intrinsicdiff~leafarea.y*grassland_type + SLA.x, data=noT) #nah
summary(modD_la) 
anova(modD_la)
#better without meancov_local ~*best
#when SLA is removed, all predictors become NS. 
#If just TLP removed similar to full model, but SLA.x remains NS in anova 
#when TLP is also then removed, the whole model becomes NS 
noT<-CWM_sitedata1%>%filter(grassland_type!="Tallgrass") #remove tallgrass with not enough
lafig <- ggplot(noT, aes(x = leafarea.y, y = intrinsicdiff, color=grassland_type)) +
  geom_point() + 
  #scale_x_log10() +
  geom_smooth(data=noT, method = "lm", se = T)+
  facet_wrap(~grassland_type, scales = "fixed", nrow=1)+
  scale_color_manual(values=c("red", "pink", "tan", "sky blue", "dark blue"))+
  labs(y="(observed)", x = "log(leaf area)", color="Grassland")+
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1))
lacwmfig <- visreg::visreg(modD_la, xvar="SLA.x",
                           cond=list(grassland_type="Southern Mixed"), 
                           xlab= "log(community weighted mean SLA)", ylab="(predicted)", 
                           gg=T, line=list(col="white"), band=F, top="points")+
  geom_point(cex=1.15, col="dark grey")+
  theme_classic()


#response = response to neighbors drought
#leaf N
modND_ln<-lm(chrdiff~leafN.y*grassland_type + SLA.x + TLP.x + abscov, data=CWM_sitedata1) #full model
summary(modND_ln) #** 35%
anova(modND_ln) #* grassland, interaction, SLA
modND_ln<-lm(chrdiff~leafN.y*grassland_type + SLA.x, data=CWM_sitedata1)
summary(modND_ln) 
anova(modND_ln)
#worse without meancov_local
#when TLP is removed, still good model ~*best
#solid model(?)
lnfig <- ggplot(noT, aes(x = leafN.y, y = chrdiff, color=grassland_type)) +
  geom_point() + 
  #scale_x_log10() +
  geom_smooth(data=noT, method = "lm", se = T)+
  facet_wrap(~grassland_type, scales = "fixed", nrow=1)+
  scale_color_manual(values=c("red", "pink", "tan", "sky blue", "dark blue"))+
  labs(y="(observed)", x = "log(leaf nitrogen)")+
  theme_classic()+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1))
lncwmfig <- visreg::visreg(modND_ln, xvar="SLA.x", cond=list(grassland_type="Southern Mixed"),
                           xlab= "log(community weighted mean SLA)", ylab="(predicted)", gg=T,
                           line=list(col="black"), band=T)+
  geom_point(cex=1.15, col="dark grey")+
  #, by="grassland_type")+  #using this line I can see all interactions are the same
  theme_classic()


library(ggpubr)
##for drought significant traits
latraitfig <- ggarrange(lafig, lacwmfig, nrow=1, ncol=2, common.legend = T,
                        widths = c(1.75,1.25))
latraitfig <- annotate_figure(latraitfig,left = "Response to drought")
latraitfig

##for competition significant traits
lntraitfig <- ggarrange(lnfig, lncwmfig, nrow=1, ncol=2, common.legend = F,
                        widths = c(1.75,1.25))
lntraitfig<-annotate_figure(lntraitfig,left = "Response to nieghbors in drought")
lntraitfig

#combo plot
traitfig <- ggarrange(latraitfig, lntraitfig, nrow=2, ncol=1, common.legend = T, 
                      heights = c(1.25,1))
traitfig

#export
ggsave(traitfig, filename = "traitfig.png", dpi=300, height = 5.8,width =8.5)

plot(CWM_sitedata1$leafarea.y,CWM_sitedata1$leafN.y)

##NEW FIGURES:
library(visreg)

#la
modD_la<-lm(intrinsicdiff~leafarea.y*grassland_type + SLA.x, data=noT)
summary(modD_la) 
anova(modD_la)
lafig <- visreg(modD_la, xvar="leafarea.y", by="grassland_type", rug=F, 
                partial=F,xlab= "log(leaf area)", ylab=" ", 
                line=list(col="black"), gg=T)+
  geom_rug(sides="b")+
  theme(axis.title.y.left=element_blank())+
  #ylim(-2,2) +
  coord_cartesian(ylim = c(-3, 4))+
  theme_classic()
lacwmfig <- visreg::visreg(modD_la, xvar="SLA.x", rug=F, partial=F,
                           cond=list(grassland_type="Southern Mixed"),
                           xlab= "log(community weighted mean SLA)", 
                           ylab=" ", gg=T, line=list(col="black"), band=T)+
  geom_rug(sides="b")+
  coord_cartesian(ylim = c(-3, 3))+
  theme_classic()
##for drought significant traits
latraitfig <- ggarrange(lafig, lacwmfig, nrow=1, ncol=2, common.legend = T,
                        widths = c(2,1.5))
latraitfig <- annotate_figure(latraitfig,left = "Response to drought")
latraitfig

#ln
modND_ln<-lm(chrdiff~leafN.y*grassland_type + SLA.x, data=CWM_sitedata1)
summary(modND_ln) 
anova(modND_ln)
lnfig <- visreg(modND_ln, xvar="leafN.y", by="grassland_type", rug=F, 
                partial=F,xlab= "log(leaf nitrogen)", ylab=" ", 
                line=list(col="black"), gg=T)+
  geom_rug(sides="b")+
  theme(axis.title.y.left=element_blank())+
  coord_cartesian(ylim = c(-3, 4))+
  theme_classic()
lncwmfig <- visreg::visreg(modND_ln, xvar="SLA.x", rug=F, partial=F,
                           cond=list(grassland_type="Southern Mixed"),
                           xlab= "log(community weighted mean SLA)", 
                           ylab=" ", gg=T, line=list(col="black"), band=T)+
  geom_rug(sides="b")+
  coord_cartesian(ylim = c(-3, 3))+
  theme_classic()
##for competition significant traits
lntraitfig <- ggarrange(lnfig, lncwmfig, nrow=1, ncol=2,
                        widths = c(2,1.5))
lntraitfig<-annotate_figure(lntraitfig,left = "Response to nieghbors in drought")
lntraitfig

#combo plot
traitfig <- ggarrange(latraitfig, lntraitfig, nrow=2, ncol=1, common.legend = T)
traitfig

#export
ggsave(traitfig, filename = "traitfig.png", dpi=300, height = 5.8,width =8.5)