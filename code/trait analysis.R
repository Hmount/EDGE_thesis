#### traits analysis ####

#### data and packages + clean ####
library(tidyverse)

CWM_sitedata <- read.csv("data/CWM_sitedata.csv") #trait data

trtdata<-CWM_sitedata[,c(1:13,27:37,39,41:48)] #just variables I will use
trtdata[,c(3:11,13,16:25,33)]<-log(trtdata[,c(3:11,13,16:25,33)]) #log focal and CWM traits 
trtdata[,12]<-log(abs(trtdata[,12])) #abs then log for TLP.x


#### basic stats and relationships ####
# how many populations have data?
sum(complete.cases(NEWallsite[,c(6:16)])) #21 species have all rows
trtcnt <- rowSums(!is.na(NEWallsite[,c(6:16)])) #count for each row w/ traits filled in
sum(trtcnt == 0) #20 have no trait data 
sum(trtcnt > 5) #58 have more than 5 traits
(sum(trtcnt > 5)/sum(trtcnt >= 0))*100 #50% of data has more than 5 traits

# do CWMs differ between grasslands? by both traits? (yes and yes)
summary(aov(SLA.x ~ grassland_type, trtdata))
summary(aov(TLP.x ~ grassland_type, trtdata))
summary(mod<-lm(SLA.x~TLP.x*grassland_type, trtdata))
anova(mod)
#view:
ggplot(trtdata, aes(x=TLP.x, y=SLA.x, color=grassland_type))+ 
  geom_point()

# does intrinsicdiff change with CWM SLA alone or by grassland? (no)
ggplot(trtdata, aes(x=SLA.x, y=intrinsicdiff))+ 
  geom_point()
summary(lm(intrinsicdiff~SLA.x, trtdata))


#### modelling ####
# model responses as a function of focal traits* grassland + community CWM SLA and TLP
# considered including + total relative cover, but I am looking at pop. level diffs not 
# specific times/cover so it would either be redundant with grassland or difference in
# cover between conditions?

# Response to drought by each trait
summary(mod_la<-lm(intrinsicdiff~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #* 22%
anova(mod_la)
summary(modt<-lm(intrinsicdiff~leafN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #na
anova(modt)
summary(modt<-lm(intrinsicdiff~LDMC.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(intrinsicdiff~LTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(intrinsicdiff~TLP_tran*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(intrinsicdiff~SLA.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #na
anova(modt)
summary(modt<-lm(intrinsicdiff~height.y*grassland_type + SLA.x + TLP.x,data=trtdata)) #no
anova(modt)
summary(modt<-lm(intrinsicdiff~RTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(intrinsicdiff~rootN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #na
anova(modt)
summary(mod_srl<-lm(intrinsicdiff~SRL.y*grassland_type + SLA.x + TLP.x , data=trtdata)) #*close 20%
anova(mod_srl)
summary(mod_rd<-lm(intrinsicdiff~rootdiam.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #* 24%
anova(mod_rd)


#now response to neighbors in drought
summary(modt<-lm(chrdiff~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #* 22%
anova(modt) #grassland+SLA
summary(mod_ln<-lm(chrdiff~leafN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #** 36%
anova(mod_ln) #SLA.x + grassland*leafN
summary(modt<-lm(chrdiff~SLA.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #** 22%
anova(modt)#grassland+SLA.x
summary(modt<-lm(chrdiff~LTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #nah?
anova(modt)
summary(modt<-lm(chrdiff~TLP_tran*grassland_type + SLA.x + TLP.x, data=trtdata)) #*** 35% 
anova(modt)#grassland+SLA.x
summary(mod_sla<-lm(chrdiff~LDMC.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #close
anova(mod_sla)#grassland+SLA.x
summary(modt<-lm(chrdiff~height.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #close 13%
anova(modt)
summary(mod_rtd<-lm(chrdiff~RTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(mod_rtd) #SLA.x
summary(modt<-lm(chrdiff~rootN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(chrdiff~SRL.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #close, 19%
anova(modt)
summary(modt<-lm(chrdiff~rootdiam.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)


#now response to neighbors in ambient
summary(modt<-lm(condiff~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(mod_ln<-lm(condiff~leafN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #* 26%
anova(mod_ln) #SLA.x + grassland"leafN
summary(modt<-lm(condiff~LDMC.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(condiff~LTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(condiff~TLP_tran*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(mod_sla<-lm(condiff~SLA.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(mod_sla)
summary(modt<-lm(condiff~height.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(mod_rtd<-lm(condiff~RTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
summary(modt<-lm(condiff~rootN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(condiff~SRL.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #na
anova(modt)
summary(modt<-lm(condiff~rootdiam.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #nah
anova(modt)


summary(modt<-lm(invasiondiff~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(mod_ln<-lm(invasiondiff~leafN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #* 26%
anova(mod_ln) #SLA.x + grassland"leafN
summary(modt<-lm(invasiondiff~LDMC.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(invasiondiff~LTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(invasiondiff~TLP_tran*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(mod_sla<-lm(invasiondiff~SLA.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(mod_sla)
summary(modt<-lm(invasiondiff~height.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(mod_rtd<-lm(invasiondiff~RTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
summary(modt<-lm(invasiondiff~rootN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt)
summary(modt<-lm(invasiondiff~SRL.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #na
anova(modt)
summary(modt<-lm(invasiondiff~rootdiam.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #nah
anova(modt)

#### figures ####
# response = response to drought
# leaf area
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



#######################################################################################
### find difference between CWM and focal traits
test <- trtdata %>% mutate(traitdiff = TLP.x-TLP_tran)
#plot
ggplot(test, aes(y=chrdiff, x=traitdiff))+#, color=grassland_type))+ 
  geom_point()+
  geom_smooth(method="lm")
### pop responses as a function of trait difference
summary(lm(chrdiff~traitdiff, test))



trait PCA
```{r}
library(factoextra)

subdfPCA <- trtdata %>% select(species, height.y,SLA.y, LTD.y, LDMC.y, leafarea.y)
subdfPCA$trtcnt <- rowSums(!is.na(subdfPCA[,c(2:6)])) #count for each row w/ traits filled in
#subset data for just species with all traits present
subdfPCA <- subdfPCA %>% filter(trtcnt >= 5) #filter for 58 spp with more than 5 traits
subdfPCA <- na.omit(subdfPCA)
subdfPCA <- subdfPCA %>% select(species, height.y,SLA.y, LTD.y, LDMC.y, leafarea.y)

colSums(!is.na(subdfPCA[,c(2:11)]))  


#subdfPCA$species_site <- paste(subdfPCA$species, subdfPCA$grassland_type)
unique(subdfPCA$species)
#library(stringr)
subdfPCA$species = str_replace(subdfPCA$species,"Pascopyrumsmithii","Pascopyrum smithii")
subdfPCA$species = str_replace(subdfPCA$species,"Boutelouagracilis","Bouteloua gracilis")
subdfPCA$species = str_replace(subdfPCA$species,"Koeleriapyramidata","Koeleria pyramidata")
subdfPCA$species = str_replace(subdfPCA$species,"Lepidiumdensiflorum","Lepidium densiflorum")
subdfPCA$species = str_replace(subdfPCA$species,"Phloxhoodii","Phlox hoodii")
subdfPCA$species = str_replace(subdfPCA$species,"Sphaeralceacoccinea","Phlox hoodii")
subdfPCA$species = str_replace(subdfPCA$species,"Stipacomata","Phlox hoodii")
subdfPCA$species = str_replace(subdfPCA$species,"Tragopogondubius","Phlox hoodii")
subdfPCA$species = str_replace(subdfPCA$species,"Vulpiaoctoflora","Vulpia octoflora")
subdfPCA$species = str_replace(subdfPCA$species,"Gutierreziasarothrae","Gutierrezia sarothrae")
subdfPCA$species = str_replace(subdfPCA$species,"Liatrispunctata","Liatris punctata")


subdfPCA <- subdfPCA %>% select(species, height.y, rootdiam.y, SRL.y, RTD.y, 
                                rootN.y, SLA.y, LTD.y, LDMC.y, leafarea.y, leafN.y, TLP_tran)

subdfPCA <- na.omit(subdfPCA)


#PCA
my.PCA <- prcomp(subdfPCA[,-1], scale = TRUE)

#screeplot
fviz_eig(my.PCA)

#just species 
fviz_pca_ind(my.PCA)

groups <- as.factor(subdfPCA$lifespan[1:20])
fviz_pca_ind(my.PCA,
             #col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)
fviz_pca_ind(my.PCA, label="none", habillage=na.omit(subdfPCA$species))


#just traits
fviz_pca_var(my.PCA)

#biplot
fviz_pca_biplot(my.PCA, repel=T)
pcaplot #
  fviz_pca_biplot(my.PCA, repel=T,habillage=subdfPCA$species, legend.title ="Species")+scale_shape_manual(values=c(19,19,19,19,19,19,19,19,19,19,19))

#export
ggsave(pcaplot, filename = "pcaplot.png", dpi=300, height = 4,width =6)

write.csv(my.PCA$rotation[,c(1,2)], "PCAloading.csv")
#save csv to copy from


```
allsub <- all %>% filter(species %in% )
library(FD)
fdisp(as.matrix(subdfPCA), )


ggplot(trtdata, aes(x=leafN.x, y=grassland_type))+
  geom_boxplot()
summary(aov(trtdata$leafN.x~trtdata$grassland_type))

ggplot(trtdata, aes(x=SLA.y, y=grassland_type))+
  geom_boxplot()
summary(aov(trtdata$SLA.y~trtdata$grassland_type))
