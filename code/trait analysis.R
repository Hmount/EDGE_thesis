#### traits analysis ####

#### data and packages + clean ####
library(tidyverse)

CWM_sitedata <- read.csv("data/CWM_sitedata.csv") #trait data

trtdata<-CWM_sitedata[,c(1:13,27:37,39,41:48)] #just variables I will use
trtdata[,c(3:11,13,16:25,33)]<-log(trtdata[,c(3:11,13,16:25,33)]) #log focal and CWM traits 
trtdata[,12]<-log(abs(trtdata[,12])) #abs then log for TLP.x
trtdata <- trtdata %>% mutate(TLP_tran_fig = TLP_tran*-1,
                              TLP.x_fig = TLP.x*-1)  #make neg. to interpret


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
# use multivariate linear model to see if responses are a function of focal traits* grassland + community CWM SLA and TLP
# run MOANOVA with significant model objects to test for significance of predictors to response interaction

#models:
summary(mod_la<-lm(cbind(intrinsicdiff, chrdiff)~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_la) #trait, trait*grassland, CWM SLA, CWM TLP
summary(mod_ln<-lm(cbind(intrinsicdiff, chrdiff)~leafN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_ln) #trait*grassland
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~LDMC.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) #nothin
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~LTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) # not working???
summary(mod_tlp<-lm(cbind(intrinsicdiff, chrdiff)~TLP_tran*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_tlp) #trait, CWM SLA
summary(mod_sla<-lm(cbind(intrinsicdiff, chrdiff)~SLA.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_sla)
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~height.y*grassland_type + SLA.x + TLP.x,data=trtdata)) #no
car::Anova(modt)
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~RTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) #not working???
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~rootN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) #no
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~SRL.y*grassland_type + SLA.x + TLP.x , data=trtdata)) #close
car::Anova(modt) #trait, grassland, SLA
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~rootdiam.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) #nah

library("corrplot")
test <- trtdata %>% dplyr::select(c(leafN.y,SLA.x,SLA.y,TLP.x_fig,TLP_tran_fig,leafarea.y))
M<-cor(test, use = "pairwise.complete.obs")
head(round(M,2))

# visualizing correlogram
# as circle
corrplot(M, method="circle")




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
