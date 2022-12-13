#### additional analyses ####
# These anlayses were done, but did not make it into the final paper:
# do pop.responses differ by basic life history traits? (no)

#### do pop.responses differ by basic life history traits? ####
# response to drought may differ slightly by lifespan, but non-sig.
# response to neighbors in drought does not differ by any life history (interesting)
# response to nieghbors in ambient does differ on the grasslands by lifespan, life form, and photo-type 

#response to drought
anova(lm(intrinsicdiff~lifespan*grassland_type,data=NEWallsite))#slightly sig in anova, not in lm
anova(lm(intrinsicdiff~grass.forb*grassland_type,data=NEWallsite))#nah
anova(lm(intrinsicdiff~Photosynthesis*grassland_type,data=NEWallsite[-c(125,136),]))#nah

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


#### test if difference in traits inproved fitness ####
#find difference between CWM and focal traits
test <- trtdata %>% mutate(traitdiff = SLA.x-SLA.y)
#plot
ggplot(test, aes(y=chrdiff, x=traitdiff))+#, color=grassland_type))+ 
  geom_point()+
  geom_smooth(method="lm")
### pop responses as a function of trait difference
summary(lm(chrdiff~traitdiff, test))



#### Figure S4; ####
# Principle Component Analysis (PCA) of relationship between all traits. This is done for just 20 
# populations across the different grasslands that had data for all traits. 
#create dataframe
CWM_sitedata <- read.csv("data/CWM_sitedata.csv") #trait data
trtdata<-CWM_sitedata[,c(1:13,27:37,39,41:48)] #just variables I will use
trtdata[,c(3:11,13,16:25,33)]<-log(trtdata[,c(3:11,13,16:25,33)]) #log focal and CWM traits 
trtdata[,12]<-log(abs(trtdata[,12])) #abs then log for TLP.x
trtdata <- trtdata %>% mutate(TLP_tran_fig = TLP_tran*-1,
                              TLP.x_fig = TLP.x*-1)  #make neg. to interpret

library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify); library(ggplot2)
data(iris)
iris.pca <- iris[c(1, 2, 3, 4)] 
autoplot(prcomp(iris.pca))

library(ggfortify)

pca_res <- stats::prcomp(na.omit(test[,-1]), scale. = TRUE)

ggplot2::autoplot(pca_res)

library(ggplot2)
dtp <- data.frame('Species' = subdfPCA$species, my.PCA$x[,1:2]) # the first two componets are selected (NB: you can also select 3 for 3D plottings or 3+)
ggplot(data = dtp) + 
  geom_point(aes(x = PC1, y = PC2, size = Species)) + 
  theme_minimal() 


# set up for PCA
sum(duplicated(trtdata$species))
test <-  trtdata %>% filter(duplicated(trtdata$species))
test <- test %>% select(species, height.y, rootdiam.y, SRL.y, RTD.y, 
                        rootN.y, SLA.y, LTD.y, LDMC.y, leafarea.y, leafN.y, TLP_tran)
test <- na.omit(test)
#test <- na.omit(test)
my.PCA <- prcomp(test[,-1], scale = TRUE)
fviz_pca_biplot(my.PCA)#, habillage = test$species)



trtdata$trtcnt <- rowSums(!is.na(trtdata[,c(1:13)])) #count how many trait estimates for each population
subdfPCA <- trtdata %>% filter(trtcnt >= 5) #filter for rows with at least 9 traits present (36 populations)
#subdfPCA <- subdfPCA %>% select() #select a few key traits for PCA

unique(subdfPCA$species) #how many species in the 40 selected populations (31)
# fix species names 
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

library("factoextra")
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
fviz_pca_biplot(my.PCA, repel=T,
                #habillage=subdfPCA$species,
                legend.title ="Species", geom = "point", label = "var")+
  scale_shape_manual(values=c(19,19,19,19,19,19,19,19,19,19,19))#+
geom_point(alpha=2)

#export
ggsave(pcaplot, filename = "pcaplot.png", dpi=300, height = 4,width =6)

write.csv(my.PCA$rotation[,c(1,2)], "PCAloading.csv")
#save csv to copy from

?FactoMineR::PCA
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
