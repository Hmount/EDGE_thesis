#### traits analysis ####

#### data and packages + cleaning ####
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


#### modelling; traits as predictors of population responses ####
# use multivariate linear model to see if responses are a function of focal traits* grassland + community CWM SLA and TLP
# run MOANOVA with significant model objects to test for significance of predictors to response interaction

summary(mod_la<-lm(cbind(intrinsicdiff, chrdiff)~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_la) 
summary(mod_ln<-lm(cbind(intrinsicdiff, chrdiff)~leafN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_ln) 
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~LDMC.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) #nothin
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~LTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) # not enough sites, exclude from analysis
summary(mod_sla<-lm(cbind(intrinsicdiff, chrdiff)~SLA.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_sla)
summary(mod_tlp<-lm(cbind(intrinsicdiff, chrdiff)~TLP_tran*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
car::Anova(mod_tlp) 
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~height.y*grassland_type + SLA.x + TLP.x,data=trtdata)) #no
car::Anova(modt)
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~RTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) # not enough sites, exclude from analysis
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~SRL.y*grassland_type + SLA.x + TLP.x , data=trtdata)) #close
car::Anova(modt) 
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~rootdiam.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) 
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~rootN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
car::Anova(modt) 

#### Supporting info; variation in traits ####
# traits vary much more strongly between species than within
# Because MANOVA show little explanatory power of traits, and traits only significantly differ 
# interspecifically, intraspecific variation cannot account for pop. differences.
# this suggests that the same traits/ species responds differently based on local biotic and abiotic conditions.
# (see Table S3)

# Caveat: while we attempted to account for intra variation as much as possible, there were limits to
# the trait data we could obtain and our results cannot conclusively dismiss the effects of 
# intraspeciifc traits variation on differences in population responses of the same species. 

data <- CWM_sitedata %>% filter()
data <- read.csv("data/MasterTrait.csv")
multiples <- as.data.frame(table(data$Species))
multiples <- multiples%>%filter(Freq>=2) #get spp w/ 2+ observations
multiples #check
allmultiples <- data %>% filter(Species%in%multiples$Var1) #subset

#test for variation in each trait between and across populations
anova(lm(log(SLA..cm2.g..cm2.g.1.)~Species, data=allmultiples))
anova(lm(log(Leaf.area..cm2.)~Species, data=allmultiples))
anova(lm(log(Leaf.N....)~Species, data=allmultiples))
anova(lm(log(Leaf.tissue.density..gcm.3.)~Species, data=allmultiples))
anova(lm(log(LDMC..g.g.)~Species, data=allmultiples))
anova(lm(log(abs(tugor.loss.point))~Species, data=allmultiples))
anova(lm(log(max.height.mm.)~Species, data=allmultiples))
anova(lm(log(SRL..m.g.)~Species, data=allmultiples))
anova(lm(log(Root.N..)~Species, data=allmultiples))
anova(lm(log(Root.tissue.density.gcm.3.)~Species, data=allmultiples))
anova(lm(log(allmultiples$Root.diameter..mm.)~Species, data=allmultiples))
