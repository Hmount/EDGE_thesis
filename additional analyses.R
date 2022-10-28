#### additional analyses ####
# These anlayses were done, but did not make it into the final paper:
# do pop.responses differ by basic life history traits? (no)

# last edited 10/8/22 by Hailey Mount
# edited to be more concise and now including both Sevietta sites


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



#############################################traits
summary(mod_la<-lm(chrdiff~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #*
anova(mod_la) #grassland, CWM SLA
summary(mod_ln<-lm(chrdiff~leafN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
anova(mod_ln) #trait, trait*grassland, grassland, CWM SLA
summary(modt<-lm(chrdiff~LDMC.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #nah
summary(modt<-lm(chrdiff~LTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #nah
summary(mod_tlp<-lm(chrdiff~TLP_tran*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
anova(mod_tlp) #grassland, CWM SLA
summary(mod_sla<-lm(chrdiff~SLA.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
anova(mod_sla)# grassland, CWM SLA
summary(modt<-lm(chrdiff~height.y*grassland_type + SLA.x + TLP.x,data=trtdata)) #no
anova(modt) #close
summary(modt<-lm(chrdiff~RTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #no
summary(modt<-lm(chrdiff~rootN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #no
summary(modt<-lm(chrdiff~SRL.y*grassland_type + SLA.x + TLP.x , data=trtdata)) #close
anova(modt) #close
summary(modt<-lm(chrdiff~rootdiam.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #no

summary(mod_la<-lm(intrinsicdiff~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #*
anova(mod_la) #grassland*trait, trait
summary(mod_ln<-lm(intrinsicdiff~leafN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(mod_ln) 
summary(modt<-lm(intrinsicdiff~LDMC.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #nah
summary(modt<-lm(intrinsicdiff~LTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #nah
summary(mod_tlp<-lm(intrinsicdiff~TLP_tran*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
anova(mod_tlp) 
summary(mod_sla<-lm(intrinsicdiff~SLA.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
anova(mod_sla)
summary(modt<-lm(intrinsicdiff~height.y*grassland_type + SLA.x + TLP.x,data=trtdata)) #no
anova(modt) #close
summary(modt<-lm(intrinsicdiff~RTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #no
summary(modt<-lm(intrinsicdiff~rootN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #no
summary(modt<-lm(intrinsicdiff~SRL.y*grassland_type + SLA.x + TLP.x , data=trtdata)) #close
anova(modt) #close
summary(modt<-lm(intrinsicdiff~rootdiam.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #*
anova(modt) #trait*grassland
