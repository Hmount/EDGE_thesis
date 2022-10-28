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
anova(modt) #trait, trait*grassland, CWM SLA
car::Anova(mod_la) #trait, trait*grassland, CWM SLA, CWM TLP
summary(mod_ln<-lm(cbind(intrinsicdiff, chrdiff)~leafN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
anova(mod_ln) #trait, trait*grassland, CWM SLA
car::Anova(mod_ln) #trait*grassland
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~LDMC.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #nah
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~LTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #nah
summary(mod_tlp<-lm(cbind(intrinsicdiff, chrdiff)~TLP_tran*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
anova(modt) #grassland, CWM SLA
Anova(mod_tlp) #trait, CWM SLA
summary(mod_sla<-lm(cbind(intrinsicdiff, chrdiff)~SLA.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #**
anova(modt)# trait*grassland, grassland, CWM SLA, CWM TLP
Anova(mod_sla) #grassland, CWM SLA, CWM TLP
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~height.y*grassland_type + SLA.x + TLP.x,data=trtdata)) #no
anova(modt) #close
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~RTD.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #no
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~rootN.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #no
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~SRL.y*grassland_type + SLA.x + TLP.x , data=trtdata)) #close
anova(modt) #close
summary(modt<-lm(cbind(intrinsicdiff, chrdiff)~rootdiam.y*grassland_type + SLA.x + TLP.x, data=trtdata)) #no
anova(modt) #no
car::Anova(modt)

library("corrplot")
test <- trtdata %>% dplyr::select(c(leafN.y,SLA.x,SLA.y,TLP.x_fig,TLP_tran_fig,leafarea.y))
M<-cor(test, use = "pairwise.complete.obs")
head(round(M,2))

# visualizing correlogram
# as circle
corrplot(M, method="circle")


##trying linear models no CWM
summary(mod_la<-lm(chrdiff~leafarea.y*grassland_type + leafN.y*grassland_type + rootdiam.y*grassland_type, data=trtdata)) #**
anova(mod_la)
summary(mod_la1<-lm(intrinsicdiff~leafarea.y*grassland_type + leafN.y*grassland_type + rootdiam.y*grassland_type, data=trtdata)) #**
anova(mod_la1)

summary(mod_la1<-lm(intrinsicdiff~leafN.y*grassland_type, data=trtdata)) #**
anova(mod_la1)

#### figures ####
# model multivariate response relationships (how?)

library(heplots)
heplot(mod_la)
heplot(mod_ln)
heplot(mod_tlp)
heplot(mod_sla)

glance.mlm(mod_la)

library(candisc)
ct <- candisc(mod_sla)
??candisc::heplot(ct)
plot(ct, ellipse = T)

#plot predictions from models for each response at each grassland 
ggplot(trtdata, aes(x=leafarea.y, y=intrinsicdiff, color=grassland_type))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~grassland_type, scales = "free")
ggplot(trtdata, aes(x=SLA.x, y=chrdiff))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(chrdiff~SLA.x, trtdata))
library(plotly)
plot_ly(x=trtdata$SLA.x, y=trtdata$instrinsicdiff, z=trtdata$leafarea.y, type="scatter3d")+
 # geom_point()+
  facet_wrap(~grassland_type)

library(scatterplot3d)
scatterplot3d(x=trtdata$SLA.x, y=trtdata$instrinsicdiff, z=trtdata$leafarea.y)

ggplot(trtdata, aes(x=SLA.x y=intrinsicdiff))

predict$predict <- 

nd <- data.frame(intrinsicdiff = c(0:100), chrdiff = c(0:100))
p <- predict(mod_la, nd)
p
predictionEllipse <- function(mod, newdata, level = 0.95, ggplot = TRUE){
  # labels
  lev_lbl <- paste0(level * 100, "%")
  resps <- colnames(mod$coefficients)
  title <- paste(lev_lbl, "confidence ellipse for", resps[1], "and", resps[2])
  
  # prediction
  p <- predict(mod, newdata)
  
  # center of ellipse
  cent <- c(p[1,1],p[1,2])
  
  # shape of ellipse
  Z <- model.matrix(mod)
  Y <- mod$model[[1]]
  n <- nrow(Y)
  m <- ncol(Y)
  r <- ncol(Z) - 1
  S <- crossprod(resid(mod))/(n-r-1)
  
  # radius of circle generating the ellipse
  # see Johnson and Wichern (2007), p. 399
  tt <- terms(mod)
  Terms <- delete.response(tt)
  mf <- model.frame(Terms, newdata, na.action = na.pass, 
                    xlev = mod$xlevels)
  z0 <- model.matrix(Terms, mf, contrasts.arg = mod$contrasts)
  rad <- sqrt((m*(n-r-1)/(n-r-m))*qf(level,m,n-r-m)*z0%*%solve(t(Z)%*%Z) %*% t(z0))
  
  # generate ellipse using ellipse function in car package
  ell_points <- car::ellipse(center = c(cent), shape = S, radius = c(rad), draw = FALSE)
  
  # ggplot2 plot
  if(ggplot){
    require(ggplot2, quietly = TRUE)
    ell_points_df <- as.data.frame(ell_points)
    ggplot(ell_points_df, aes(x, y)) +
      geom_path() +
      geom_point(aes(x = TOT, y = AMI), data = data.frame(p)) +
      labs(x = resps[1], y = resps[2], 
           title = title)
  } else {
    # base R plot
    plot(ell_points, type = "l", xlab = resps[1], ylab = resps[2], main = title)
    points(x = cent[1], y = cent[2])
  }
}
predictionEllipse(mod_la,trtdata)



#testing figures
library(visreg)

#la
m1<-lm(chrdiff~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)

m2<-lm(intrinsicdiff~leafarea.y*grassland_type + SLA.x + TLP.x, data=trtdata)



visreg(m2, xvar="leafarea.y", by="grassland_type", rug=F, 
       partial=F,xlab= "log(leaf area)", ylab=" ", 
       line=list(col="black"), gg=T)

visreg(m2, xvar="leafarea.y", by="grassland_type", rug=F, 
                partial=F,xlab= "log(leaf area)", ylab=" ", 
                line=list(col="black"), gg=T)+
  geom_rug(sides="b")+
  theme(axis.title.y.left=element_blank())+
  #ylim(-2,2) +
  coord_cartesian(ylim = c(-3, 4))+
  theme_classic()

visreg(m1, xvar="TLP.x", rug=F, partial=F,
                           cond=list(grassland_type="Southern Mixed"),
                           xlab= "log(community weighted mean SLA)", 
                           ylab=" ", gg=T, line=list(col="black"), band=T)+
  geom_rug(sides="b")+
  coord_cartesian(ylim = c(-3, 3))+
  theme_classic()


visreg(m2, xvar="TLP.x", rug=F, partial=F,
       by = "grassland_type",
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
anova(mod_ln) #trait*grassland
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
