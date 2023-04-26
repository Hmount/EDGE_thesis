#### All figures in text and supporting info ####

## library 
library(tidyverse)
library(ggpubr)
library(stringr)
library(factoextra)

NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Chihuahuan Desert", "Great Plains Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))

## functions
# function to extract a legend from ggplots to use on grouped figures:
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

#function to extract and plot tukey results 
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompView::multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

#
#### Figure 1; ####             
# conceptual diagram #1 shows the options for a hypothesized relationship between
# population response to drought and to neighbors
set.seed(114009)
x= runif(100,-10,10)
y= x * rnorm(100,-1,1) -x
fd1=data.frame(x,y, group="Grime")
y= - x * rnorm(100,-1,1)+x
fd2=data.frame(x,y, group="Tilman")

fakedata <- rbind(fd1,fd2)

conceptfig_redo <- ggplot(fakedata, aes(x=x,y=y,lty=group))+
 # geom_point(size=1, alpha=.5)+
  geom_smooth(method="lm", se=F,size=1, color="black")+
  labs(y="Fitness with neighbors", x = "Fitness in drought", lty = "Theory")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "white"),
        axis.text.y.left = element_text(color = "white"),
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")))
conceptfig_redo
#export
ggsave(conceptfig_redo, filename = "figures/conceptual_redo.jpg", dpi=300, height = 3,width =5)


#### Figure 2; ####
# conceptual diagram/ visualization of methods used to estimate population growth 
# rates in different conditions and the response to drought and/or neighbors.
# Mathematical graphs produced below, rest of figure constructed in PowerPoint.

#example of population growth rates over time (as measured by cover)
set.seed(29)
intra <- rnorm(100)
inter <- rnorm(100)
f <- -intra -inter +rnorm(100)
cols <- rep('black', length(f))
cols[c(5:9)] <- 'red'
cexs <- rep(.5, length(f))
cexs[c(5:9)] <- 1.5
par(mfrow=c(1,2),mar=c(4,4,2,1), xaxt='n', yaxt='n')
plot(f[21:38], type="l",lwd=2, ylab="", xlab="", col="black", bg="transparent")
points(f[21:38], cex=cexs, pch=19, col=cols)
title(ylab="log(cover)", line=1, cex.lab=1.5)
title(xlab="Time (years)", line=1, cex.lab=1.5)
dev.off()

#3D diagram of inter- and intra-specific cover on lambda 
library(rgl) 
library(fields)

## make some data 
grid.l<- list( seq(-2,2,length=20), seq(-2,2,length=20))
ae <- make.surface.grid(grid.l)
z.fit <- -ae[,1] -ae[,2]
ae.norm <- as.surface(ae, z.fit)

## make a plot 
plot3d(ae.norm, type = 'n', xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-3, 3), xlab = 'interspecific cover', ylab = 'intraspecific cover', zlab = 'log(lambda)', theta=40,phi=30) 
#Add planes
planes3d(-.5, 1, 2, 2, col = 'red', alpha = 0.6)
planes3d(-.5, 1, 2, -1, col = 'cyan', alpha = 0.6)
lines3d(x=c(0, 0),y=c(-2, -2),z=c(-3, 3),col="black") 
#lines3d(x=c(0, 2),y=c(-2, -2),z=c(0, 0),col="darkred") 
lines3d(x=c(0, 2),y=c(-2, -2),z=c(1.5, 1.5),col="darkcyan", lwd=2) 
#points3d(x=2,y=-2,z=2,cex=0.5, pch=19, size=10,col="darkcyan")
points3d(x=2,y=-2,z=.5,cex=0.5, pch=19, size=10,col="darkred")
points3d(x=0,y=-2,z=1.5, pch=1,size=20, col="darkcyan")
#points3d(x=0,y=-2,z=0,cex=0.5, pch=24, size=20,col="darkred")


#### Figure 3; ####
# large combined figure showing the population growth rates distribution in different 
# conditions (histograms), differences in growth rates between grasslands (boxplots),
# differences in total cover between grasslands (boxplot).
NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure data is properly leveled
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
# differences in quadrat-level cover by year, treatment, and grassland
allsum <- read.csv("data/allsum_quadrat.csv") #data
allsum$year <- as.factor(allsum$year)
allsum <- allsum %>% #ensure data is properly leveled
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))

# histograms of all calculated growth rate measures: 
# (All are non-normal and have a right/positive skew). 
# #intrinsic growth rate ambient (rintA)
# rinta_hist <- ggplot(NEWallsite, aes(x=intrinsicLDGRcon))+
#   geom_histogram(binwidth = .1, fill="black")+
#   theme_classic()+
#   labs(x=expression(italic(r)[intA]), y = " ")+
#   xlim(-1,4)+
#   theme(legend.position = "right", axis.title.x = element_text(size=15))
#intrinsic growth rate drought (rintD)
rintd_hist <- ggplot(NEWallsite, aes(x=intrinsicLDGRchr))+
  geom_histogram(binwidth = .1, fill="black")+
  theme_classic()+
  labs(x=expression(italic(r)[intD]), y = " ")+
  xlim(-1,4)+
  theme(legend.position = "right", axis.title.x = element_text(size=15))
#realized invasion ambient (rrinvA)
rrinva_hist <- ggplot(NEWallsite, aes(x=invasionLDGRcon))+
  geom_histogram(binwidth = .1, fill="black")+
  theme_classic()+
  labs(x=expression(italic(r)[rinvA]), y = " ")+
  xlim(-1,4)+
  theme(legend.position = "right", axis.title.x = element_text(size=15))
# #realized invasion drought (rrinvD)
# rrinvd_hist <- ggplot(NEWallsite, aes(x=invasionLDGRchr))+
#   geom_histogram(binwidth = .1, fill="black")+
#   theme_classic()+
#   labs(x=expression(italic(r)[rinvD]), y = " ")+
#   xlim(-1,4)+
#   theme(legend.position = "right", axis.title.x = element_text(size=15))
#combined histogram figure for larger combo figure 
histcombo <- ggarrange(rintd_hist, rrinva_hist, nrow=2, ncol=1, 
                       common.legend = F, labels = c("a","b"), label.x = .9)
histcombo <- annotate_figure(histcombo,left = "Number of Observations") #add axis label

# Boxplots showing differences in population growth rates between grasslands: 
# #intrinsic rate of increase in ambient 
# rinta_box <- ggplot(NEWallsite, aes(x=intrinsicLDGRcon, y=grassland_type, fill=grassland_type))+
#   geom_boxplot()+
#   scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
#   theme_classic()+
#   labs(x=expression(italic(r)[intA]), y = " ")+
#   xlim(-1,4)+
#   theme(legend.position="none",
#         axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))
#intrinsic rate of increase in drought
rintd_box <- ggplot(NEWallsite, aes(x=intrinsicLDGRchr, y=grassland_type, fill=grassland_type))+
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(x=expression(italic(r)[intD]), y = " ")+
  xlim(-1,4)+
  theme(legend.position="none",
        axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))
#realized invasion rate of increase in ambient
rrinva_box<- ggplot(NEWallsite, aes(x=invasionLDGRcon, y=grassland_type, fill=grassland_type))+
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(x=expression(italic(r)[rinvA]), y = " ")+
  xlim(-1,4)+
  theme(legend.position="none",
        axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))
# #realized invasion rate of increase in drought
# rrinvd_box<- ggplot(NEWallsite, aes(x=invasionLDGRcon, y=grassland_type, fill=grassland_type))+
#   geom_boxplot()+
#   scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
#   theme_classic()+
#   labs(x=expression(italic(r)[rinvD]), y = " ")+
#   xlim(-1,4)+
#   theme(legend.position="none",
#         axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))
#boxplot figure combined
boxcombo <- ggarrange(rintd_box, rrinva_box, nrow=2, ncol=1, 
                      common.legend = F, labels = c("c","d"), label.x = .9)
boxcombo <- annotate_figure(boxcombo,left = "Grassland")

## combined boxplot/histogram figure 
histboxcombo <- ggarrange(histcombo, boxcombo, nrow=1, ncol=2, common.legend = F)
histboxcombo #view


#precipitation gradient and gradient of competition for light:
summary(anova_cov <- aov(totalcov ~ grassland_type, data = allsum)) #remake anova  
tuk_cov <- TukeyHSD(anova_cov) #do tukey hosthoc
multcompView::multcompLetters4(anova_cov, tuk_cov) #letters for each site
#use function made earlier for tukey numbers
sum_labels <- generate_label_df(TUKEY=tuk_cov , variable = "grassland_type")#generate labels using function
names(sum_labels)<-c('Letters','grassland_type')#rename columns for merging
yvalue<-aggregate(totalcov~grassland_type, data=allsum, mean)# obtain letter position for y axis using means
tukeylabel<-merge(sum_labels,yvalue) #merge dataframes
#boxplot w/ labels and regression line
gcover <- ggplot(allsum, aes(x=grassland_type, y=totalcov, color=grassland_type))+
  geom_boxplot()+
  geom_smooth(method="lm", se=T, color="black", aes(group=1))+
  #scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  geom_text(data = tukeylabel, aes(x = grassland_type, y = totalcov, label = Letters), 
            hjust=2, vjust=-1.75, color="black")+
  theme_classic()+
  labs(x="Grassland", y = "Total cover (%)")+
  theme(legend.position="none",
        #axis.text.y.left = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1))

#export
summaryfig <- ggarrange(histboxcombo, gcover, nrow=2, heights = c(3,2), 
                        labels = c(" ","e"), label.x = .95)
ggsave(summaryfig, filename = "figures/summaryfig.jpg", dpi=300, height = 5,width =5)



#### Figure 4; ####
# response to drought in relation to neighbors response in ambient conditions
corr_fig <- ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr))+
  geom_point(shape=16) +
  geom_abline()+
  geom_smooth(method="lm", se=T, color="black")+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  #geom_text(aes(label=species),hjust=0, vjust=0) +
  labs(y=" ", x = " ")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())
#plot again by grassland type
corrgl_fig <- ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=grassland_type))+
  geom_point(alpha=.5, shape=16)+
  geom_abline()+
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(y=" ", x = " ", color="Grassland")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y.left = element_blank())

## figure together
# plot for extracting legend:
forlegend <- ggplot(NEWallsite, aes(y=invasionLDGRcon, x=intrinsicLDGRchr, color=grassland_type))+
  geom_point(size=1)+
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(y=" ", x = " ", color="Grassland")+
  theme_classic()+
  theme(legend.position = "right", legend.direction = "vertical")+
  guides(colour = guide_legend(ncol=1))
# Apply user-defined function to extract legend
shared_legend <- as_ggplot(extract_legend(forlegend))
#blank plot
spacer <- ggplot()+
  theme_void()

# combine all
corrfig <- ggarrange(corr_fig, corrgl_fig, nrow=2, ncol=1, common.legend = F, 
                     labels = c("a","b"), label.x = .95, label.y = 1.02)
corrfig <-annotate_figure(corrfig,
                          bottom = text_grob(bquote("("*italic(r)[intD]*")")), 
                          left = text_grob(bquote("("*italic(r)[rinvA]*")"), rot = 90))
corrlegend <- ggarrange(spacer,shared_legend, nrow = 2, heights = c(1,2))
corrfig <- ggarrange(corrfig, corrlegend, ncol=2, widths = c(2.2,.8)) #legend
corrfig

#export
ggsave(corrfig, filename = "figures/corrfig.jpg", dpi=300, height = 6,width =6)



#### Figure 5;#### 

#using ggpredict
library(ggeffects)
library(sjmisc)
LDMCmodA <- lm(invasionLDGRcon~LDMC*grassland_type, data=alldat)
preddata <- ggpredict(LDMCmodA, terms = c("LDMC", "grassland_type"))
mycols <- list("red", "rosybrown3", "skyblue2", "steelblue", "dark blue")
LDMCmodA_fig <- plot(preddata, add.data=T, colors = mycols, 
                     dot.alpha = .65, alpha = .5, limit.range = T,
                     show.legend = F, show.title = F)+
  facet_wrap(~group, nrow=1, labeller = label_wrap_gen(width = 5))+
  labs(x=" ", y= expression(italic(r)[rinvA]))+
  scale_x_continuous(n.breaks = 3)+
  theme_classic()+
  theme(strip.text = element_text(size=6))
  

LDMCmodD <- lm(intrinsicLDGRchr~LDMC*grassland_type, data=alldat)
preddata2 <- ggpredict(LDMCmodD, terms = c("LDMC", "grassland_type"))
LDMCmodD_fig <- plot(preddata2, add.data=T, colors = mycols, 
                     dot.alpha = .65, alpha = .5, limit.range = T,
                     show.legend = F, show.title = F)+
  facet_wrap(~group, nrow=1, labeller = label_wrap_gen(width = 5))+
  labs(x=" ", y= expression(italic(r)[intD]))+
  scale_x_continuous(n.breaks = 3)+
  theme_classic()+
  theme(strip.text = element_text(size=6))


#combine LDMC panel
LDMCpanel <- ggarrange(LDMCmodA_fig,LDMCmodD_fig, nrow=2)
LDMCpanel <- annotate_figure(LDMCpanel,bottom = "log(leaf dry matter content)") #add axis label
LDMCpanel

#TLP
TLPmodA <- lm(invasionLDGRcon~TLP*grassland_type, data=alldat)
preddata3 <- ggpredict(TLPmodA, terms = c("TLP", "grassland_type"))
TLPmodA_fig <- plot(preddata3, add.data=T, colors = mycols, 
                    dot.alpha = .65, alpha = .5, limit.range = T,
                    show.legend = F, show.title = F)+
  facet_wrap(~group, nrow=1, labeller = label_wrap_gen(width = 5))+
  labs(x=" ", y= " ")+
  theme_classic()+
  theme(strip.text = element_text(size=6))


TLPmodD <- lm(intrinsicLDGRchr~TLP*grassland_type, data=alldat)
preddata4 <- ggpredict(TLPmodD, terms = c("TLP", "grassland_type"))
TLPmodD_fig <- plot(preddata4, add.data=T, colors = mycols, 
                    dot.alpha = .65, alpha = .5, limit.range = T,
                    show.legend = F, show.title = F)+
  facet_wrap(~group, nrow=1, labeller = label_wrap_gen(width = 5))+
  labs(x=" ", y= " ")+
  scale_x_continuous(n.breaks = 4)+
  theme_classic()+
  theme(strip.text = element_text(size=6))

#combine TLP panel
TLPpanel <- ggarrange(TLPmodA_fig,TLPmodD_fig, nrow=2)
TLPpanel <- annotate_figure(TLPpanel,bottom = "log(tugor loss point)") #add axis label
TLPpanel

#combine both trait panels
trtfig <- ggarrange(LDMCpanel,TLPpanel, ncol=2)
trtfig

#export
ggsave(trtfig, filename = "figures/trtfig.png", dpi=300, height = 4,width =7)



#
#
#### Figure S1; ####
# population cover relationship with intra- and inter-specific cover
# create 12 panels to export as part of one single figure

## data
allraw <- read.csv("data/allraw.csv") #raw cover data
names <- read.csv("data/names.csv") #species names with a space for plotting

allraw <- merge(allraw, names, by.x="species", by.y="old_species") #merge data
allraw$trt = str_replace(allraw$trt,"con","Ambient") #fix trt names for legends
allraw$trt = str_replace(allraw$trt,"chr","Drought")

## make a figure to extract common legend and paste on each figure where desired.
forlegend <- ggplot(allraw, aes(x=log_cover, y=log_lambda, color = trt)) + # figure
  geom_point()+
  geom_smooth()+
  scale_color_manual(values = c("blue", "red"))+
  labs(color="Treatment")
shared_legend <- as_ggplot(extract_legend(forlegend)) # common legend

## make figures and export 
# CHYintra
CHY <- allraw %>% filter(site=="CHY")
CHYintra <- ggplot(CHY, aes(x=log_cover, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=4,
             labeller = labeller(new_species = label_wrap_gen(10))) + 
  theme(legend.position = "bottom", plot.margin=unit(c(.5,0,.5,.5), "cm")) + 
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(intraspecific cover)", 
       y= "log(lambda)", 
       title="Northern Mixedgrass-intraspecific",
       color="Treatment")+
  theme_bw()+
  theme(strip.text = element_text(face = "italic"),
        legend.direction = "horizontal",
        legend.position = c(.75,.05))
ggsave(CHYintra, filename = "figures/CHYintra.png", dpi=300, height = 8,width =6)
# CHYinter
CHYinter <- ggplot(CHY, aes(x=log_other, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=4,
             labeller = labeller(new_species = label_wrap_gen(10))) +
  theme_bw()+
  theme(legend.position = c(.75,.05),
        legend.direction = "horizontal",
        #plot.margin=unit(c(.5,0,.5,.5), "cm"),
        strip.text = element_text(face = "italic")) + 
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(interspecific cover)", y= "log(lambda)", 
       title="Northern Mixedgrass-interspecific",
       color= "Treament")
ggsave(CHYinter, filename = "figures/CHYinter.png", dpi=300, height = 8,width =6)

# HYSintra
HYS <- allraw %>% filter(site=="HYS")
HYSintra <- ggplot(HYS, aes(x=log_cover, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=5,
             labeller = labeller(new_species = label_wrap_gen(10))) + 
  theme(legend.position = "bottom", plot.margin=unit(c(.5,0,.5,.5), "cm")) + 
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(intraspecific cover)", y= "log(lambda)", 
       title="Southern Mixedgrass-intraspecific",
       color="Treatment")+
  theme_bw()+
  theme(strip.text = element_text(face = "italic"),
        legend.direction = "vertical",
        legend.position = c(.9,.07))
ggsave(HYSintra, filename = "figures/HYSintra.png", dpi=300, height = 8,width =6)
# HYSinter
HYSinter <- ggplot(HYS, aes(x=log_other, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=5,
             labeller = labeller(new_species = label_wrap_gen(10))) + 
  theme_bw()+
  theme(legend.position = c(.9,.07),
        legend.direction = "vertical",
        #plot.margin=unit(c(.5,0,.5,.5), "cm"),
        strip.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(interspecific cover)", y= "log(lambda)", 
       title="Southern Mixedgrass-interspecific",
       color="Treatment")
ggsave(HYSinter, filename = "figures/HYSinter.png", dpi=300, height = 8,width =6)

# SGSintra
SGS <- allraw %>% filter(site=="SGS")
SGSintra <- ggplot(SGS, aes(x=log_cover, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=4,
             labeller = labeller(new_species = label_wrap_gen(10))) + 
  theme(legend.position = "bottom", plot.margin=unit(c(.5,0,.5,.5), "cm")) + 
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(intraspecific cover)", y= "log(lambda)", 
       title="Shortgrass-intraspecific",
       color="Treatment")+
  theme_bw()+
  theme(strip.text = element_text(face = "italic"),
        legend.direction = "horizontal",
        legend.position = c(.6,.07))
ggsave(SGSintra, filename = "figures/SGSintra.png", dpi=300, height = 8,width =6)
# SGSinter
SGSinter <- ggplot(SGS, aes(x=log_other, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=4,
             labeller = labeller(new_species = label_wrap_gen(10))) +
  theme_bw()+
  theme(legend.position = c(.6,.07),
        legend.direction = "horizontal",
        #plot.margin=unit(c(.5,0,.5,.5), "cm"),
        strip.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(interspecific cover)", y= "log(lambda)", 
       title="Shortgrass-interspecific",
       color="Treatment")+
  ggsave(SGSinter, filename = "figures/SGSinter.png", dpi=300, height = 8,width =6)

# KNZintra
KNZ <- allraw %>% filter(site=="KNZ")
KNZintra <- ggplot(KNZ, aes(x=log_cover, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",nrow = 7, ncol=4,labeller = labeller(new_species = label_wrap_gen(10))) + 
  theme(legend.position = "bottom", plot.margin=unit(c(.5,0,.5,.5), "cm")) + 
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(intraspecific cover)", y= "log(lambda)",
       title="Tallgrass-intraspecific",
       color="Treatment")+
  theme_bw()+
  theme(strip.text = element_text(face = "italic"),
        legend.direction = "horizontal",
        legend.position = c(.6,.07))
ggsave(KNZintra, filename = "figures/KNZintra.png", dpi=300, height = 7,width =6)
# KNZinter
KNZinter <- ggplot(KNZ, aes(x=log_other, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=4,
             labeller = labeller(new_species = label_wrap_gen(10))) + 
  theme_bw()+
  theme(legend.position = c(.6,.07),
        legend.direction = "horizontal",
        #plot.margin=unit(c(.5,0,.5,.5), "cm"),
        strip.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(interspecific cover)", y= "log(lambda)", 
       title="Tallgrass-interspecific",
       color="Treatment")
ggsave(KNZinter, filename = "figures/KNZinter.png", dpi=300, height = 7,width =6)

# SBLintra
SBL <- allraw %>% filter(site=="sev.blue")
SBLintra <- ggplot(SBL, aes(x=log_cover, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=4,
             labeller = labeller(new_species = label_wrap_gen(10))) + 
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(intraspecific cover)", y= "log(lambda)",
       title="Southern Shortgrass-intraspecific",
       color="Treatment")+
  theme_bw()+
  theme(strip.text = element_text(face = "italic"),
        legend.direction = "horizontal",
        legend.position = "bottom")
ggsave(SBLintra, filename = "figures/SBLintra.png", dpi=300, height = 7,width =6)
# SBLinter
SBLinter <- ggplot(SBL, aes(x=log_other, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=4,
             labeller = labeller(new_species = label_wrap_gen(10))) + 
  theme_bw()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        #plot.margin=unit(c(.5,0,.5,.5), "cm"),
        strip.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(interspecific cover)", y= "log(lambda)", 
       title="Southern Shortgrass-interspecific",
       color="Treatment")
ggsave(SBLinter, filename = "figures/SBLinter.png", dpi=300, height = 7,width =6)

# SBKintra
SBK <- allraw %>% filter(site=="sev.black")
SBKintra <- ggplot(SBK, aes(x=log_cover, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=4,
             labeller = labeller(new_species = label_wrap_gen(10))) + 
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(intraspecific cover)", y= "log(lambda)",
       title="Desert Grassland-intraspecific",
       color="Treatment")+
  theme_bw()+
  theme(strip.text = element_text(face = "italic"),
        legend.direction = "horizontal",
        legend.position = c(.6,.09))
ggsave(SBKintra, filename = "figures/SBKintra.png", dpi=300, height = 7,width =6)
# SBKinter
SBKinter <- ggplot(SBK, aes(x=log_other, y=log_lambda, color = trt)) + 
  geom_point(size=.5) + 
  geom_smooth(method="lm") + 
  facet_wrap(~new_species, scales = "fixed",
             nrow = 7, ncol=4,
             labeller = labeller(new_species = label_wrap_gen(10))) + 
  theme_bw()+
  theme(legend.position = c(.6,.09),
        legend.direction = "horizontal",
        #plot.margin=unit(c(.5,0,.5,.5), "cm"),
        strip.text = element_text(face = "italic")) +
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "log(interspecific cover)", y= "log(lambda)", 
       title="Desert Grassland-interspecific",
       color="Treatment")

ggsave(SBKinter, filename = "figures/SBKinter.png", dpi=300, height = 7,width =6)


#### Figure S2; ####
# differences in quadrat-level cover by year, treatment, and grassland
allsum <- read.csv("data/allsum_quadrat.csv") #data (Cover summed at quadrrat level)
allsum$year <- as.factor(allsum$year)
#plot
supsum <- ggplot(allsum, aes(x=year, y=totalcov, fill=trt))+
  geom_boxplot()+
  scale_fill_manual(values=c("darkred", "darkcyan"), labels=c("Extreme drought", "Ambient precipitation"))+
  theme_classic()+
  labs(x="Year", y = "Cover", fill= "Treatment")+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 20, hjust = 1))+
  facet_wrap(~grassland_type, ncol=2, scales="free")
#export:
ggsave(supsum, filename = "figures/supsum.png", dpi=300, height = 6,width =5)
