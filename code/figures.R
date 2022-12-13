#### All figures in text and supporting info ####

## library 
library(tidyverse)
library(ggpubr)
library(stringr)
library(factoextra)

## data
NEWallsite <- read.csv("data/allsite_new.csv") #data for Figure 3, 4, S3
NEWallsite <- NEWallsite %>% #ensure data is properly leveled
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
## functions
# function to extract a legend from ggplots to use on grouped figures:
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
#function to create gradient background on ggplot
make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
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

#### Figure 1; ####
# conceptual diagram #1 shows the options for a hypothesized trade-off between
# population response to drought and to neighbors

#create gradient
grad <- make_gradient(
  deg = 130, n = 500, cols = c("darkgrey","White","darkgrey")
)
#for Hypothesis B:
set.seed(1998)
x= rnorm(50, 10, 1) #set response to drought mean and sd 
y= -x - rnorm(50,1,3) + rnorm(50,.1,.1) # add response to neighbors (high for wet) weak slope
fd1=data.frame(x,y, group="Xeric")
x= rnorm(50, 15, 1) #set response to drought mean and sd 
y= -x - rnorm(50,5,1.5) # add response to neighbors (med for int) some variation
fd2=data.frame(x,y, group="Mesic")
x= rnorm(50, 20, 1) #set response to drought mean and sd 
y= -x - rnorm(50,8,.6) # add response to neighbors (low for dry), just a little variation
fd3=data.frame(x,y, group="Intermediate")
fakedata <- rbind(fd1,fd2,fd3)
#figure
B <- ggplot(fakedata, aes(x=x,y=y,color=group))+
  annotation_custom(grad,xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_point(size=1)+
  geom_smooth(method="lm", se=F)+
  scale_color_manual(values=c("sky blue", "dark blue", "black"), labels=c("Xeric", "Intermediate", "Mesic"))+
  labs(y=" ", x = " ", color = "Precipitation gradient")+
  #theme_classic()+
  annotate(geom="text", x=22, y=-7, label="Darwinian 
Demon", size=5, family="serif")+
  annotate(geom="text", x=9.1, y=-30, label="Competitively 
Excluded", size=5, family="serif")+
  theme(legend.text = element_text(family = "serif"),
        legend.title = element_text(family = "serif"),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20),
        axis.line = element_line(arrow = arrow(type='closed', length = unit(10,'pt'))))
B

#this one for hypothesis A:
set.seed(210122)
x= rnorm(50, 10, 1)
y= -x*x*rnorm(50, 9.8, .8) + rnorm(50, 10, 1)
fd1=data.frame(x,y, group="Xeric")
xx= rnorm(50, 10, .05)
y= -xx*x*x
fd2=data.frame(x,y, group="Intermediate")
xx= rnorm(50, 10, .5)
y= -xx*x*x
fd3=data.frame(x,y, group="Mesic")
fakedata=rbind(fd1,fd2,fd3)
fakedatajitter <- fakedata %>% filter(group=="Mesic")
fakedata2 <- fakedata %>% filter(group!="Mesic")
A <- ggplot(fakedata, aes(x=x,y=y,color=group))+
  annotation_custom(grad,xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  geom_point(size=1)+
  geom_smooth(method="lm", se=F)+
  scale_color_manual(values=c("sky blue", "dark blue", "black"), labels=c("Xeric", "Intermediate", "Mesic"))+
  labs(y=" ", x = " ", color = "Precipitation gradient")+
  theme_classic()+
  annotate(geom="text", x=12,y=-690, label="Darwinian 
Demon", size=5, family="serif")+
  annotate(geom="text", x=8.5, y=-1500, label="Competitively 
Excluded", size=5, family="serif")+
  theme(legend.text = element_text(family = "serif"),
        legend.title = element_text(family = "serif"),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        axis.line = element_line(arrow = arrow(type='closed', length = unit(10,'pt'))))
A

#combine
conceptfig<-ggarrange(A,B,nrow=2, ncol=1, common.legend = T, labels = c("a","b"))
conceptfig<-annotate_figure(conceptfig,left =text_grob("Response to neighbors", size =20, rot=90, family="serif"), bottom=text_grob("Response to drought", size=20, family="serif"))
conceptfig

#export
ggsave(conceptfig, filename = "figures/conceptual_hypotheses.png", dpi=300, height = 7,width =5)

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
lines3d(x=c(0, 2),y=c(-2, -2),z=c(0, 0),col="darkred") 
lines3d(x=c(0, 2),y=c(-2, -2),z=c(1.5, 1.5),col="darkcyan", lwd=2) 
points3d(x=2,y=-2,z=2,cex=0.5, pch=19, size=10,col="darkcyan")
points3d(x=2,y=-2,z=.5,cex=0.5, pch=19, size=10,col="darkred")
points3d(x=0,y=-2,z=1.5, pch=1,size=20, col="darkcyan")
points3d(x=0,y=-2,z=0,cex=0.5, pch=24, size=20,col="darkred")


#### Figure 3; ####
# large combined figure showing the population growth rates distribution in different 
# conditions (histograms), differences in growth rates between grasslands (boxplots),
# differences in total cover between grasslands (boxplot), and the effect of cover on
# responses to neighbors.
NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure data is properly leveled
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))

# histograms of all calculated growth rate measures: 
# (All are non-normal and have a right/positive skew). 
#intrinsic growth rate ambient (rintA)
rinta_hist <- ggplot(NEWallsite, aes(x=intrinsicLDGRcon))+
  geom_histogram(binwidth = .1, fill="black")+
  theme_classic()+
  labs(x=expression(italic(r)[intA]), y = " ")+
  xlim(-1,4)+
  theme(legend.position = "right", axis.title.x = element_text(size=15))
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
#realized invasion drought (rrinvD)
rrinvd_hist <- ggplot(NEWallsite, aes(x=invasionLDGRchr))+
  geom_histogram(binwidth = .1, fill="black")+
  theme_classic()+
  labs(x=expression(italic(r)[rinvD]), y = " ")+
  xlim(-1,4)+
  theme(legend.position = "right", axis.title.x = element_text(size=15))
#combined histogram figure for larger combo figure 
histcombo <- ggarrange(rinta_hist, rintd_hist, rrinva_hist, rrinvd_hist, nrow=4, ncol=1, 
                       common.legend = F, labels = c("a","b","c","d"), label.x = .9)
histcombo <- annotate_figure(histcombo,left = "Number of Observations") #add axis label

# Boxplots showing differences in population growth rates between grasslands: 
#intrinsic rate of increase in ambient 
rinta_box <- ggplot(NEWallsite, aes(x=intrinsicLDGRcon, y=grassland_type, fill=grassland_type))+
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(x=expression(italic(r)[intA]), y = " ")+
  xlim(-1,4)+
  theme(legend.position="none",
        axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))
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
# because this ANOVA had  significant differences, extract the tukey results to plot on figure
summary(siganova <- aov(invasionLDGRcon ~ grassland_type, data = NEWallsite)) # ANOVA w/ sig. diff
tuk.siganova <- TukeyHSD(siganova) #NS differs from D and SS
multcompView::multcompLetters4(siganova, tuk.siganova) #letters for each site
sum_labels <- generate_label_df(TUKEY=tuk.siganova , variable = "grassland_type")#generate labels using function
names(sum_labels)<-c('Letters','grassland_type')#rename columns for merging
yvalue<-aggregate(invasionLDGRcon~grassland_type, data=NEWallsite, min)# obtain letter position for y axis using means
tukeylabel<-merge(sum_labels,yvalue) #merge dataframes
#figure:
rrinva_box<- ggplot(NEWallsite, aes(x=invasionLDGRcon, y=grassland_type, fill=grassland_type))+
  geom_boxplot()+
  geom_text(data = tukeylabel, aes(x=invasionLDGRcon, y=grassland_type, label = Letters),
            hjust=2, vjust=.5 )+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(x=expression(italic(r)[rinvA]), y = " ")+
  xlim(-1,4)+
  theme(legend.position="none",
        axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))
#realized invasion rate of increase in drought
rrinvd_box<- ggplot(NEWallsite, aes(x=invasionLDGRcon, y=grassland_type, fill=grassland_type))+
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(x=expression(italic(r)[rinvD]), y = " ")+
  xlim(-1,4)+
  theme(legend.position="none",
        axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))
#boxplot figure combined
boxcombo <- ggarrange(rinta_box, rintd_box, rrinva_box, rrinvd_box, nrow=4, ncol=1, 
                      common.legend = F, labels = c("e","f","g","h"), label.x = .9)
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
gcover <- ggplot(allsum, aes(x=grassland_type, y=totalcov, fill=grassland_type))+
  geom_boxplot()+
  geom_smooth(method="lm", se=T, color="black", aes(group=1))+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  geom_text(data = tukeylabel, aes(x = grassland_type, y = totalcov, label = Letters), 
            hjust=2, vjust=-1.75 )+
  theme_classic()+
  labs(x="Grassland", y = "Total cover (%)")+
  theme(legend.position="none",
        #axis.text.y.left = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1))

#total cover at each grassland does not have a relationship with pop. responses to neighbors:
#for response to neighbors in ambient
concov <- ggplot(covergrassland,aes(x=meanother, y=condiff, color=grassland_type))+
  geom_point()+
  geom_hline(aes(yintercept=0), linetype="dotted")+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(y="Response to neighbors in ambient", x = "Interspecific cover (%)")+
  theme(legend.position="none")
#for response to neighbors in drought
chrcov <- ggplot(covergrassland,aes(x=meanother, y=chrdiff, color=grassland_type))+
  geom_point()+
  geom_hline(aes(yintercept=0), linetype="dotted")+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(y="Response to neighbors in drought", x = "Interspecific cover (%)")+
  theme(legend.position="none")

##combine
#figure together
covfig <- ggarrange(gcover,concov,chrcov, nrow=3, ncol=1, common.legend = F, 
                    labels = c("i","j","k"), label.x = .9)

#all combined summary stats figure for paper
summaryfig <- ggarrange(histboxcombo,covfig,nrow=1, ncol=2, widths = c(2,1.5))
#add legend
forlegend <- ggplot(covergrassland,aes(x=meanother, y=condiff, color=grassland_type))+
  geom_point()+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(color="Grassland type")+
  theme(legend.direction = "horizontal")+
  guides(colour = guide_legend(nrow = 1))
combinedlegend <- as_ggplot(extract_legend(forlegend))
summaryfig <- ggarrange(summaryfig,combinedlegend,nrow=2, ncol=1, heights = c(2,.15))
summaryfig

#export
ggsave(summaryfig, filename = "figures/summaryfig.png", dpi=300, height = 8,width =9)


#### Figure 4; ####
# response to drought in relation to neighbors response in ambient conditions
intcon_fig <- ggplot(NEWallsite, aes(y=condiff,x=intrinsicdiff))+
  geom_point(shape=16) +
  #geom_smooth(method="lm", se=T, color="black")+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  #geom_text(aes(label=species),hjust=0, vjust=0) +
  labs(y=" ", x = " ")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())
#plot again by grassland type
intcon2_fig <- ggplot(NEWallsite,aes(y=condiff,x=intrinsicdiff, color = grassland_type)) +
  geom_point(alpha=.5, shape=16)+
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(y=" ", x = " ", color="Grassland")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y.left = element_blank())

# response to drought in relation to neighbors response in drought conditions
intchr_fig <- ggplot(NEWallsite, aes(y=chrdiff,x=intrinsicdiff))+
  geom_point(shape=16) +
  geom_smooth(method="lm", se=T, color="black")+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  #geom_text(aes(label=species),hjust=0, vjust=0) +
  labs(y=" ", x = " ")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())
#plot again by grassland type
intchr2_fig <- ggplot(NEWallsite,aes(y=chrdiff,x=intrinsicdiff, color = grassland_type)) +
  geom_point(alpha=.5, shape=16)+
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
forlegend <- ggplot(NEWallsite,aes(y=condiff,x=intrinsicdiff, color = grassland_type)) +
  geom_point(size=1)+
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  #labs(x=" ",y=" ", color="Grassland Type")+
  labs(y=" ", x = " ", color="Grassland")+
  theme_classic()+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  guides(colour = guide_legend(nrow = 1))
# Apply user-defined function to extract legend
shared_legend <- as_ggplot(extract_legend(forlegend))

# combine all
leftcol <- ggarrange(intcon_fig, intcon2_fig, nrow=2, ncol=1, common.legend = F, 
                     labels = c("a","c"), label.x = .9)
leftcol <-annotate_figure(leftcol,left = "Response to neighbors in ambient")

rightcol <- ggarrange(intchr_fig, intchr2_fig, nrow=2, ncol=1, common.legend = F, 
                      labels = c("b","d"), label.x = .9)
rightcol <-annotate_figure(rightcol,left = "Response to neighbors in drought")

tradeoffcombo <- ggarrange(leftcol, rightcol, nrow=1, ncol=2) 
tradeoffcombo<-annotate_figure(tradeoffcombo,bottom = "Response to drought")
tradeoffcombo <- ggarrange(tradeoffcombo, shared_legend, nrow=2, heights = c(2.5,.5)) #legend
tradeoffcombo

#export
ggsave(tradeoffcombo, filename = "figures/tradeoffcombo.png", dpi=300, height = 7,width =8)


#### Figure 5; ####
# plot species that appear on multiple grasslands to see how popualtion differ in responses
#subset species that have response estimates from multiple grasslands
multiples <- as.data.frame(table(NEWallsite$species))
multiples <- multiples%>%filter(Freq>=2) #get spp w/ 2+ observations
multiples #check
allmultiples <- NEWallsite %>% filter(species%in%multiples$Var1) #subset

subBG <- allmultiples %>% filter(species=="Boutelouagracilis")
subPT <- allmultiples %>% filter(species=="Psoraleatenuiflora")
subAE <- allmultiples %>% filter(species=="Asterericoides")
subspp <- allmultiples %>% mutate(Species = ifelse(species %in% "Boutelouagracilis", "Bouteloua gracilis",
                                                   ifelse(species %in% "Psoraleatenuiflora", "Psoralea tenuiflora",
                                                          ifelse(species %in% "Asterericoides", "Aster ericoides", "Other"))))


repeatspecies <- ggplot(subspp, aes(y=chrdiff,x=intrinsicdiff, group=species, color=Species))+
  geom_smooth(method="lm", se=F)+
  #geom_smooth(data= subBG, method="lm", se=F, color="blue")+
  #geom_smooth(data= subPT, method="lm", se=F, color="green")+
  #geom_smooth(data= subAE, method="lm", se=F, color="red")+
  scale_color_manual(name="Species", values=c("Aster ericoides"= "red",
                                              "Bouteloua gracilis" ="blue",
                                              "Psoralea tenuiflora" = "green",
                                              "Other"="black"),
                     guide=guide_legend(label.theme = element_text(face="italic", size=10)))+
  geom_hline(yintercept=0, linetype="dotdash")+
  geom_vline(xintercept=0, linetype="dotdash")+
  labs(y="Response to neighbors in drought", x = "Response to drought", color="Species")+
  theme_classic()+
  theme(legend.position = c(.86,.86))

#export
ggsave(repeatspecies, filename = "figures/repeatspecies.png", dpi=300, height = 5,width =8)



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
allsum <- read.csv("data/allsum_quadrat.csv") #data
allsum$year <- as.factor(allsum$year)
#plot
supsum <- ggplot(allsum, aes(x=year, y=totalcov, fill=trt))+
  geom_boxplot()+
  scale_fill_manual(values=c("darkred", "darkcyan"), labels=c("Extreme drought", "Ambient precipitation"))+
  theme_classic()+
  labs(x="Year", y = "Cover", fill= "Treatment")+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 20, hjust = 1))+
  facet_wrap(~grassland_type, ncol=2, scales="fixed")
#export:
ggsave(supsum, filename = "figures/supsum.png", dpi=300, height = 6,width =5)

#### Figure S3; ####
# boxplots showing ANOVA results for each difference in mean population response ~ grassland
#population responses to drought (no significant differences):
boxd <- ggplot(NEWallsite,aes(x=intrinsicdiff, y=grassland_type, fill = grassland_type)) +
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(y=" ",x="Response to drought", fill="Grassland")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.y = element_blank())

#population response to neighbors in ambient conditions (sig. difference): 
summary(con_anova <-aov(condiff ~ grassland_type, NEWallsite)) #sig. difference
con_tuk <- TukeyHSD(con_anova) #southern-mixed vs. two driest sites
sum_labels <- generate_label_df(TUKEY=con_tuk , variable = "grassland_type")#generate labels using function
names(sum_labels)<-c('Letters','grassland_type')#rename columns for merging
yvalue<-aggregate(condiff~grassland_type, data=NEWallsite, mean)# obtain letter position for y axis using means
tukeylabel<-merge(sum_labels,yvalue) #merge dataframes
#boxplot
boxna <- ggplot(NEWallsite,aes(x=condiff, y=grassland_type, fill = grassland_type)) +
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(y=" ",x="Response to neighbors in ambient", fill="Grassland")+
  geom_text(data = tukeylabel, aes(y = grassland_type, x = condiff, label = Letters), 
            hjust=-3.5, vjust=-.5 )+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.y = element_blank())

#population response to neighbors in drought conditions (no significant differences): 
boxnd <- ggplot(NEWallsite,aes(x=chrdiff, y=grassland_type, fill = grassland_type)) +
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(y=" ",x="Response to neighbors in drought", fill="Grassland")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.y = element_blank())+
  guides(colour = guide_legend(nrow = 1))

# plot for extracting legend:
forlegend <- ggplot(NEWallsite,aes(x=chrdiff, y=grassland_type, fill = grassland_type)) +
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(y=" ",x="Response to neighbors in drought", fill="Grassland")+
  theme_classic()+
  theme(legend.direction = "horizontal",
        axis.text.y = element_blank())+
  guides(fill = guide_legend(nrow = 1))
shared_legend <- as_ggplot(extract_legend(forlegend))

bplots <- ggarrange(boxd, boxnd, boxna, nrow=1, ncol=3,labels = c("a","b","c"))
bplots <- ggarrange(bplots, shared_legend, nrow = 2, ncol=1, heights = c(2,.2))
bplots<-annotate_figure(bplots,left = "                  Grassland")
bplots

#export
ggsave(bplots, filename = "figures/bplots.png", dpi=300, height = 3,width =8)

