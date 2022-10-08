#### Summary stats ####
# how do growth rates vary by condition? (histograms). 

#### load in data and packages ####
library(tidyverse)
library(ggpubr)
NEWallsite <- read.csv("data/allsite_new.csv")
#ensure this data is releveled with grassland_type facets along precipitation gradient 
NEWallsite <- NEWallsite %>%
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
all <- read.csv("data/allraw.csv")

#### how do growth rates vary by condition (histograms) and across grasslands? (boxplots) ####
# histograms of all calculated growth rate measures: All are non-normal and have a right/positive 
# skew. Most species maintaining and some doing well is consistent with expectations. 

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
                       common.legend = F, labels = c("a","c","e","g"), label.x = .9)
histcombo <- annotate_figure(histcombo,left = "Number of Observations") #add axis label


#ANOVA and boxplots of calculated growth rate measures: no differences in growth rates by
# grassland type.

#ANOVA for differences in growth rates between grasslands
summary(aov(intrinsicLDGRcon ~ grassland_type, data = NEWallsite)) #no difference (but close)
summary(aov(intrinsicLDGRchr ~ grassland_type, data = NEWallsite)) #no difference
summary(aov(invasionLDGRcon ~ grassland_type, data = NEWallsite)) # sig. diff
TukeyHSD(aov(invasionLDGRcon ~ grassland_type, data = NEWallsite)) #NS differs from D and SS
summary(aov(invasionLDGRchr ~ grassland_type, data = NEWallsite)) #no difference (but close)
library(ggplot2)
#boxplots
rinta_box <- ggplot(NEWallsite, aes(x=intrinsicLDGRcon, y=grassland_type, fill=grassland_type))+
  geom_boxplot()+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(x=expression(italic(r)[intA]), y = " ")+
  xlim(-1,4)+
  theme(legend.position="none",
        axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))

rintd_box <- ggplot(NEWallsite, aes(x=intrinsicLDGRchr, y=grassland_type, fill=grassland_type))+
  geom_boxplot()+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(x=expression(italic(r)[intD]), y = " ")+
  xlim(-1,4)+
  theme(legend.position="none",
        axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))

rrinva_box<- ggplot(NEWallsite, aes(x=invasionLDGRcon, y=grassland_type, fill=grassland_type))+
  geom_boxplot()+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(x=expression(italic(r)[rinvA]), y = " ")+
  xlim(-1,4)+
  theme(legend.position="none",
        axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))

rrinvd_box<- ggplot(NEWallsite, aes(x=invasionLDGRcon, y=grassland_type, fill=grassland_type))+
  geom_boxplot()+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  theme_classic()+
  labs(x=expression(italic(r)[rinvD]), y = " ")+
  xlim(-1,4)+
  theme(legend.position="none",
        axis.text.y.left = element_blank(), axis.title.x = element_text(size=15))

#boxplot figure combined
boxcombo <- ggarrange(rinta_box, rintd_box, rrinva_box, rrinvd_box, nrow=4, ncol=1, 
                      common.legend = F, labels = c("b","d","f","h"), label.x = .9)
boxcombo <- annotate_figure(boxcombo,left = "Grassland")


## combined boxplot/histogram figure 
histboxcombo <- ggarrange(histcombo, boxcombo, nrow=1, ncol=2, common.legend = F)
histboxcombo #view


#### How does precipitation effect cover? ####
#does precipitation gradient coincide with gradient of competition for light
all <- all %>% mutate(grassland_type = ifelse(site %in% "CHY","Northern Mixed",
                                              ifelse(site %in% "HYS","Southern Mixed",
                                                     ifelse(site %in% "KNZ","Tallgrass",
                                                            ifelse(site %in% "SGS","Shortgrass","Shortgrass/Desert"))))) 
all <- all %>%
  mutate(grassland_type = fct_relevel(grassland_type, 
                                      "Shortgrass/Desert", "Shortgrass", "Northern Mixed", 
                                      "Southern Mixed", "Tallgrass"))

#summarizing quadrat-level cover
allsum <- all %>% group_by(grassland_type, year, trt, plot, subplot) %>% summarize(totalcov = sum(cover))
#relevel new df
allsum <- allsum %>%
  mutate(grassland_type = fct_relevel(grassland_type, 
                                      "Shortgrass/Desert", "Shortgrass", "Northern Mixed", 
                                      "Southern Mixed", "Tallgrass"))

#is cover significantly different between grasslands? (yes, each is uniquely different)
summary(anova_cov <- aov(totalcov ~ grassland_type, data = allsum)) #anova sig. 
tuk_cov <- TukeyHSD(anova_cov) #each site is significantly different

multcompView::multcompLetters4(anova_cov, tuk_cov) #letters for each site
#code to make function that extracts and plots tukey results 
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompView::multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}
sum_labels <- generate_label_df(TUKEY=tuk_cov , variable = "grassland_type")#generate labels using function
names(sum_labels)<-c('Letters','grassland_type')#rename columns for merging
yvalue<-aggregate(totalcov~grassland_type, data=allsum, mean)# obtain letter position for y axis using means
tukeylabel<-merge(sum_labels,yvalue) #merge dataframes


#use linear regression to assess changes along increasing cover
summary(lm_covsum <- lm(totalcov ~ grassland_type, data = allsum)) #sig. pos., 47%

#boxplot w/ labels and regression line
gcover <- ggplot(allsum, aes(x=grassland_type, y=totalcov, fill=grassland_type))+
  geom_boxplot()+
  geom_smooth(method="lm", se=T, color="black", aes(group=1))+
  scale_fill_manual(values=c("red", "pink", "tan", "sky blue", "dark blue"))+
  geom_text(data = tukeylabel, aes(x = grassland_type, y = totalcov, label = Letters), 
            hjust=2, vjust=-1.75 )+
  theme_classic()+
  labs(x="Grassland", y = "Total cover (%)")+
  theme(legend.position="none",
        #axis.text.y.left = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1))


#does average neighbor cover effect estimates of response to neighbors in either condition
#overall, no we aren't not concerned that these differences effected estimates 

#sum all the cover for each plot/ year/ grassland/ species (bounding measures between 0 and 100/300)
allothermeans <- all %>%
  filter(trt=="con") %>%
  group_by(grassland_type, species, plot, year, subplot) %>%
  summarize(meanother = mean(other)) %>% ungroup 
covergrassland <- full_join(NEWallsite, allothermeans, by=c("grassland_type", "species"))#merge dataframes
#make a model
summary(lm(condiff~meanother, covergrassland)) 
#plot:
concov <- ggplot(covergrassland,aes(x=meanother, y=condiff, color=grassland_type))+
  geom_point()+
  geom_hline(aes(yintercept=0), linetype="dotted")+
  scale_color_manual(values=c("red", "pink", "tan", "sky blue", "dark blue"))+
  theme_classic()+
  labs(y="Response to neighbors in ambient", x = "Interspecific cover (%)")+
  theme(legend.position="none")

#repeat for chrdiff (no diff)
allothermeans <- all %>%
  filter(trt=="chr")%>%
  group_by(grassland_type, species, plot, year, subplot) %>%
  summarize(meanother = mean(other)) %>% ungroup 
covergrassland <- full_join(NEWallsite, allothermeans, by=c("grassland_type", "species"))
#make a model
summary(lm(chrdiff~meanother, covergrassland)) 
#plot:
chrcov <- ggplot(covergrassland,aes(x=meanother, y=chrdiff, color=grassland_type))+
  geom_point()+
  geom_hline(aes(yintercept=0), linetype="dotted")+
  scale_color_manual(values=c("red", "pink", "tan", "sky blue", "dark blue"))+
  theme_classic()+
  labs(y="Response to neighbors in drought", x = "Interspecific cover (%)")+
  theme(legend.position="none")

#although the mean density of cover is significantly related to the response to neighbors,
#mean neighbor density actually explains very little variation in response to neighbors (con > chr)
#(increases in cover = decreases in GR responses to neighbors/ worse w/ neighbors)

#figure together
covfig <- ggarrange(gcover,concov,chrcov, nrow=3, ncol=1, common.legend = F, 
                    labels = c("i","j","k"), label.x = .9)

#all combined summary stats figure for paper
summaryfig <- ggarrange(histboxcombo,covfig,nrow=1, ncol=2, common.legend = T, widths = c(2,1.5))
summaryfig

#export
ggsave(summaryfig, filename = "summaryfig.png", dpi=300, height = 8,width =9)




######supplemental life history traits
#make fig?

#response to drought
anova(lm(intrinsicdiff~lifespan*grassland_type,data=NEWallsite))#?
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