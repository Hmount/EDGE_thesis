#### Analysis of popualtion responses to different conditions ####
# testing for a trade-off in responses to drought and neighbors in different conditions

# last edited 10/8/22 by Hailey Mount
# edited order for clarity and checked OLS results against Major axis (MA) as the analysis 
# same results. r as the stat I report for correlation, ANOVA's to show lack of difference
# between grassland types.

#### data and packages ####
library(tidyverse)
library(ggpubr)

NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))


#### first, how correlated are measures of the same response in different conditions? ####
#compare response to drought when alone and with neighbors
cor.test(NEWallsite$intrinsicdiff, NEWallsite$invasiondiff) #* 53% pos. cor
#not direct 1:1, no relationships w/ grassland or life history traits
ggplot(NEWallsite, aes(x=intrinsicdiff, y=invasiondiff, color=grassland_type))+
  geom_point()

#compare response to neighbors when in ambient and in drought
cor.test(NEWallsite$condiff, NEWallsite$chrdiff) #* 40% pos. cor
#even less direct 1:1, no relationships w/ grassland or life history traits
ggplot(NEWallsite, aes(x=condiff, y=chrdiff, color=grassland_type))+
  geom_point()

# response to drought is slightly more concerted, but both vary more than expected
# in different conditions


#### How do responses to drought relate to responses to neighbors in diff. conditions? ####
# using lm function to incorporate weights and compare model w/ and w/out grassland,
# but calculating r to report in paper (same as cor):
# (removing row 106 for -inf)
### neighbors response in ambient conditions: 
intcon <- lm(condiff~intrinsicdiff, weights=weight2, NEWallsite[-106,]) #model with weights 
summary(intcon) #uncorrelated -> the pure response to neighbors and to drought are not related 
#(not a physiological trade-off, but an expressed trade-off?)
sqrt(summary(intcon)$adj.r.squared)#calculate r

#run second model w/ grassland
intcon2_fig <- lm(condiff~intrinsicdiff*grassland_type, weights=weight2, NEWallsite[-106,]) #old finding 
summary(intcon2_fig) 
#compare w/ anova
anova(intcon,intcon2_fig)

### neighbors response in drought conditions: 
intchr <- lm(chrdiff~intrinsicdiff, weights=weight2, NEWallsite[-106,]) #new main finding
anova(intchr) #trade-off
sqrt(summary(intchr)$adj.r.squared)#calculate r

#run second model w/ grassland
intchr2 <- lm(chrdiff~intrinsicdiff*grassland_type, weights=weight2, NEWallsite[-106,]) #old finding 
anova(intchr2) 
#compare w/ anova
anova(intchr,intchr2)


#### repeat with Standard Major Axis regression ####
# RJG suggested to use MA regression instead of OLS to avoid causal relationships 

library(tidyverse)
library(smatr) #for major axis regression

alldat <- read.csv("allsite_new.csv")

# re-run model of response to neighbors and response to drought as major axis regression
#uncertainty is incorporated. cannot use weights because uncertainty is needed for both sides.
test <- sma(chrdiff~intrinsicdiff,NEWallsite[-106,]) # weights=weight2, ) 
summary.sma(test)
sqrt(test$r2[[1]]) #correlation
plot(test)

test1 <- sma(chrdiff~intrinsicdiff+grassland_type,NEWallsite[-106,])
anova(test,test1)
#### make figures ####
#plot:
intcon_fig <- ggplot(NEWallsite, aes(y=condiff,x=intrinsicdiff))+
  geom_point() +
  #scale_color_manual(values=c("red", "pink", "tan", "sky blue", "dark blue"))+
  geom_smooth(method="lm", se=T, color="black")+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  #geom_text(aes(label=species),hjust=0, vjust=0) +
  labs(y=" ", x = " ")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())
#plot by grassland type
intcon2_fig <- ggplot(NEWallsite,aes(y=condiff,x=intrinsicdiff, color = grassland_type)) +
  geom_point(size=1)+
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  #labs(x=" ",y=" ", color="Grassland Type")+
  labs(y=" ", x = " ", color="Grassland")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y.left = element_blank())

#plot:
intchr_fig <- ggplot(NEWallsite, aes(y=chrdiff,x=intrinsicdiff))+
  geom_point() +
  #scale_color_manual(values=c("red", "pink", "tan", "sky blue", "dark blue"))+
  geom_smooth(method="lm", se=T, color="black")+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  #geom_text(aes(label=species),hjust=0, vjust=0) +
  labs(y=" ", x = " ")+
  theme_classic()+
  theme(axis.title.y.left = element_blank())
#plot by grassland type
intchr2_fig <- ggplot(NEWallsite,aes(y=chrdiff,x=intrinsicdiff, color = grassland_type)) +
  #geom_point(size=1)+
  geom_point(alpha=.5, shape=16)+
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_vline(xintercept=0, linetype="dashed")+
  scale_color_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  #scale_color_manual(values=cc)+
  #scale_color_manual(values=c("red", "pink", "tan", "sky blue", "dark blue"))+
  labs(y=" ", x = " ", color="Grassland")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y.left = element_blank())


# extract common legend:
# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

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
  theme(legend.position = "bottom", legend.direction = "horizontal")
# Apply user-defined function to extract legend
shared_legend <- as_ggplot(extract_legend(forlegend))


#combine
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

#### supplemental ####
# ANOVA of each response ~ grassland
# does response to drought differ by grassland? (no)
summary(aov(intrinsicdiff ~ grassland_type, NEWallsite)) #no difference
#boxplot response to drought
boxd <- ggplot(NEWallsite,aes(x=intrinsicdiff, y=grassland_type, fill = grassland_type)) +
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(y=" ",x="Response to drought", fill="Grassland")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.y = element_blank())

# does response to neighbors in ambient differ by grassland? (yes)
summary(con_anova <-aov(condiff ~ grassland_type, NEWallsite)) #sig. difference
con_tuk <- TukeyHSD(con_anova) #southern-mixed vs. drier sites
multcompView::multcompLetters4(con_anova, con_tuk) #letters for each site
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
sum_labels <- generate_label_df(TUKEY=con_tuk , variable = "grassland_type")#generate labels using function
names(sum_labels)<-c('Letters','grassland_type')#rename columns for merging
yvalue<-aggregate(condiff~grassland_type, data=NEWallsite, mean)# obtain letter position for y axis using means
tukeylabel<-merge(sum_labels,yvalue) #merge dataframes
#boxplot response to neighbors in ambient
boxna <- ggplot(NEWallsite,aes(x=condiff, y=grassland_type, fill = grassland_type)) +
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(y=" ",x="Response to neighbors in ambient", fill="Grassland")+
  geom_text(data = tukeylabel, aes(y = grassland_type, x = condiff, label = Letters), 
            hjust=-3.5, vjust=-.5 )+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.y = element_blank())

# does response to neighbors in ambient differ by grassland? (no)
summary(aov(chrdiff ~ grassland_type, NEWallsite)) #no difference
#boxplot response to neighbors in drought
boxnd <- ggplot(NEWallsite,aes(x=chrdiff, y=grassland_type, fill = grassland_type)) +
  geom_boxplot()+
  scale_fill_manual(values=c("red", "tomato", "rosybrown3", "skyblue2", "steelblue", "dark blue"))+
  labs(y=" ",x="Response to neighbors in drought", fill="Grassland")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.y = element_blank())


bplots <- ggarrange(boxd, boxnd, boxna, nrow=1, ncol=3,
                    common.legend = T, legend = "bottom",labels = c("a","b","c"))
bplots<-annotate_figure(bplots,left = "Grassland")
bplots

#export
ggsave(bplots, filename = "figures/grassland_anovas.png", dpi=300, height = 3,width =8)
