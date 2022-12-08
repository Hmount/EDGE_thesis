#### All figures in text and supporting info ####

## library and functions
library(tidyverse)
library(ggpubr)
library(stringr)

# function to extract a legend from ggplots to use on grouped figures:
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}



#### Figure S1; ####
# population cover relationship with intra- and inter-specific cover
# create 12 panels to export as part of one single figure ####

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

