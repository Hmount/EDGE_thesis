#### Figure 1; ####             NOT CHANGED/FIXED YET!
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
ggsave(conceptfig, filename = "figures/conceptual_hypotheses.jpg", dpi=300, height = 7,width =5)
