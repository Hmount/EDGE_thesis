#### Conceptual diagrams ####
## First is the 3D diagram for inside of larger conceptual figure (made in ppt)
## Next is the main figure (figure 1) that shows hypothesized possibilities 

#### example of population growth rates over time (as measured by cover) ####
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


#### 3D diagram for inside of larger conceptual figure (rest of fig made in ppt) ####
library(rgl) 
library(fields)

## make some data 
grid.l<- list( seq(-2,2,length=20), seq(-2,2,length=20))
ae <- make.surface.grid(grid.l)
z.fit <- -ae[,1] -ae[,2]
ae.norm <- as.surface(ae, z.fit)

## make a plot 
plot3d(ae.norm, type = 'n', xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-3, 3), xlab = 'interspecific cover', ylab = 'intraspecific cover', zlab = 'log(lambda)', theta=40,phi=30) 
# Add planes
planes3d(-.5, 1, 2, 2, col = 'red', alpha = 0.6)
planes3d(-.5, 1, 2, -1, col = 'cyan', alpha = 0.6)
lines3d(x=c(0, 0),y=c(-2, -2),z=c(-3, 3),col="black") 
lines3d(x=c(0, 2),y=c(-2, -2),z=c(0, 0),col="darkred") 
lines3d(x=c(0, 2),y=c(-2, -2),z=c(1.5, 1.5),col="darkcyan", lwd=2) 
points3d(x=2,y=-2,z=2,cex=0.5, pch=19, size=10,col="darkcyan")
points3d(x=2,y=-2,z=.5,cex=0.5, pch=19, size=10,col="darkred")
points3d(x=0,y=-2,z=1.5, pch=1,size=20, col="darkcyan")
points3d(x=0,y=-2,z=0,cex=0.5, pch=24, size=20,col="darkred")



#### Hypothesized possibilities for drought/neighbor trade-off ####
library(tidyverse)
#function for gradient?
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
#create gradient
grad <- make_gradient(
  deg = 130, n = 500, cols = c("darkgrey","White","darkgrey")
)
#for Hypothesis B:
set.seed(1998)
x= rnorm(50, 10, 1)
y= -x - rnorm(50,1,2)
fd1=data.frame(x,y, group="Xeric")
x= rnorm(50, 15, 1)
y= -x - rnorm(50,5,2)
fd2=data.frame(x,y, group="Mesic")
x= rnorm(50, 20, 1)
y= -x - rnorm(50,10,2)
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
  annotate(geom="text", x=21, y=-9, label="Darwinian 
Demon", size=5, family="serif")+
  annotate(geom="text", x=9.1, y=-33, label="Competitively 
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
set.seed(2)#11
x= rnorm(50, 10, .5)
y= -x*x*rnorm(50, 9.8, .4)
fd1=data.frame(x,y, group="Xeric")
xx= rnorm(50, 10, .5)
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
  annotate(geom="text", x=10.85,y=-790, label="Darwinian 
   Demon", size=5, family="serif")+
  annotate(geom="text", x=9.1, y=-1200, label="Competitively 
   Excluded", size=5, family="serif")+
  theme(legend.text = element_text(family = "serif"),
        legend.title = element_text(family = "serif"),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        axis.line = element_line(arrow = arrow(type='closed', length = unit(10,'pt'))))

#combine
library(ggpubr)
conceptfig<-ggarrange(A,B,nrow=2, ncol=1, common.legend = T, labels = c("a","b"))
conceptfig<-annotate_figure(conceptfig,left =text_grob("Response to neighbors", size =20, rot=90, family="serif"), bottom=text_grob("Response to drought", size=20, family="serif"))
conceptfig

#export
ggsave(conceptfig, filename = "figures/conceptual_hypotheses.png", dpi=300, height = 7,width =5)



#redoing??
#function for gradient?
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
set.seed(210122)#11
#set.seed(111)
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
