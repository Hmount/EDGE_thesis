#### testing betas

dat <- read.csv("data/alldata_redo.csv") #data
dat <- dat[-109,]

summary(lm(effectND~effectN, dat))

ggplot(dat, aes(x=effectN, y=effectND))+
  geom_point()

dat$source <- as.factor(dat$source)
m1<-lm(effectN~source, dat)
anova(m1)
summary(m1)
ggplot(dat, aes(x=effectN, y=effectND, color=source))+
  geom_point()+
  geom_smooth(method="lm")
