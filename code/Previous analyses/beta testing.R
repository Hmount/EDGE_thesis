#### testing betas

dat <- read.csv("data/alldata_redo.csv") #data
dat <- dat[-109,]

summary(lm(effectND~effectN, dat))
summary(lm(effectND~effectN*source, dat)) #no effect of grassland

ggplot(dat, aes(x=effectN, y=effectND))+
  geom_point()

dat$source <- as.factor(dat$source)
m1<-lm(effectN~source, dat)
anova(m1)
summary(m1)
ggplot(dat, aes(x=effectN, y=effectND, color=source))+
  geom_point()+
  geom_smooth(method="lm")


#trait relationships?
NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
test <- merge(NEWallsite, dat, by = c("species", "source"))

summary(lm(effectND~rootdiam, test))
summary(lm(effectN~leafN, test))

