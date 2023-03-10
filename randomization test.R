#### randomization test ####

#packages
library(tidyverse)

#data
NEWallsite <- read.csv("data/allsite_new.csv") #data
NEWallsite <- NEWallsite %>% #ensure levels are correct
  mutate(grassland_type = fct_relevel(grassland_type,
                                      "Desert", "Southern Shortgrass", "Northern Shortgrass", 
                                      "Northern Mixed", "Southern Mixed", "Tallgrass"))
justdiffs <- NEWallsite[c(23:26)]

#obtain correlation coeficent (R) for my data
cor(justdiffs)
#most important one to test
cor.test(justdiffs$intrinsicdiff, justdiffs$chrdiff) #R = -0.57
intchr.R <- cor(justdiffs$intrinsicdiff, justdiffs$chrdiff) #make a variable

#create simulated random variables and determine their R (repeat >1000 times)
#keep X and permute Y values 1000 times

#determine the sampling distribution of R using random permutations

#plot a histogram

#determine the number of the randomizations that exceed my R 

#Find probability 





# example test on a correlation coefficient.
Score <- c(58,  48,  48,  41,  34,  43,  38,  53,  41,  60,  55,  44,  43, 49,  47,  33,  47,  40,  46,  53,  40,  45,  39,  47,  50,  53,  46,  53)
SAT <- c(590, 590, 580, 490, 550, 580, 550, 700, 560, 690, 800, 600, 650, 580, 660, 590, 600, 540, 610, 580, 620, 600, 560, 560, 570, 630, 510, 620)
r.obt <- cor(Score, SAT)
cat("The obtained correlation is ",r.obt,'\n')
nreps <- 5000
r.random <- numeric(nreps)
for (i in 1:nreps) {
  Y <- Score
  X <- sample(SAT, 28, replace = FALSE)
  r.random[i] <- cor(X,Y)
}
prob <- length(r.random[r.random >= r.obt])/nreps
cat("Probability randomized r >= r.obt",prob)
hist(r.random, breaks = 50, main =  expression(paste("Distribution around ",rho, "= 0")), xlab = "r from randomized samples")
r.obt <- round(r.obt, digits = 2)
legend(.40, 200, r.obt, bty = "n")
arrows(.5,150,.53, 10)


# fill in example for main finding
rintA <-  NEWallsite$intrinsicLDGRcon
rintD <-  NEWallsite$intrinsicLDGRchr
rinvA <-  NEWallsite$invasionLDGRcon
rinvD <-  NEWallsite$invasionLDGRchr

intdif <- rintD - rintA #response to drought without neighbors
invdif <- rinvD - rinvA #response to drought with neighbors
condif <- rinvA - rintA #response to neighbors in ambient
chrdif <- rinvD - rintD

r.obt <- cor(chrdif, intdif)
cat("The obtained correlation is ",r.obt,'\n')
nreps <- 5000
r.random <- numeric(nreps)
for (i in 1:nreps) {
  Y <- intdif
  X <- sample(chrdif, replace = FALSE)
  r.random[i] <- cor(X,Y)
}

#Pvalue is the probability of finding a more extreme (more negative) correlation using randomized data
prob <- length(r.random[r.random <= r.obt])/nreps 
cat("Probability randomized r <= r.obt",prob)
par(mfrow=c(1,3))
plot(chrdif~intdif) #model 2
plot(Y,X) #last permuted model
hist(r.random, breaks = 50, main =  expression(paste("Distribution around ",rho, "= 0")), xlab = "r from randomized samples")
r.obt <- round(r.obt, digits = 2)
abline(v=r.obt,col="blue", lwd=3)




#with growth rates
rintA <-  NEWallsite$intrinsicLDGRcon
rintD <-  NEWallsite$intrinsicLDGRchr
rinvA <-  NEWallsite$invasionLDGRcon
rinvD <-  NEWallsite$invasionLDGRchr

intdif <- rintD - rintA #response to drought without neighbors
invdif <- rinvD - rinvA #response to drought with neighbors
condif <- rinvA - rintA #response to neighbors in ambient
chrdif <- rinvD - rintD

r.obt <- cor(intdif, chrdif)

cat("The obtained correlation is ",r.obt,'\n')
nreps <- 5000
r.random <- numeric(nreps)
for (i in 1:nreps) {
  perm.rintD <- sample(rintD, replace = FALSE)
  Y <- rinvD - perm.rintD #permuted chrdif
  X <- perm.rintD - rintA
  r.random[i] <- cor(X,Y)
}

#Pvalue is the probability of finding a more extreme (more negative) correlation using randomized data
prob <- length(r.random[r.random <= r.obt])/nreps 
cat("Probability randomized r <= r.obt",prob)

par(mfrow=c(1,3))
plot(chrdif~intdif) #model 2
plot(Y,X) #last permuted model
hist(r.random, breaks = 50, main =  expression(paste("Distribution around ",rho, "= 0")), xlab = "r from randomized samples")
r.obt <- round(r.obt, digits = 2)
abline(v=r.obt,col="blue", lwd=3)




##attempt for other finding
# fill in example for main finding
rintA <-  NEWallsite$intrinsicLDGRcon
rintD <-  NEWallsite$intrinsicLDGRchr
rinvA <-  NEWallsite$invasionLDGRcon
rinvD <-  NEWallsite$invasionLDGRchr

intdif <- rintD - rintA #response to drought without neighbors
invdif <- rinvD - rinvA #response to drought with neighbors
condif <- rinvA - rintA #response to neighbors in ambient
chrdif <- rinvD - rintD

r.obt <- cor(condif, intdif)
cat("The obtained correlation is ",r.obt,'\n')
nreps <- 5000
r.random <- numeric(nreps)
for (i in 1:nreps) {
  Y <- condif
  X <- sample(intdif, replace = FALSE)
  r.random[i] <- cor(X,Y)
}

#Pvalue is the probability of finding a more extreme (more negative) correlation using randomized data
prob <- length(r.random[r.random >= r.obt])/nreps 
cat("Probability randomized r >= r.obt",prob)
par(mfrow=c(1,3))
plot(condif~intdif) #model 2
plot(Y,X) #last permuted model
hist(r.random, breaks = 50, main =  expression(paste("Distribution around ",rho, "= 0")), xlab = "r from randomized samples")
r.obt <- round(r.obt, digits = 2)
abline(v=r.obt,col="blue", lwd=3)

#with growth rates
rintA <-  NEWallsite$intrinsicLDGRcon
rintD <-  NEWallsite$intrinsicLDGRchr
rinvA <-  NEWallsite$invasionLDGRcon
rinvD <-  NEWallsite$invasionLDGRchr

intdif <- rintD - rintA #response to drought without neighbors
condif <- rinvA - rintA #response to neighbors in ambient

r.obt <- cor(intdif, condif)

cat("The obtained correlation is ",r.obt,'\n')
nreps <- 999
r.random <- numeric(nreps)
for (i in 1:nreps) {
  perm.rintA <- sample(rintA, replace = FALSE)
  Y <- rinvA - perm.rintA #permuted condif
  X <- rintD - perm.rintA
  r.random[i] <- cor(X,Y)
}

#Pvalue is the probability of finding a more extreme (more positive) correlation using randomized data
prob <- length(r.random[r.random >= r.obt])/nreps 
cat("Probability randomized r >= r.obt",prob)

par(mfrow=c(1,3))
plot(condif~intdif) #model 2
plot(Y,X) #last permuted model
hist(r.random, breaks = 50, main =  expression(paste("Distribution around ",rho, "= 0")), xlab = "r from randomized samples")
r.obt <- round(r.obt, digits = 2)
abline(v=r.obt,col="blue", lwd=3)
