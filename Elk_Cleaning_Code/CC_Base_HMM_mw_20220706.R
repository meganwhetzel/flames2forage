### Creating base HMM for deer ###

### Load Packages ---

library(tidyverse)
library("momentuHMM")
library(fitdistrplus)
library(circular)

### Load Data ---

elk <- readRDS("C:/Users/A02362279/Box/MeganResearch/MSc work/Elk_Cleaning_new/elk_gps_cleaning/20220617_clean.rds") %>% 
  as_tibble() 

# Subsetting Data for testing

test <- subset(elk, animal_id == "EL11F0001")

clean.out <- test %>%
  filter(is.na(test$x) == FALSE)

clean.test <- prepData(data = clean.out) 

clean.test<- clean.test %>%
  filter(is.na(clean.test$step) == FALSE & is.na(clean.test$angle) == FALSE)

test.2 <- subset(out, ID == "EL19F0079")

clean.out.2 <- test.2 %>%
  filter(is.na(test.2$x) == FALSE)

clean.test.2 <- prepData(data = clean.out.2) 

clean.test.2 <- clean.test.2 %>%
  filter(is.na(clean.test.2$step) == FALSE & is.na(clean.test.2$angle) == FALSE)

saveRDS(clean.test, "C:/Users/Avgar Lab/Box/My_Code/Cleaning_GPS_Data/clean_test_data_20220629.rds")

# Cleaning full data
unique(out$ID)
dat.hmm <- dat %>%
  dplyr::select(ID, dt, x, y) %>%
  rename(Time.name = dt)


clean.out <- crawlWrap(obsData = dat.hmm, timeStep = "1 hour", coord = c("x", "y"), Time.name = "Time.name")

# Estimating distributions
#clean.test$step = clean.test$step/10000
fit <- fitdist(clean.test$step, distr = "gamma", method = "mme")
summary(fit)
mean(clean.test$step)

sd(clean.test$step)/mean(clean.test$step)

sc = var(clean.test$step)/mean(clean.test$step)
sh = mean(clean.test$step)/sc

fit.vm <- mle.vonmises(clean.test$angle)
fit.vm$kappa
hist(clean.test$angle)
k <- mean(cos(clean.test$angle))
######
fit.2 <- fitdist(clean.test.2$step, distr = "gamma", method = "mme")
summary(fit.2)
mean(clean.test.2$step)

sd(clean.test.2$step)/mean(clean.test.2$step)

sc.2 = var(clean.test.2$step)/mean(clean.test.2$step)
sh.2 = mean(clean.test.2$step)/sc

fit.vm.2 <- mle.vonmises(clean.test.2$angle)
fit.vm.2$kappa
k.2 <- mean(cos(clean.test.2$angle))

# Fit HMM
1.6*20
test.HMM <- fitHMM(clean.test, nbStates = 3, 
                   dist = list(step='gamma', angle = 'vm'), 
                   formula = ~1, 
                   Par0 = list(step=c(20,100,500,28,140,700),
                               angle=c(0.01,0.2, 1.0)),
                   stateNames =  c("resting", "foraging", "commuting"))

plot(test.HMM)
plot.momentuHMM(test.HMM)
plotSat(test.HMM)

test.HMM.2 <- fitHMM(clean.test.2, nbStates = 3, 
                   dist = list(step='gamma', angle = 'vm'), 
                   formula = ~1, 
                   Par0 = list(step=c(20,100,500,32,160,800),
                               angle=c(0.0001,0.1, 0.5)),
                   stateNames =  c("resting", "foraging", "commuting"))

plot(test.HMM.2)
plot.momentuHMM(test.HMM.2)
plotSat(test.HMM.2)
plotStates(test.HMM.2)

#step: means 20, 100, 500 / sds = multiply same cov by means / zmass: see if you can run it without or leave it as zero (step only)
#angle: means = 0 / 0. 0.1, 0.5
#avg of cos of turn angles for von mis = concentration, also do this mme
