
library(lubridate)
library(mgcv)

summary(dat <- readRDS("complete_fire_data_20230115.rds"))

#### data prep ####

# do not trust classifications with no data
dat$foraging[dat$step < 5] <- 0
dat$for_nf[dat$step < 5] <- 0
dat$foraging[is.na(dat$angle)] <- NA
dat$for_nf[is.na(dat$angle)] <- NA

# shift day-time time-of-day variables to negative 
dat$is.day <- (dat$dt > dat$dawn_time) & (dat$dt < dat$dusk_time)
dat$log_dawn[dat$is.day] <- -1 * dat$log_dawn[dat$is.day]
dat$log_dusk[dat$is.day] <- -1 * dat$log_dusk[dat$is.day]

# extract the day of the year to be used in a seasonal spline 
dat$julian.day <- yday(dat$dt) 

dat$is.aspen <- as.numeric(dat$espgroup == "aspen")
dat$is.conifer <- as.numeric(dat$espgroup == "conifer")
dat$is.juniper <- as.numeric(dat$espgroup == "juniper")
dat$is.other <- as.numeric((dat$is.aspen == 0) & (dat$is.conifer == 0) & (dat$is.juniper == 0))

dat$burned.1 <- as.numeric((dat$elkpn_6 == "2") | (dat$elkpn_6 == "5"))
dat$burned.2 <- as.numeric(dat$elkpn_6 == "3")
dat$burned.3 <- as.numeric(dat$elkpn_6 == "4")
dat$unburned.in <- as.numeric(dat$elkpn_6 == "1")
dat$unburned.out <- as.numeric(dat$elkpn_6 == "0")
dat$daydiff[dat$unburned.out == 1] <- max(dat$daydiff[dat$unburned.out == 0]) # set all time-since-fire values on unburned points max
dat$daydiff.log <- log(dat$daydiff + 1)

# renormlize NDVI
NDVI.min <- min(min(dat$NDVI_Previous, na.rm = T), min(dat$NDVI_Current, na.rm = T), min(dat$NDVI_Subsequent, na.rm = T))
NDVI.max <- max(max(dat$NDVI_Previous, na.rm = T), max(dat$NDVI_Current, na.rm = T), max(dat$NDVI_Subsequent, na.rm = T))
dat$NDVI.norm.prev <- (dat$NDVI_Previous - NDVI.min) / (NDVI.max - NDVI.min)
dat$NDVI.norm.curr <- (dat$NDVI_Current - NDVI.min) / (NDVI.max - NDVI.min)
dat$NDVI.norm.subs <- (dat$NDVI_Subsequent - NDVI.min) / (NDVI.max - NDVI.min)

# remove snow effect on NDVI
dat$snd <- round(dat$snd, digits = 5)
NDVI.min <- min(dat$NDVI.norm.curr[dat$snd == 0], na.rm = T) # minimum NDVI in the absence of snow
dat$NDVI.norm.prev[dat$NDVI.norm.prev < NDVI.min] <- NDVI.min
dat$NDVI.norm.curr[dat$NDVI.norm.curr < NDVI.min] <- NDVI.min
dat$NDVI.norm.subs[dat$NDVI.norm.subs < NDVI.min] <- NDVI.min

# green-up
dat$NDVI.delta <- (dat$NDVI.norm.subs - dat$NDVI.norm.prev) / dat$NDVI.norm.curr
dat$NDVI.delta <- ((dat$NDVI.delta - min(dat$NDVI.delta, na.rm = T)) /
                     (max(dat$NDVI.delta, na.rm = T) - min(dat$NDVI.delta, na.rm = T)))

# add the previous step's probability of foraging
dat$foraging.prior <- NA
for (ID in unique(dat$animalid)) {
  subset <- which(dat$animalid == ID)
  if (length(subset) > 1) {
    ind.dat <- dat[subset, ]
    ind.dat <- ind.dat[order(ind.dat$dt), ]
    foraging.prior <- c(NA, ind.dat$foraging[1:(nrow(ind.dat) - 1)])
    foraging.prior[diff(ind.dat$dt) != 2] <- NA
    dat$foraging.prior[subset] <- foraging.prior
  }
}

# filter and scale
dat <- dat[!is.na(dat$foraging.prior) &
             !is.na(dat$foraging) &
             !is.na(dat$NDVI.norm.curr) &
             !is.na(dat$NDVI.delta), ]
dat$foraging.prior.scl <- scale(dat$foraging.prior)
dat$snd.scl <- scale(dat$snd) 
dat$NDVI.scl <- scale(dat$NDVI.norm.curr)
dat$NDVI.delta.scl <- scale(dat$NDVI.delta)

dat$cover.herbaceous <- (dat$cover_herbaceous - min(dat$cover_herbaceous)) /
  (max(dat$cover_herbaceous) - min(dat$cover_herbaceous))
dat$cover.tree <- (dat$cover_tree - min(dat$cover_tree)) /
  (max(dat$cover_tree) - min(dat$cover_tree))
dat$cover.shrub <- (dat$cover_shrub - min(dat$cover_shrub)) /
  (max(dat$cover_shrub) - min(dat$cover_shrub))


#### null model ####

gam.null.TA <- gam(for_nf ~ 0 + 
                           # intercepts
                           is.other +
                           is.juniper +
                           is.conifer +
                           is.aspen +
                           # an autoregressive term
                           foraging.prior.scl + 
                           # the effect of snow depth
                           snd.scl + 
                           # effects of cover
                           is.other:cover.tree +
                           is.juniper:cover.tree +
                           is.conifer:cover.tree +
                           is.aspen:cover.tree +
                           # effects of greenness
                           cover.herbaceous:NDVI.scl +
                           cover.herbaceous:NDVI.delta.scl +
                           cover.shrub:NDVI.scl +
                           cover.shrub:NDVI.delta.scl +
                           is.aspen:cover.tree:NDVI.scl +
                           is.aspen:cover.tree:NDVI.delta.scl + 
                           # splines
                           s(log_dawn, log_dusk) + # a 2D spline accounting for time of day 
                           s(julian.day, bs = "cc") + # a circular spline accounting for time of year
                           s(animalid, bs = "re") + # a random spline accounting for the random effect of individual ID
                           s(Evnt_ID, bs = "re"), # a random spline accounting for the random effect of fire ID
                         family = binomial, data = dat)
plot(glam.null)
MuMIn::AICc(glam.null)


# summary(glam.null.2 <- gam(for_nf ~ 0 + 
#                              # intercepts
#                              is.other +
#                              is.juniper +
#                              is.conifer +
#                              is.aspen +
#                              # an autoregressive term
#                              foraging.prior.scl + 
#                              # the effect of snow depth
#                              snd.scl + 
#                              # effects of cover
#                              is.other:cover.tree +
#                              is.juniper:cover.tree +
#                              is.conifer:cover.tree +
#                              is.aspen:cover.tree +
#                              # effects of greenness
#                              cover.herbaceous:NDVI.scl +
#                              cover.herbaceous:NDVI.delta.scl +
#                              cover.shrub:NDVI.scl +
#                              cover.shrub:NDVI.delta.scl +
#                              is.aspen:cover.tree:NDVI.scl +
#                              is.aspen:cover.tree:NDVI.delta.scl + 
#                              # splines
#                              s(log_dawn, log_dusk) + # a 2D spline accounting for time of day 
#                              s(julian.day, bs = "cc"), # + # a circular spline accounting for time of year
#                            # s(animalid, bs = "re") + # a random spline accounting for the random effect of individual ID
#                            # s(Evnt_ID, bs = "re"), # a random spline accounting for the random effect of fire ID
#                            family = binomial, data = dat, weights = foraging / mean(foraging)))
# plot(glam.null.2)
# MuMIn::AICc(glam.null.2)
# 
# 
# summary(glam.null.3 <- gam(for_nf ~ 0 + 
#                              # intercepts
#                              is.other +
#                              is.juniper +
#                              is.conifer +
#                              is.aspen +
#                              # an autoregressive term
#                              foraging.prior.scl + 
#                              # the effect of snow depth
#                              snd.scl + 
#                              # effects of cover
#                              is.other:cover.tree +
#                              is.juniper:cover.tree +
#                              is.conifer:cover.tree +
#                              is.aspen:cover.tree +
#                              # effects of greenness
#                              cover.herbaceous:NDVI.scl +
#                              cover.herbaceous:NDVI.delta.scl +
#                              cover.shrub:NDVI.scl +
#                              cover.shrub:NDVI.delta.scl +
#                              is.aspen:cover.tree:NDVI.scl +
#                              is.aspen:cover.tree:NDVI.delta.scl + 
#                              # splines
#                              s(log_dawn, log_dusk) + # a 2D spline accounting for time of day 
#                              s(julian.day, bs = "cc"), # + # a circular spline accounting for time of year
#                            # s(animalid, bs = "re") + # a random spline accounting for the random effect of individual ID
#                            # s(Evnt_ID, bs = "re"), # a random spline accounting for the random effect of fire ID
#                            family = binomial, data = dat))
# plot(glam.null.3)
# MuMIn::AICc(glam.null.3)


#### plotting null-model results ####

newdat <- dat[order(dat$cover.tree), ]
newdat$log_dawn <- mean(dat$log_dawn)  
newdat$log_dusk <- mean(dat$log_dusk)  
newdat$julian.day <- mean(dat$julian.day)  
newdat$snd.scl[] <- 0  
newdat$foraging.prior.scl[] <- 0 
newdat$cover.herbaceous <- mean(dat$cover.herbaceous)
newdat$cover.shrub <- mean(dat$cover.shrub)
newdat$NDVI.delta.scl[] <- 0
newdat$NDVI.scl[] <- 0

# To fix the random effects I just used here the animal and the fire with
# the smallest random effect based on sort(abs(coef(glam.fire))).
# If possible, this should be modified to something more elegant.
newdat$animalid[] <- unique(dat$animalid)[332]
newdat$Evnt_ID[] <- unique(dat$Evnt_ID)[14]

predictions <- predict(gam_null, newdata = newdat,
                       type = "link", se.fit = TRUE)
newdat$pred.ml <- plogis(predictions$fit)
newdat$pred.lwr <- plogis(predictions$fit -
                            (1.96 * predictions$se))
newdat$pred.upr <- plogis(predictions$fit +
                            (1.96 * predictions$se))

subset <- which(newdat$is.aspen == 1)
plot(newdat$cover.tree[subset] * 100,
     newdat$pred.ml[subset],
     type = "l", lwd = 2,
     xlim = c(0, 100),
     ylim = c(0.2, 0.6),
     xlab = "Tree Cover [%]",
     ylab = "Probability of Foraging")
lines(newdat$cover.tree[subset] * 100,
      newdat$pred.lwr[subset], lty = 2)
lines(newdat$cover.tree[subset] * 100,
      newdat$pred.upr[subset], lty = 2)

subset <- which(newdat$is.other == 1)
plot(newdat$cover.tree[subset] * 100,
     newdat$pred.ml[subset],
     type = "l", lwd = 2,
     xlim = c(0, 100),
     ylim = c(0.2, 0.6),
     xlab = "Tree Cover [%]",
     ylab = "Probability of Foraging")
lines(newdat$cover.tree[subset] * 100,
      newdat$pred.lwr[subset], lty = 2)
lines(newdat$cover.tree[subset] * 100,
      newdat$pred.upr[subset], lty = 2)


#### fire model ####

gam.fire.TA <- gam(for_nf ~ 0 + 
                           # intercepts
                           unburned.out:is.other +
                           unburned.out:is.juniper +
                           unburned.out:is.conifer +
                           unburned.out:is.aspen +
                           unburned.in:is.other +
                           unburned.in:is.juniper +
                           unburned.in:is.conifer +
                           unburned.in:is.aspen +
                           burned.1:is.other +
                           burned.1:is.juniper +
                           burned.1:is.conifer +
                           burned.1:is.aspen +
                           burned.2:is.other +
                           burned.2:is.juniper +
                           burned.2:is.conifer +
                           burned.2:is.aspen +
                           burned.3:is.other +
                           burned.3:is.juniper +
                           burned.3:is.conifer +
                           burned.3:is.aspen +
                           # an autoregressive term
                           foraging.prior.scl + 
                           # the effect of snow depth
                           snd.scl +
                           # effects of time since fire
                           unburned.in:is.other:daydiff.log +
                           unburned.in:is.juniper:daydiff.log +
                           unburned.in:is.conifer:daydiff.log +
                           unburned.in:is.aspen:daydiff.log +
                           burned.1:is.other:daydiff.log +
                           burned.1:is.juniper:daydiff.log +
                           burned.1:is.conifer:daydiff.log +
                           burned.1:is.aspen:daydiff.log +
                           burned.2:is.other:daydiff.log +
                           burned.2:is.juniper:daydiff.log +
                           burned.2:is.conifer:daydiff.log +
                           burned.2:is.aspen:daydiff.log +
                           burned.3:is.other:daydiff.log +
                           burned.3:is.juniper:daydiff.log +
                           burned.3:is.conifer:daydiff.log +
                           burned.3:is.aspen:daydiff.log +
                           unburned.in:is.other:I(daydiff.log ^ 2) +
                           unburned.in:is.juniper:I(daydiff.log ^ 2) +
                           unburned.in:is.conifer:I(daydiff.log ^ 2) +
                           unburned.in:is.aspen:I(daydiff.log ^ 2) +
                           burned.1:is.other:I(daydiff.log ^ 2) +
                           burned.1:is.juniper:I(daydiff.log ^ 2) +
                           burned.1:is.conifer:I(daydiff.log ^ 2) +
                           burned.1:is.aspen:I(daydiff.log ^ 2) +
                           burned.2:is.other:I(daydiff.log ^ 2) +
                           burned.2:is.juniper:I(daydiff.log ^ 2) +
                           burned.2:is.conifer:I(daydiff.log ^ 2) +
                           burned.2:is.aspen:I(daydiff.log ^ 2) +
                           burned.3:is.other:I(daydiff.log ^ 2) +
                           burned.3:is.juniper:I(daydiff.log ^ 2) +
                           burned.3:is.conifer:I(daydiff.log ^ 2) +
                           burned.3:is.aspen:I(daydiff.log ^ 2) +
                           s(log_dawn, log_dusk) + # a 2D spline accounting for time of day 
                           s(julian.day, bs = "cc") + # a circular spline accounting for time of year
                           s(animalid, bs = "re") + # a random spline accounting for the random effect of individual ID
                           s(Evnt_ID, bs = "re"), # a random spline accounting for the random effect of fire ID
                         family = binomial, data = dat)
plot(gam.fire.TA)

MuMIn::AICc(glam.fire)


#### plotting fire-model results ####

newdat <- dat[order(dat$daydiff), ]
newdat$log_dawn <- mean(dat$log_dawn)  
newdat$log_dusk <- mean(dat$log_dusk)  
newdat$julian.day <- mean(dat$julian.day)  
newdat$snd.scl[] <- 0  
newdat$foraging.prior.scl[] <- 0

# To fix the random effects I just used here the animal and the fire with
# the smallest random effect based on sort(abs(coef(glam.fire))).
# If possible, this should be modified to something more elegant.
newdat$animalid[] <- unique(dat$animalid)[332]
newdat$Evnt_ID[] <- unique(dat$Evnt_ID)[14]

predictions <- predict(glam.fire, newdata = newdat,
                       type = "link", se.fit = TRUE)
newdat$pred.ml <- plogis(predictions$fit)
newdat$pred.lwr <- plogis(predictions$fit -
                            (1.96 * predictions$se))
newdat$pred.upr <- plogis(predictions$fit +
                            (1.96 * predictions$se))

subset <- which((newdat$burned.3 == 1) & (newdat$is.aspen == 1))
plot(newdat$daydiff[subset] / 365.25,
     newdat$pred.ml[subset],
     type = "l", lwd = 2,
     # xlim = c(0, 35),
     log = "x",
     ylim = c(0.2, 0.6),
     xlab = "Years since fire",
     ylab = "Probability of Foraging")
lines(newdat$daydiff[subset] / 365.25,
      newdat$pred.lwr[subset], lty = 2)
lines(newdat$daydiff[subset] / 365.25,
      newdat$pred.upr[subset], lty = 2)


subset <- which((newdat$unburned.in == 1) & (newdat$is.juniper == 1))
plot(newdat$daydiff[subset] / 365.25,
     newdat$pred.ml[subset],
     type = "l", lwd = 2,
     # xlim = c(0, 35),
     log = "x",
     ylim = c(0.2, 0.6),
     xlab = "Years since fire",
     ylab = "Probability of Foraging")
lines(newdat$daydiff[subset] / 365.25,
      newdat$pred.lwr[subset], lty = 2)
lines(newdat$daydiff[subset] / 365.25,
      newdat$pred.upr[subset], lty = 2)




#### combined model ####

gam.combo <- gam(foraging ~ 0 + 
                               # intercepts
                               unburned.out:is.other +
                               unburned.out:is.juniper +
                               unburned.out:is.conifer +
                               unburned.out:is.aspen +
                               unburned.in:is.other +
                               unburned.in:is.juniper +
                               unburned.in:is.conifer +
                               unburned.in:is.aspen +
                               burned.1:is.other +
                               burned.1:is.juniper +
                               burned.1:is.conifer +
                               burned.1:is.aspen +
                               burned.2:is.other +
                               burned.2:is.juniper +
                               burned.2:is.conifer +
                               burned.2:is.aspen +
                               burned.3:is.other +
                               burned.3:is.juniper +
                               burned.3:is.conifer +
                               burned.3:is.aspen +
                               # an autoregressive term
                               foraging.prior.scl + 
                               # the effect of snow depth
                               snd.scl +
                               # effects of cover
                               is.other:cover.tree +
                               is.juniper:cover.tree +
                               is.conifer:cover.tree +
                               is.aspen:cover.tree +
                               # effects of greenness
                               cover.herbaceous:NDVI.scl +
                               cover.herbaceous:NDVI.delta.scl +
                               cover.shrub:NDVI.scl +
                               cover.shrub:NDVI.delta.scl +
                               is.aspen:cover.tree:NDVI.scl +
                               is.aspen:cover.tree:NDVI.delta.scl + 
                               # effects of time since fire
                               unburned.in:is.other:daydiff.log +
                               unburned.in:is.juniper:daydiff.log +
                               unburned.in:is.conifer:daydiff.log +
                               unburned.in:is.aspen:daydiff.log +
                               burned.1:is.other:daydiff.log +
                               burned.1:is.juniper:daydiff.log +
                               burned.1:is.conifer:daydiff.log +
                               burned.1:is.aspen:daydiff.log +
                               burned.2:is.other:daydiff.log +
                               burned.2:is.juniper:daydiff.log +
                               burned.2:is.conifer:daydiff.log +
                               burned.2:is.aspen:daydiff.log +
                               burned.3:is.other:daydiff.log +
                               burned.3:is.juniper:daydiff.log +
                               burned.3:is.conifer:daydiff.log +
                               burned.3:is.aspen:daydiff.log +
                               unburned.in:is.other:I(daydiff.log ^ 2) +
                               unburned.in:is.juniper:I(daydiff.log ^ 2) +
                               unburned.in:is.conifer:I(daydiff.log ^ 2) +
                               unburned.in:is.aspen:I(daydiff.log ^ 2) +
                               burned.1:is.other:I(daydiff.log ^ 2) +
                               burned.1:is.juniper:I(daydiff.log ^ 2) +
                               burned.1:is.conifer:I(daydiff.log ^ 2) +
                               burned.1:is.aspen:I(daydiff.log ^ 2) +
                               burned.2:is.other:I(daydiff.log ^ 2) +
                               burned.2:is.juniper:I(daydiff.log ^ 2) +
                               burned.2:is.conifer:I(daydiff.log ^ 2) +
                               burned.2:is.aspen:I(daydiff.log ^ 2) +
                               burned.3:is.other:I(daydiff.log ^ 2) +
                               burned.3:is.juniper:I(daydiff.log ^ 2) +
                               burned.3:is.conifer:I(daydiff.log ^ 2) +
                               burned.3:is.aspen:I(daydiff.log ^ 2) +
                               s(log_dawn, log_dusk) + # a 2D spline accounting for time of day 
                               s(julian.day, bs = "cc") + # a circular spline accounting for time of year
                               s(animalid, bs = "re") + # a random spline accounting for the random effect of individual ID
                               s(Evnt_ID, bs = "re"), # a random spline accounting for the random effect of fire ID
                             family = binomial, data = dat)
plot(glam.combined)
MuMIn::AICc(glam.combined)


#### plotting combined-model results ####

newdat <- dat[order(dat$daydiff), ]
newdat$log_dawn <- mean(dat$log_dawn)  
newdat$log_dusk <- mean(dat$log_dusk)  
newdat$julian.day <- mean(dat$julian.day)  
newdat$snd.scl[] <- 0  
newdat$foraging.prior.scl[] <- 0
newdat$cover.herbaceous <- mean(dat$cover.herbaceous)
newdat$cover.shrub <- mean(dat$cover.shrub)
newdat$cover.tree <- mean(dat$cover.tree)
newdat$NDVI.delta.scl[] <- 0
newdat$NDVI.scl[] <- 0

# To fix the random effects I just used here the animal and the fire with
# the smallest random effect based on sort(abs(coef(glam.fire))).
# If possible, this should be modified to something more elegant.
newdat$animalid[] <- unique(dat$animalid)[332]
newdat$Evnt_ID[] <- unique(dat$Evnt_ID)[14]

predictions <- predict(glam.combined, newdata = newdat,
                       type = "link", se.fit = TRUE)
newdat$pred.ml <- plogis(predictions$fit)
newdat$pred.lwr <- plogis(predictions$fit -
                            (1.96 * predictions$se))
newdat$pred.upr <- plogis(predictions$fit +
                            (1.96 * predictions$se))

subset <- which((newdat$burned.3 == 1) & (newdat$is.aspen == 1))
plot(newdat$daydiff[subset] / 365.25,
     newdat$pred.ml[subset],
     type = "l", lwd = 2,
     # xlim = c(0, 35),
     log = "x",
     ylim = c(0.2, 0.6),
     xlab = "Years since fire",
     ylab = "Probability of Foraging",
)
lines(newdat$daydiff[subset] / 365.25,
      newdat$pred.lwr[subset], lty = 2)
lines(newdat$daydiff[subset] / 365.25,
      newdat$pred.upr[subset], lty = 2)


subset <- which((newdat$unburned.in == 1) & (newdat$is.juniper == 1))
plot(newdat$daydiff[subset] / 365.25,
     newdat$pred.ml[subset],
     type = "l", lwd = 2,
     # xlim = c(0, 35),
     log = "x",
     ylim = c(0.2, 0.6),
     xlab = "Years since fire",
     ylab = "Probability of Foraging")
lines(newdat$daydiff[subset] / 365.25,
      newdat$pred.lwr[subset], lty = 2)
lines(newdat$daydiff[subset] / 365.25,
      newdat$pred.upr[subset], lty = 2)


