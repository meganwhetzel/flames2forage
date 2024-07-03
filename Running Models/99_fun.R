# Functions

# Prepare data

data_prep <- function(dat) {
  
  # do not trust classifications with no data
  # dat$foraging[dat$step < 5] <- 0
  # dat$foraging[is.na(dat$angle)] <- NA
  
  # shift day-time time-of-day variables to negative 
  dat$is.day <- (dat$dt > dat$dawn_time) & (dat$dt < dat$dusk_time)
  dat$log_dawn[dat$is.day] <- -1 * dat$log_dawn[dat$is.day]
  dat$log_dusk[dat$is.day] <- -1 * dat$log_dusk[dat$is.day]
  
  # extract the day of the year to be used in a seasonal spline 
  dat$julian.day <- yday(dat$dt) 
  
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
  dat$NDVI.delta <- (dat$NDVI.delta - min(dat$NDVI.delta, na.rm = T)) /
    (max(dat$NDVI.delta, na.rm = T) - min(dat$NDVI.delta, na.rm = T))
  
  # add the previous step's probability of foraging
  # Tal's code
  dat$foraging.prior <- NA
  for (ID in unique(dat$animalid)) {
    subset <- which(dat$animalid == ID)
    if (length(subset) > 1) {
      ind.dat <- dat[subset, ]
      ind.dat <- ind.dat[order(ind.dat$dt), ]
      foraging.prior <- c(NA, ind.dat$foraging[1:(nrow(ind.dat) - 1)])
      # Seems to mark the wrong step as NA (off by 1 position)
      foraging.prior[diff(ind.dat$dt) != 2] <- NA
      dat$foraging.prior[subset] <- foraging.prior
    }
  }
  
  # Brian's version
  # Seems to correctly assign NA when burst switches
  dat <- dat %>% 
    group_by(animalid, burst) %>% 
    mutate(foraging_prior = lag(foraging))
  
  dat$cover.herbaceous <- (dat$cover_herbaceous - min(dat$cover_herbaceous)) /
    (max(dat$cover_herbaceous) - min(dat$cover_herbaceous))
  dat$cover.tree <- (dat$cover_tree - min(dat$cover_tree)) /
    (max(dat$cover_tree) - min(dat$cover_tree))
  dat$cover.shrub <- (dat$cover_shrub - min(dat$cover_shrub)) /
    (max(dat$cover_shrub) - min(dat$cover_shrub))
  
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
  
  # Go back to regular tibble
  dat <- as.data.frame(dat)
  
  return(dat)
}

intxn <- function(dat) {
  dat %>% 
    mutate(cover.herbaceous_NDVI.norm.curr = cover.herbaceous * NDVI.norm.curr,
           cover.herbaceous_NDVI.delta = cover.herbaceous * NDVI.delta, 
           cover.shrub_NDVI.norm.curr = cover.shrub * NDVI.norm.curr,
           cover.shrub_NDVI.delta = cover.shrub * NDVI.delta,
           cover.tree_NDVI.norm.curr = cover.tree * NDVI.norm.curr,
           cover.tree_NDVI.delta = cover.tree * NDVI.delta) %>% 
    return()
}

scale_data <- function(dat, sc_dat) {
  # Loop through all the variables in sc_dat
  for (var in sc_dat$name){
    if (!is.null(dat[[var]])) {
      # Get mean for that variable
      mn <- sc_dat[which(sc_dat$name == var), "mean"]
      sd <- sc_dat[which(sc_dat$name == var), "sd"]
      dat[[var]] <- (dat[[var]] - mn)/sd
    }
  }
  return(dat)
}


# Create prediction data
# Give all arguments defaults so that you can change what you want
pred_data <- function(scale_df, 
                      foraging_prior = scale_df["foraging_prior", "mean"],
                      snd = scale_df["snd", "mean"],
                      burn_type = "low",
                      veg_type = "aspen",
                      daydiff.log = scale_df["daydiff.log", "mean"],
                      log_dawn = log(6),
                      log_dusk = log(6),
                      julian.day = 115,
                 #     cover.herbaceous_NDVI.norm.curr = scale_df["cover.herbaceous_NDVI.norm.curr", "mean"] ,
                 #     cover.herbaceous_NDVI.delta = scale_df["cover.herbaceous_NDVI.delta", "mean"],
                 #     cover.shrub_NDVI.norm.curr = scale_df["cover.shrub_NDVI.norm.curr", "mean"],
                 #     cover.shrub_NDVI.delta = scale_df["cover.shrub_NDVI.delta", "mean"],
                 #     cover.tree_NDVI.norm.curr = NA,
                 #     cover.tree_NDVI.delta =NA ,
                      cover.herbaceous = scale_df["cover.herbaceous", "mean"],
                      cover.shrub = scale_df["cover.shrub", "mean"],
                      cover.tree = scale_df["cover.tree", "mean"], 
                      NDVI.delta = scale_df["NDVI.delta", "mean"],
                      NDVI.norm.curr = scale_df["NDVI.norm.curr", "mean"],
                      animalid = "EL13F0002", 
                      Evnt_ID = "UT4089410939519880723") {
   res <- expand.grid(foraging_prior = foraging_prior,
                     snd = snd,
                     burn_type = burn_type,
                     veg_type = veg_type,
                     daydiff.log = daydiff.log,
                     log_dawn = log_dawn,
                     log_dusk = log_dusk,
                     julian.day = julian.day,
                     cover.herbaceous = cover.herbaceous,
                 #    cover.herbaceous_NDVI.norm.curr = cover.herbaceous_NDVI.norm.curr,
                 #    cover.herbaceous_NDVI.delta = cover.herbaceous_NDVI.delta,
                 #    cover.shrub_NDVI.norm.curr = cover.shrub_NDVI.norm.curr,
                 #    cover.shrub_NDVI.delta = cover.shrub_NDVI.delta,
                 #    cover.tree_NDVI.norm.curr = NA,
                 #    cover.tree_NDVI.delta = NA,
                     cover.shrub = cover.shrub,
                     cover.tree = cover.tree,
                     NDVI.delta = NDVI.delta,
                     NDVI.norm.curr = NDVI.norm.curr,
                     animalid = animalid,
                     Evnt_ID = Evnt_ID)
  return(res)
}

# Convert veg_type and burn_type to dummy variables
dummy_vars <- function(dat) {
  # Vegetation type
  dat$is.aspen <- as.numeric(dat$veg_type == "aspen")
  dat$is.conifer <- as.numeric(dat$veg_type == "conifer")
  dat$is.juniper <- as.numeric(dat$veg_type == "juniper")
  dat$is.other <- as.numeric(dat$veg_type == "other")
  # Burn severity
  dat$burned.1 <- as.numeric((dat$burn_type == "l"))
  dat$burned.2 <- as.numeric(dat$burn_type == "m")
  dat$burned.3 <- as.numeric(dat$burn_type == "h")
  dat$unburned.in <- as.numeric(dat$burn_type == "u")
  dat$unburned.out <- as.numeric(dat$burn_type == "o")
  # Return
  return(dat)
  
}

# Function to convert a Julian Day to a date
jd_2_date <- function(x, year = 2016) {
  # Dummy date
  d <- as.Date(paste(year, "01-01", sep = "-"))
  # Replace the yday
  yday(d) <- x
  # Return
  return(d)
}




