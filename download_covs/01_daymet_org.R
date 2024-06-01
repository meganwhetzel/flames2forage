#########################################X
#----- Chapter 1: Propensity to mig.-----X
#-------- Daymet data organization ------X
#------------------ VAW -----------------X
##------------ April 5, 2022 ------------X
##---------------- 04/18/22 -------------X
#########################################X
# --------- CC, MW, VAW, RBH ------------X
#########################################X
####---- Last edited: 06/17/2022 --------X
#########################################X

# Clean env ----
rm(list = ls())
gc()

# Libraries ----
#install.packages("FedData")
# library(daymetr)
library(ncdf4)
library(raster)
library(dplyr)
library(snow)
library(parallel)

box_dir <- "../../../../Avgar Lab on WILD/"

# Templates ----
#template: UTM
ut <- raster(paste0(box_dir, "UtahEnvironmentalCovariates/raster_template_utm_250m.tif"))
# add in values
values(ut) <- 1:ncell(ut)
plot(ut)

# Load in rasters and grab desired bands
# find bands I want
year1 <- c(seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "days"))

#format as julian day
dat.jul <- as.numeric(format(year1, "%j"))

# directory/file load in 
dir <- paste0(box_dir, "UtahEnvironmentalCovariates/Daymet/daymet/UT_buff/temp/")

f_tmin <- list.files(path = dir, pattern = "tmin_2012_", all.files = F, full.names = T)
f_tmax <- list.files(path = dir, pattern = "tmax_2012_", all.files = F, full.names = T)
f_prcp <- list.files(path = dir, pattern = "prcp_2012_", all.files = F, full.names = T)

# stack raster w/desired bands
cov_data1 <- lapply(f_tmin, stack)
cov_data2 <- lapply(f_tmax, stack)
cov_data3 <- lapply(f_prcp, stack)
#names(cov_data) <-  prcp
summary(cov_data1)

# Project stacks ----
detectCores()

beginCluster(6)
prj_tmin <- lapply(cov_data1, function(r) projectRaster(r, ut, method = 'ngb'))
prj_tmax <- lapply(cov_data2, function(r) projectRaster(r, ut, method = 'ngb'))
prj_prcp <- lapply(cov_data3, function(r) projectRaster(r, ut, method = 'ngb'))
endCluster()


# To save ----
# tmin
outdir1 <- "../daymet_covs/tmin/"
if(!dir.exists(outdir1)){dir.create(outdir1, recursive = T)}
dir.exists(outdir1)
# tmax
outdir2 <- "../HMM/daymet_covs/tmax/"
if(!dir.exists(outdir)){dir.create(outdir)}
dir.exist(outdir)
# prcp
outdir3 <- "../HMM/daymet_covar/prcp/"
if(!dir.exists(outdir)){dir.create(outdir)}
dir.exist(outdir)

writeRaster(cov_data1, paste0(outdir1, "2012_daymet_tmin.tif"), overwrite = T)
writeRaster(cov_data2, paste0(outdir2, "2012_daymet_tmax.tif"), overwrite = T)
writeRaster(cov_data3, paste0(outdir3, "2012_daymet_prcp.tif"), overwrite = T)

warnings()
names(cov_data1)[1:2] <- c("x", "y")
names(cov_data1) <- year1

for(i in 1:length(cov_data1)){
  writeRaster(cov_data1[[i]], paste0(outdir1, "2021"), format = 'GTiff')
}
                           



# OLD CODE -----
# Subset put the desired bands (per month) ----
# BJS comment: Here you are missing parentheses after raster::subset
# But you are also already subsetting with `[[`, so the subset
# function is redundant (but you also didn't provide enough arguments)
# mo <- lapply(cov_data, raster::subset, subset = dat.feb)
# mo2 <- lapply(cov_data, raster::subset, subset = dat.apr)
# mo3 <- lapply(cov_data, raster::subset, subset = dat.jul)
# mo4 <- lapply(cov_data, raster::subset, subset = dat.nov)
# # Or,
# # feb <- lapply(cov_data, function(x) {
# #   return(x[[dat.feb]])
# # })

## TO FIND MEAN ----
# # Now, average for each tile for that month.
# x_mean <- lapply(mo, mean)
# # BJS comment: see that each element of 'feb' had 28 layers
# sapply(mo, nlayers)
# # And now see that each element of 'feb_mean' has 1 layer (which is the
# # average of the 28 days)
# sapply(x_mean, nlayers)
# 
# # BJS comment: now mosaic
# names(x_mean)[1:2] <- c("x", "y")
# x_mean$fun <- mean # Probably doesn't matter
# x_mean$na.rm <- TRUE
# mos <- do.call(mosaic, x_mean)
# 
# # Check
# plot(mos)

# Save ----



