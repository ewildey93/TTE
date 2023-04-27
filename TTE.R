library (spaceNtime)
library(dplyr)
library(data.table)
library(amt)
library(tidyverse)
setwd("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE")

browseVignettes("spaceNtime")
#Need dataframe with capture history that has 3 columns: cam-unique ID for camera,
#datetime  that has date and time of photo and count that has count of study species
#although this only matters for IS not TTE (or STE)
camdf <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/images.csv")
str(camdf)
df <- camdf[camdf$common_name=="Mule Deer",]
df <- df[,c(2,16,17,22)]
colnames(df) <- c("cam","datetime","count","comments")
df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S", tz="America/Denver")
df$cam <- as.factor(df$cam)
#fawns?
df <- filter(df, !(count==1 & comments== "fawn"))

#Need a deploy dataframe with active periods and area
#if camera went offline and came back on need multiple rows *check this
deploy <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/deployments.csv")
deploy <- deploy[,c(2,6,7,10)]
colnames(deploy) <- c("Camera","start","end","featuretype")
DTimes <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/DeploymentTimes.csv")
DeployFeatures <- deploy[,c(1,4)]
#entered BUSH4 as BUSH3 on Wildlife Insights , BUSH3 camera stolen
DeployFeatures$Camera <- replace(x = DeployFeatures$Camera,list=10, values = "BUSH4")
deploy2 <- left_join(DTimes, DeployFeatures, by="Camera")
#need area covered by camera viewshed, 45m2 pulled from Loonam et al.2020 supplemental info
deploy2$area <- 45
deploy2$Start<- as.POSIXct(deploy2$Start,format="%m/%d/%Y %H:%M:%S", tz="America/Denver")
deploy2$End<- as.POSIXct(deploy2$End,format="%m/%d/%Y %H:%M:%S", tz="America/Denver")
str(deploy2)

#define sampling periods and sampling occasions
#figure out movement rate (lps)
Deer2 <- readRDS("C:/Users/eliwi/OneDrive/Documents/R/DeerISSFTWS/DeerList2.rds")
trkList <- lapply(Deer2, function (x) make_track(x, X, Y, DateTime, id = ID, crs = CRS("+init=EPSG:32613")))
stepList <- lapply(trkList, function (x) steps(x))
steps <- rbindlist(stepList,idcol=TRUE)
steps$dt <- as.numeric(steps$dt_)
hist(as.numeric(steps$dt_))
steps[steps$dt < 15]
hist(steps[steps$dt < 15]$sl_)
hist(steps$sl_)
steps15 <- steps[steps$dt < 15,]
steps30 <- steps[steps$dt < 30,]
steps60 <- steps[steps$dt < 60,]
steps15$mvmtrate <- steps15$sl/steps15$dt
steps30$mvmtrate <- steps30$sl/steps30$dt
steps60$mvmtrate <- steps60$sl/steps60$dt
hist(steps15$mvmtrate)
hist(steps30$mvmtrate)
hist(steps60$mvmtrate)
meanspeeds <- c(mean(steps15$mvmtrate),mean(steps30$mvmtrate),mean(steps60$mvmtrate))
medianspeeds <- c(median(steps15$mvmtrate),median(steps30$mvmtrate),median(steps60$mvmtrate))
#sampling period
per <- tte_samp_per(deploy, lps = 6.53/60)
#sampling occasion
study_dates <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-04 23:59:59"), tz = "GMT")
occ <- tte_build_occ(
  per_length = per,
  nper = 24,
  time_btw = 2 * 3600,
  study_start = study_dates[1],
  study_end = study_dates[2]
)
