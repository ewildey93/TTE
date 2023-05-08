library (spaceNtime)
library(dplyr)
library(data.table)
library(amt)
library(tidyverse)
library(fitdistrplus)
library(ggplot2)
library(bquote)
setwd("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE")

browseVignettes("spaceNtime")
#Need dataframe with capture history that has 3 columns: cam-unique ID for camera,
#datetime  that has date and time of photo and count that has count of study species
#although this only matters for IS not TTE (or STE)
camdf <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/images.csv")
str(camdf)
table(camdf$deployment_id)
df <- camdf[camdf$common_name=="Mule Deer",]
df <- df[,c(2,16,17,22)]
colnames(df) <- c("cam","datetime","count","comments")
df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S", tz="America/Denver")
#fawns?
df <- filter(df, !(count==1 & comments== "fawn"))
table(df$cam)
#replace BUSH3 with BUSH4
BUSH3 <- which(df$cam == "BUSH3")
df$cam <- replace(x = df$cam,list=BUSH3, values = "BUSH4")
table(df$cam)
df$cam <- as.factor(df$cam)


#Need a deploy dataframe with active periods and area
#if camera went offline and came back on need multiple rows *check this
deploy <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/deployments.csv")
deploy <- deploy[,c(2,6,7,10)]
colnames(deploy) <- c("Camera","start","end","featuretype")
DTimes <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/DeploymentTimes3.csv")
DeployFeatures <- deploy[,c(1,4)]
#entered BUSH4 as BUSH3 on Wildlife Insights , BUSH3 camera stolen
DeployFeatures$Camera <- replace(x = DeployFeatures$Camera,list=10, values = "BUSH4")
deploy2 <- left_join(DTimes, DeployFeatures, by="Camera")
c <- ((45/3)/360)*pi
deploy2$area <- c*deploy2$L^2 + c*deploy2$C^2 + c*deploy2$R^2
#need area covered by camera viewshed, 45m2 pulled from Loonam et al.2020 supplemental info
Areas <- data.frame(area45=rnorm(45, mean=45, sd=5), area65=rnorm(45, mean=65, sd=5), area80=rnorm(45, mean=80, sd=5))
#or
deploy2$area <- 45
#or
deploy2$area <- c*deploy2$L^2 + c*deploy2$C^2 + c*deploy2$R^2
deploy2$Start<- as.POSIXct(deploy2$Start,format="%m/%d/%Y %H:%M:%S", tz="America/Denver")
deploy2$End<- as.POSIXct(deploy2$End,format="%m/%d/%Y %H:%M:%S", tz="America/Denver")
str(deploy2)
hist(deploy2$Start, breaks = 30)
#define sampling periods and sampling occasions
#figure out movement rate (lps)
Deer2 <- readRDS("C:/Users/eliwi/OneDrive/Documents/R/DeerISSFTWS/DeerList2.rds")
trkList <- lapply(Deer2, function (x) make_track(x, X, Y, DateTime, id = ID, crs = CRS("+init=EPSG:32613")))
stepList <- lapply(trkList, function (x) steps(x))
steps <- rbindlist(stepList,idcol=TRUE)
steps$dt <- as.numeric(steps$dt_)
hist(steps$dt[steps$dt < 9000], breaks=100)
hist(as.numeric(steps$dt_))
steps[steps$dt < 15]
hist(steps[steps$dt < 15]$sl_)
hist(steps$sl_)
steps15 <- steps[steps$dt < 15,]
steps30 <- steps[steps$dt < 30,]
steps60 <- steps[steps$dt < 60,]
steps130 <- steps[steps$dt < 130,]
steps$mvmtrate <- steps$sl_/steps$dt
steps15$mvmtrate <- steps15$sl/steps15$dt
steps30$mvmtrate <- steps30$sl/steps30$dt
steps60$mvmtrate <- steps60$sl/steps60$dt
steps130$mvmtrate <- steps130$sl/steps130$dt
hist(steps15$mvmtrate)
hist(steps30$mvmtrate)
hist(steps60$mvmtrate)
meanspeeds <- c(mean(steps15$mvmtrate),mean(steps30$mvmtrate),mean(steps60$mvmtrate), mean(steps130$mvmtrate),mean(steps$mvmtrate))
medianspeeds <- c(median(steps15$mvmtrate),median(steps30$mvmtrate),median(steps60$mvmtrate), median(steps130$mvmtrate),median(steps$mvmtrate))

#for loop for different speeds
#sampling period
speeds <- c(0.681/60, 1.76/60, 207.91/3600)
DiffSpeedsN <- data.frame(Camera=as.character, N=as.numeric(), SE=as.numeric(), LCI=as.numeric(), UCI=as.numeric(), speed=as.numeric())
for (i in 1:length(speeds)) {
deploy2$area <- 45
per <- tte_samp_per(deploy2, lps = speeds[i])
#sampling occasion
study_dates <- as.POSIXct(c("2022-04-15 00:00:00", "2022-05-15 23:59:59"), tz = "America/Denver")
occ <- tte_build_occ(
  per_length = per,
  nper = 5,
  time_btw = 2 * 3600,
  study_start = study_dates[1],
  study_end = study_dates[2]
)

#build encounter history
colnames(deploy2)[c(1:3)] <- c("cam", "start", "end")
deploy2$cam <- as.factor(deploy2$cam)
tte_eh <- tte_build_eh(df=df, deploy=deploy2, occ=occ,  samp_per=per)
#str(tte_eh)
#estimate abundance
#study area size must be in the same units as camera area in sampling effort
#each grid cell 152909.665m^2 *36 grid cells=5504748 or 
N <- tte_estN_fn(tte_eh, study_area = 5.504748e6)
N$speed <- speeds[i]
DiffSpeedsN <- rbind(DiffSpeedsN, N)
}
DiffSpeedsN$D <- DiffSpeedsN$N/5.504748
DiffSpeedsN$DSE <- DiffSpeedsN$SE/5.504748
DiffSpeedsN$DLCI <- DiffSpeedsN$LCI/5.504748
DiffSpeedsN$DUCI <- DiffSpeedsN$UCI/5.504748
saveRDS(DiffSpeedsN, "./DiffSpeedsN.rds")

#for loop for different areas
DiffAreaN <- data.frame(Camera=as.character, N=as.numeric(), SE=as.numeric(), LCI=as.numeric(), UCI=as.numeric())
for (i in 1:length(colnames(Areas))) {
  deploy2$area <- Areas[,i]
  per <- tte_samp_per(deploy2, lps = 1.76/60)
  #sampling occasion
  study_dates <- as.POSIXct(c("2022-04-15 00:00:00", "2022-05-15 23:59:59"), tz = "America/Denver")
  occ <- tte_build_occ(
    per_length = per,
    nper = 5,
    time_btw = 2 * 3600,
    study_start = study_dates[1],
    study_end = study_dates[2]
  )
  
  #build encounter history
  colnames(deploy2)[c(1:3)] <- c("cam", "start", "end")
  deploy2$cam <- as.factor(deploy2$cam)
  tte_eh <- tte_build_eh(df=df, deploy=deploy2, occ=occ,  samp_per=per)
  #str(tte_eh)
  #estimate abundance
  #study area size must be in the same units as camera area in sampling effort
  #each grid cell 152909.665m^2 *36 grid cells=5504748 or 
  N <- tte_estN_fn(tte_eh, study_area = 5.504748e6)
  N$area <- mean(Areas[,i])
  DiffAreaN <- rbind(DiffAreaN, N)
}
DiffAreaN$D <- DiffAreaN$N/5.504748
DiffAreaN$DSE <- DiffAreaN$SE/5.504748
DiffAreaN$DLCI <- DiffAreaN$LCI/5.504748
DiffAreaN$DUCI <- DiffAreaN$UCI/5.504748
saveRDS(DiffAreaN, "./DiffAreaN.rds")

#for density by camera
study_dates2 <- as.POSIXct(c("2022-04-15 00:00:00", "2022-08-15 23:59:59"), tz = "America/Denver")
occ2 <- tte_build_occ(
  per_length = per,
  nper = 5,
  time_btw = 2 * 3600,
  study_start = study_dates2[1],
  study_end = study_dates2[2]
)

tte_eh2 <- tte_build_eh(df=df, deploy=deploy2, occ=occ2,  samp_per=per)
tte_ehlist <- split(tte_eh2, f=tte_eh2$cam)
estNlist <- lapply(tte_ehlist, function (x) tryCatch(tte_estN_fn(x, study_area = 0.152909e6), error=function(e) NULL))
estNlist2 <- lapply(tte_ehlist, function (x) tryCatch(tte_estN_fn(x, study_area = 5.504748e6), error=function(e) NULL))

#tte_estN_fn(tte_eh[tte_eh$cam== "BUSH31",], study_area = 0.152909e6)
estN <- rbindlist(estNlist, idcol = T)
saveRDS(estN, "./estNCam.rds")
hist(estN$N)
hist(1/estN$N)
hist(log(estN$N))
descdist(log(estN$N), discrete = FALSE)
descdist(estN$N, discrete = FALSE)
estN2 <- rbindlist(estNlist2, idcol = T)
hist(estN2$N)
hist(log(estN2$N))
descdist(log(estN2$N), discrete = FALSE)
descdist(estN2$N, discrete = FALSE)

per <- tte_samp_per(deploy2, lps = 1.76/60)
study_dates <- as.POSIXct(c("2022-04-15 00:00:00", "2022-05-15 23:59:59"), tz = "America/Denver")
occ <- tte_build_occ(
  per_length = per,
  nper = 5,
  time_btw = 2 * 3600,
  study_start = study_dates[1],
  study_end = study_dates[2]
)
#build encounter history
colnames(deploy2)[c(1:3)] <- c("cam", "start", "end")
deploy2$cam <- as.factor(deploy2$cam)
tte_eh <- tte_build_eh(df=df, deploy=deploy2, occ=occ,  samp_per=per)
N <- tte_estN_fn(tte_eh, study_area = 5.504748e6)




#STE
study_dates <- as.POSIXct(c("2022-04-15 00:00:00", "2022-05-15 23:59:59"), tz = "America/Denver")
occ <- build_occ(samp_freq = 3600, # seconds between the start of each sampling occasion
                 samp_length = 10, # duration of each sampling occasion (seconds)
                 study_start = study_dates[1],
                 study_end = study_dates[2])
ste_eh <- ste_build_eh(df, deploy2, occ)
ste_estN_fn(ste_eh, study_area = 5.504748e6)

##########################Graph###################################
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1)
DiffAreaN$group <- as.factor(c(1:3))
DiffAreaN$arearound <- round(DiffAreaN$area, digits=0)
# Use 95% confidence interval instead of SEM
ggplot(DiffAreaN, aes(x=as.factor(arearound), y=D, color=group)) + 
  geom_errorbar(aes(ymin=DLCI, ymax=DUCI), width=0.5, position=pd, size=0.5) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  ylim(0,8) +
  xlab(bquote('Area ('~m^2~')')) + ylab(bquote('Density (deer/'~km^2~')')) +
  #ggtitle("Deer Density by ")
  theme(legend.position = "none")

DiffSpeedsN$group <- as.factor(c(1:3))
DiffSpeedsN$speedround <- round(DiffSpeedsN$speed*3600, digits=0)
# Use 95% confidence interval instead of SEM
ggplot(DiffSpeedsN, aes(x=as.factor(speedround), y=D, color=group)) + 
  geom_errorbar(aes(ymin=DLCI, ymax=DUCI), width=0.5, position=pd, size=0.5) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  ylim(0,20) +
  xlab('Speed (meters/hr)') + ylab(bquote('Density (deer/'~km^2~')')) +
  #ggtitle("Deer Density by ")
  theme(legend.position = "none")


##################################scrap###################################################
x <- tte_eh[tte_eh$cam == "ACORN2",]


library(fitdistrplus)
eh <- tte_eh[tte_eh$cam == "BUSH11",]
eh <-eh$TTE[!is.na(eh$TTE)]
descdist(eh, discrete = FALSE)
hist(eh)
fit.exp <- fitdist(eh, "exp")
plot(fit.exp)

