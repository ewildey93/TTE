library(lubridate)
library(terra)
library(sf)


setwd("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE")


#Grid
Grid <- readOGR("C:/Users/eliwi/OneDrive/Documents/Salida/GeospatialLayers/Grid3.shp")
CamLocs <- read.csv("./CamLocs.csv")
str(CamLocs)
CamLocs <- CamLocs[-c(38:41),]
#Line Length
LineLength <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/LineLength.csv")

#lc
lc <- raster("C:/Users/eliwi/OneDrive/Documents/R/DeerISSFTWS/CoVs/NLCD_2019_Land_Cover_L48_20210604_JbsuwO6GkIW9V4xHbi6d.tiff")
plot(lc)
projection(lc)
freq(lc)
rcl <- matrix(data=c(22,23,24,42,43,81,82,95,21,21,21,41,41,71,71,90), nrow=8, ncol=2)
lc2 <- reclassify(lc, rcl=rcl)
plot(lc2)
CamLocsSF<-st_as_sf(CamLocs, coords=c("Long", "Lat"), crs=CRS("+init=epsg:4326"))
CamLocsSF<-st_transform(RndSteps4SF, projection(lc))
CamLocsSF$lc100<-raster::extract(lc, RndSteps4SF, buffer=100)
CamLocsSF$lc250 <- raster::extract(lc, RndSteps4SF, buffer=250)
CamLocsSF$lc400 <- raster::extract(lc, RndSteps4SF, buffer=400)



camdf <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/wildlifeinsights5.5/images.csv")
df <- camdf[camdf$genus=="Homo",]
df <- df[,c(2,16,17,22)]
colnames(df) <- c("cam","datetime","count","comments")
df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S", tz="America/Denver")
BUSH3 <- which(df$cam == "BUSH3")
df$cam <- replace(x = df$cam,list=BUSH3, values = "BUSH4")
df$week <- week(df$datetime)
df <- df %>% group_by(cam, week)%>% mutate(HumanSum= sum(count))
HumansatCam <- df%>%group_by(cam)%>%summarise(mean(HumanSum))
