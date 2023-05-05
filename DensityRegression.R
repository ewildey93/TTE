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
CamLocsSF<-st_transform(CamLocsSF, projection(lc))
lc100<-raster::extract(lc2, CamLocsSF, buffer=100)
lc250 <- raster::extract(lc2, CamLocsSF, buffer=250)
lc400 <- raster::extract(lc2, CamLocsSF, buffer=400)

z <- sort(unique(raster::values(lc2)))

summaryValueslc100 <- lapply(lc100,FUN = summarizeLC,LC_classes = z)
summaryValueslc250 <- lapply(lc250,FUN = summarizeLC,LC_classes = z)
summaryValueslc400 <- lapply(lc400,FUN = summarizeLC,LC_classes = z)

listnames <- CamLocs$Camera

names(summaryValueslc100) <- listnames
names(summaryValueslc250) <- listnames
names(summaryValueslc400) <- listnames
lclist <- list(summaryValueslc100,summaryValueslc250,summaryValueslc400)

for (i in 1: length(lclist)) {
  lclist[i] <- lapply(lclist[i], function (x) as.data.frame(t(x)))
}

Plc100 <- rbindlist(summaryValueslc100)
Plc250 <- rbindlist(summaryValueslc250)
Plc400 <- rbindlist(summaryValueslc400)
#convert herbaceous cover to polygon and get distance to values
herb <- rasterToPolygons(lc2, function (x) x == 71, n=16, dissolve = T)
crs(herb)
CamLocs$distherb <- distance(CamLocsSF, herb, doEdge=T)


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


#functions
summarizeLC <- function(x,LC_classes,LC_names = NULL){
  # Find the number of cells 
  y <- length(x)
  # Make a table of the cells
  tx <- table(x)
  # Create an empty array to store landcover data
  LC <- array(NA,c(1,length(LC_classes)))
  # Loop through the landcover types & return 
  # the number of cells within each landcover type
  for(i in seq(LC_classes)){
    LC[1,i] <- ifelse(LC_classes[i] %in% dimnames(tx)[[1]], 
                      #if true
                      tx[which(dimnames(tx)[[1]]==LC_classes[i])],
                      # if false
                      0) 
  } # end loop
  # Convert to percentage 
  LC <- LC/y
  # 
  if(!is.null(LC_names)){
    colnames(LC)<-LC_names}
  else{colnames(LC)<-LC_classes}
  
  return(LC)
}
