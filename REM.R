library(remBoot)
library(camtrapR)
library(dplyr)
library(data.table)

setwd("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE")

data(recordTableSample)
data(recordTableIndividualSample)
data(hDat)


camdf <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/images.csv")
str(camdf)
table(camdf$deployment_id)
df <- camdf[camdf$common_name=="Mule Deer",]
df <- df[,c(2,4,16,17,22)]
colnames(df) <- c("cam", "file","datetime","count","comments")
df$datetime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S", tz="America/Denver")
#fawns?
df <- dplyr::filter(df, !(count==1 & comments== "fawn"))
table(df$cam)
#replace BUSH3 with BUSH4
BUSH3 <- which(df$cam == "BUSH3")
df$cam <- replace(x = df$cam,list=BUSH3, values = "BUSH4")
table(df$cam)
df$cam <- as.factor(df$cam)
df$file <- paste(df$cam,df$file,df$datetime, sep="/")
colnames(df)[1:3] <- c("Station","FileName","DateTimeOriginal")
df$Species <- "Mule Deer"

minDeltaTime <- 60
intable <- data.frame(df,
                      delta.time.secs  = NA,
                      delta.time.mins  = NA,
                      delta.time.hours = NA,
                      delta.time.days  = NA,
                      independent      = ifelse(minDeltaTime == 0, TRUE, NA),   # all independent if no temporal filtering
                      stringsAsFactors = FALSE,
                      check.names      = FALSE)        # to prevent ":" being converted to ".", e.g. in EXIF:Mak
# sort records by station, species, then time
outtable <- camtrapR:::assessTemporalIndependence(intable = intable, deltaTimeComparedTo = "lastRecord",columnOfInterest = "Species",stationCol = "Station",minDeltaTime = 60,camerasIndependent = FALSE)
outtable2 <- camtrapR:::assessTemporalIndependence(intable = intable, deltaTimeComparedTo = "lastRecord",columnOfInterest = "Species",stationCol = "Station",minDeltaTime = 5,camerasIndependent = FALSE)
intable <- intable[order(intable[, intable$Station], intable[, intable$Species], intable$DateTimeOriginal),]
outtable3 <- outtable[outtable$DateTimeOriginal %between% study_dates,]

#remove duplicate records of same species taken in same second at the same station (by the same camera, if relevant)
metadata.tmp2 <- camtrapR:::removeDuplicatesOfRecords(metadata.tmp          = intable,
                                           removeDuplicateRecords = TRUE,
                                           camerasIndependent     = FALSE,
                                           stationCol             = "Station",
                                           speciesCol             = "Species",
                                           cameraCol              = "Station",
                                           current                = 1, 
                                           total                  = 35,
                                           max_nchar_station      = 30)

grpDat <- split_dat(hDat)
DTimes <- read.csv("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE/DeploymentTimes3.csv")
DTimes$Start<- as.POSIXct(DTimes$Start,format="%m/%d/%Y %H:%M:%S", tz="America/Denver")
DTimes$End<- as.POSIXct(DTimes$End,format="%m/%d/%Y %H:%M:%S", tz="America/Denver")
study_dates <- as.POSIXct(c("2022-04-15 00:00:00", "2022-05-15 23:59:59"), tz = "America/Denver")
DTimes$seff <- case_when(DTimes$Start %between% study_dates ~ DTimes$Start, 
                         DTimes$Start < study_dates[1] ~ study_dates[1],
                         DTimes$Start > study_dates[2] ~ study_dates[2])
DTimes$eeff <- case_when(DTimes$End %between% study_dates ~ DTimes$End, 
                         DTimes$End < study_dates[1] ~ study_dates[2],
                         DTimes$End > study_dates[2] ~ study_dates[2])
DTimes$tm <- as.numeric(difftime(DTimes$eeff, DTimes$seff, units="hours"))
