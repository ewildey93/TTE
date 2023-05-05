library(ctmm)
library(move)
library(data.table)
library(dplyr)
setwd("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE")

Deer2 <- readRDS("C:/Users/eliwi/OneDrive/Documents/R/DeerISSFTWS/DeerList2.rds")
Deer2 <- rbindlist(Deer2, idcol=T)
Deer2 <- Deer2[, -c(9,10)]
str(Deer2)
UsedList <- split(Deer2, f=Deer2$.id)
UsedList <- lapply(UsedList, function (x) x[order(x$DateTime),])
Used <- arrange(Deer2, .id, DateTime)
TagID <- Used$.id
MoveList <- lapply(UsedList, function (x) move(x=x$X, y=x$Y, time=x$DateTime, data=x, proj=CRS("+init=epsg:32613")))
Move <- move(x=Used$X, y=Used$Y, time=Used$DateTime, data=Used,proj=CRS("+init=epsg:32613"),animal=TagID)
TelemList <- lapply(MoveList, function (x) as.telemetry(x,timeformat = "",timezone = "America/Denver", projection=NULL,datum=NULL,timeout=Inf,na.rm = "col",mark.rm = FALSE, keep=FALSE,drop=TRUE))
Telem <- as.telemetry(Used,timeformat = "",timezone = "America/Denver", projection=NULL,datum=NULL,timeout=Inf,na.rm = "col",mark.rm = FALSE, keep=FALSE,drop=TRUE)
Outliers <- lapply(Telem, function (x) outlie (x))
plot(Outliers[["F9946"]])
Outliers[["F49862"]][Outliers[["F49862"]]$speed> 0.3 | Outliers[["F49862"]]$distance>2500,]
OutF49682 <- Telem[['F49862']][c(69,99,221),]
Outliers[["F47288"]][Outliers[["F47288"]]$speed> 0.35,]
which(Outliers[["F47288"]]$speed> 0.35)
OutF47288 <- Telem[['F47288']][334,]
Outliers[["F9946"]][Outliers[["F9946"]]$distance> 2000,]
which(Outliers[["F9946"]]$distance> 2000)
OutF49946 <- Telem[['F9946']][334,]

View(Outliers[['F47288']])

vgList <- lapply(Telem, function(x) variogram (x))
GUESSList <- mapply(ctmm.guess, data=Telem, variogram=vgList,interactive=FALSE, SIMPLIFY=FALSE)
GUESSList <- lapply(GUESSList, function (x) {x$error <- 10
                                            return(x)
                                            })
saveRDS(Telem,"./TelemList.rds")
saveRDS(GUESSList, "./GUESSList.rds")
FITSList <- Map(function(x,y) ctmm.select(data = x,CTMM = y,trace=T,cores=2), x=Telem, y=GUESSList)
saveRDS(FITSList, "./FITSList.rds")
f <- ctmm.select(TelemList[[2]], GUESSList[[2]])
lapply(FITSList, function (x) summary(x))

SpeedList <- Map(function (x,y) ctmm::speed(object=x,CTMM=y),x=Telem ,y=FITSList)
saveRDS(SpeedList, "./SpeedList.rds")
lapply(SpeedList, function (x) summary(x))
SpeedDF <- rbindlist(SpeedList, idcol=T)

ctmm::speed(Telem[1], FITSList[1])
