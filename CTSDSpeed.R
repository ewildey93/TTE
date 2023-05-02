library(ctmm)
library(move)

setwd("C:/Users/eliwi/OneDrive/Documents/R/TTE/TTE")

Deer2 <- readRDS("C:/Users/eliwi/OneDrive/Documents/R/DeerISSFTWS/DeerList2.rds")
Deer2 <- rbindlist(Deer2, idcol=T)
Deer2 <- Deer2[, -c(9,10)]
str(Deer2)
UsedList <- split(Deer2, f=Deer2$.id)
UsedList <- lapply(UsedList, function (x) x[order(x$DateTime),])
Used <- arrange(Deer2, ID, DateTime)
MoveList <- lapply(UsedList, function (x) move(x=x$X, y=x$Y, time=x$DateTime, data=x, proj=CRS("+init=epsg:32613")))
Move <- move(x=Used$X, y=Used$Y, time=Used$DateTime, data=Used,proj=CRS("+init=epsg:32613"),animal=Used$ID)
TelemList <- lapply(MoveList, function (x) as.telemetry(x,timeformat = "",timezone = "America/Denver", projection=NULL,datum=NULL,timeout=Inf,na.rm = "col",mark.rm = FALSE, keep=FALSE,drop=TRUE))
Outliers <- lapply(TelemList, function (x) outlie (x))


vgList <- lapply(TelemList, function(x) variogram (x))
GUESSList <- mapply(ctmm.guess, data=TelemList,CTMM=ctmm(error=rep(10, times=9)), variogram=vgList,interactive=FALSE, SIMPLIFY=FALSE)
GUESSList <- lapply(GUESSList, function (x) {x$error <- TRUE
                                            return(x)
                                            })

FITSList <- Map(function(x,y) ctmm.select(data = x,CTMM = y ), x=TelemList, y=GUESSList)
f <- ctmm.select(TelemList[[2]], GUESSList[[2]])


SpeedList <- mapply(speed,TelemList ,FITSList, SIMPLIFY=F)

