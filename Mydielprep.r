# setwd(choose.dir())
# fn <- choose.files()
# m <- read.csv(fn)
# names(m)
# 
# #m$DEDATE<-paste(m$DATE,m$TIME,sep=" ")
# # #head(m)
# m2 <- diel.prep(m)
# head(m2)
# fn2 <- choose.files()
# write.csv(m2,file=fn2,row.names=F)  # write the file to a csv

# 
# diel.prep <- function(theData) {
#   require(maptools)
#   
#   theData$DEDATE<-as.POSIXct(strptime(as.character(theData$DEDATE),'%Y/%m/%d %H:%M'),tz="GMT")
#   
#   theData$LOC.DATE <- theData$DEDATE+(theData$LON/15)*3600
#   theData$LOC.TIME <- as.POSIXct(strptime(paste('1970-01-01', format(theData$LOC.DATE, '%H:%M:%S')), '%Y-%m-%d %H:%M:%S'), tz='GMT')
#   
#   theData$JDay <- as.numeric(format(theData$DEDATE, '%j'))
#   
#   theData$sunrise.hr <- sunriset(crds=as.matrix(theData[,match(c('LON', 'LAT'), names(theData))]),
#                                  dateTime=theData$DEDATE, direction='sunrise', POSIXct.out=T)$day_frac*24
#   theData$sunrise.hr <- theData$sunrise.hr+(theData$LON/15)
#   
#   theData$sunset.hr <- sunriset(crds=as.matrix(theData[,match(c('LON', 'LAT'), names(theData))]),
#                                 dateTime=theData$LOC.DATE, direction='sunset', POSIXct.out=T)$day_frac*24
#   theData$sunset.hr <- theData$sunset.hr+(theData$LON/15)
#   
#   theData$mirror.time <- 43200-abs(as.numeric(theData$LOC.TIME)-43200)
#   
#   theData
# }
#Hi Mia

# Lyk of dit werk - kyk bietjie nadat jy een gehardloop het of the waardes
# realisties is!
# 
# So, stap vir stap:
# 1. Verander jou headings vir date/time na "DEDATE"; lat en long na "LAT"
# en "LON".
# 2. Run die aangehegte script (jy kan dit ook net drag en drop in jou R
# browser in).
# 3. Laai jou datastel met n naam (bv. m <- read.csv("..."))
# 4. Run die 'diel.prep' command (bv. m2 <- diel.prep(m))
# 
# Jou entity (m2)  behoort dan n paar aangehegte kolomme te he - die
# belangrikste is "LOC.TIME", "sunrise.hr" en "sunset.hr". Die local time
# ene het n default datum saam met dit, wat jy kan ignoreer (1970-01-01) -
# jy stel net in die tyd waarde belang.
# 
# Laat my weet of dit werk!
# 
# Trevor



################ Sean function ################

diel.loc <- function(theData) {
  require(maptools)
  
  theData$local_time <- theData$gmt+(theData$lon/15)*3600
  theData$LOC.TIME <- as.POSIXct(strptime(paste('1970-01-01', format(theData$local_time, '%H:%M:%S')), '%Y-%m-%d %H:%M:%S'), tz='GMT')
  
  theData$JDay <- as.numeric(format(theData$gmt, '%j'))
  
  theData$sunrise.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                                 dateTime=theData$gmt, direction='sunrise', POSIXct.out=T)$day_frac*24
  theData$sunrise.hr <- theData$sunrise.hr+(theData$lon/15)
  
  theData$sunset.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                                dateTime=theData$local_time, direction='sunset', POSIXct.out=T)$day_frac*24
  theData$sunset.hr <- theData$sunset.hr+(theData$lon/15)
  
  theData$mirror.time <- 43200-abs(as.numeric(theData$LOC.TIME)-43200)
  
  theData
}

diel.divestats <- function(theData) {
  require(maptools)
  
  theData$local_time <- theData$start+(theData$lon/15)*3600
  theData$LOC.TIME <- as.POSIXct(strptime(paste('1970-01-01', format(theData$local_time, '%H:%M:%S')), '%Y-%m-%d %H:%M:%S'), tz='GMT')
  
  theData$JDay <- as.numeric(format(theData$start, '%j'))
  
  theData$sunrise.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                                 dateTime=theData$start, direction='sunrise', POSIXct.out=T)$day_frac*24
  theData$sunrise.hr <- theData$sunrise.hr+(theData$lon/15)
  
  theData$sunset.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                                dateTime=theData$local_time, direction='sunset', POSIXct.out=T)$day_frac*24
  theData$sunset.hr <- theData$sunset.hr+(theData$lon/15)
  
  theData$mirror.time <- 43200-abs(as.numeric(theData$LOC.TIME)-43200)
  
  theData
}

diel.bsm <- function(theData) {
  require(maptools)
  theData$start <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + theData$time_start
  theData$start <- as.POSIXct(theData$start)
  theData$local_time <- theData$start+(theData$lon/15)*3600
  theData$LOC.TIME <- as.POSIXct(strptime(paste('1970-01-01', format(theData$local_time, '%H:%M:%S')), '%Y-%m-%d %H:%M:%S'), tz='GMT')
  
  theData$JDay <- as.numeric(format(theData$start, '%j'))
  
  theData$sunrise.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                                 dateTime=theData$start, direction='sunrise', POSIXct.out=T)$day_frac*24
  theData$sunrise.hr <- theData$sunrise.hr+(theData$lon/15)
  
  theData$sunset.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                                dateTime=theData$local_time, direction='sunset', POSIXct.out=T)$day_frac*24
  theData$sunset.hr <- theData$sunset.hr+(theData$lon/15)
  
  theData$mirror.time <- 43200-abs(as.numeric(theData$LOC.TIME)-43200)
  
  theData
}