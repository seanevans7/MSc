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
  # lubridate assumes a certain timezone and gives attribute SAST (South African Standard Time) to our gmt time, therefore:
  theData$gmt <- force_tz(theData$gmt, tzone = "GMT", roll = FALSE)
  theData$local_time <- theData$gmt+(theData$lon/15)*3600 # You divide by 15 because (360 degrees around the earth/24 hours rotation cycle = 15 time zones in hours)
  theData$local_time <- force_tz(theData$local_time, tzone = "Africa/Addis_Ababa", roll = FALSE)
  # theData$LOC.TIME <- as.POSIXct(strptime(paste('1970-01-01', format(theData$local_time, '%H:%M:%S')), '%Y-%m-%d %H:%M:%S'), tz='GMT')
  
  theData$JDay <- format(theData$local_time, '%j') #My Julian days are based on local time (So probably are not called 'Julian days' any more and just 'day of year')
  
  # theData$sunrise.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
  #                                dateTime=theData$gmt, direction='sunrise', POSIXct.out=T)$day_frac*24
  theData$sunrise <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                                 dateTime=theData$local_time, direction='sunrise', POSIXct.out=T)$time
  
  # theData$sunset.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                                # dateTime=theData$local_time, direction='sunset', POSIXct.out=T)$day_frac*24
  theData$sunset <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                                dateTime=theData$local_time, direction='sunset', POSIXct.out=T)$time
  
  theData$dawn <- crepuscule(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),             # Astronomical dawn:solarDep = 18 degrees below horizon (e.g. sunset 16:30 dusk 18:10)
                             solarDep = 12, dateTime=theData$local_time, direction='dawn', POSIXct.out=T)$time # Nautical dawn:solarDep = 12 degrees below horizon - Used by Mia PhD (e.g. sunset 16:30 dusk 17:35)
                                                                                                           # Civil dawn: solarDep = 6 degrees below horizon
  
  theData$dusk <- crepuscule(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                             solarDep = 12, dateTime=theData$local_time, direction='dusk', POSIXct.out=T)$time
  
  theData$mirror.time <- 43200-abs(as.numeric(theData$local_time)-43200)
  
  
  theData$diel_phase[theData$local_time > theData$sunset & theData$local_time < theData$dusk] = 'Dusk'
  theData$diel_phase[theData$local_time > theData$dusk | theData$local_time < theData$dawn] = 'Night'
  theData$diel_phase[theData$local_time > theData$dawn & theData$local_time < theData$sunrise] = 'Dawn'
  theData$diel_phase[theData$local_time > theData$sunrise & theData$local_time < theData$sunset] = 'Day'
  theData$hour <- format(theData$local_time, "%H")
  
  theData
}

diel.divestats <- function(theData) {
  require(maptools)
  theData$start <- force_tz(theData$start, tzone = "GMT", roll = FALSE)
  theData$local_time <- theData$start+(theData$lon/15)*3600
  theData$local_time <- force_tz(theData$local_time, tzone = "Africa/Addis_Ababa", roll = FALSE)
 # theData$LOC.TIME <- as.POSIXct(strptime(paste('1970-01-01', format(theData$local_time, '%H:%M:%S')), '%Y-%m-%d %H:%M:%S'), tz='GMT')
  
  theData$JDay <- as.numeric(format(theData$local_time, '%j')) #My Julian days are based on local time (So probably are not called 'Julian days' any more and just 'day of year')
  
  # theData$sunrise.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
  #                                dateTime=theData$gmt, direction='sunrise', POSIXct.out=T)$day_frac*24
  theData$sunrise <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                              dateTime=theData$local_time, direction='sunrise', POSIXct.out=T)$time
  
  # theData$sunset.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
  # dateTime=theData$local_time, direction='sunset', POSIXct.out=T)$day_frac*24
  theData$sunset <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                             dateTime=theData$local_time, direction='sunset', POSIXct.out=T)$time
  
  theData$dawn <- crepuscule(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),             # Astronomical dawn:solarDep = 18 degrees below horizon (e.g. sunset 16:30 dusk 18:10)
                             solarDep = 12, dateTime=theData$local_time, direction='dawn', POSIXct.out=T)$time  # Nautical dawn:solarDep = 12 degrees below horizon - Used by Mia PhD (e.g. sunset 16:30 dusk 17:35)
  # Civil dawn: solarDep = 6 degrees below horizon
  
  theData$dusk <- crepuscule(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                             solarDep = 12, dateTime=theData$local_time, direction='dusk', POSIXct.out=T)$time
  
  theData$mirror.time <- 43200-abs(as.numeric(theData$local_time)-43200)
  
  theData$diel_phase[theData$local_time > theData$sunset & theData$local_time < theData$dusk] = 'Dusk'
  theData$diel_phase[theData$local_time > theData$dusk | theData$local_time < theData$dawn] = 'Night'
  theData$diel_phase[theData$local_time > theData$dawn & theData$local_time < theData$sunrise] = 'Dawn'
  theData$diel_phase[theData$local_time > theData$sunrise & theData$local_time < theData$sunset] = 'Day'
  theData$hour <- format(theData$local_time, "%H")
  
  # theData$diel_phase[theData$local_time > theData$sunset & theData$local_time < theData$dusk+3600] = 'Dusk'
  # theData$diel_phase[theData$local_time > theData$dusk+3600 | theData$local_time < theData$dawn-3600] = 'Night'
  # theData$diel_phase[theData$local_time > theData$dawn-3600 & theData$local_time < theData$sunrise] = 'Dawn'
  # theData$diel_phase[theData$local_time > theData$sunrise & theData$local_time < theData$sunset] = 'Day'
  # theData$hour <- format(theData$local_time, "%H")
  
  theData
}

diel.bsm <- function(theData) { 
  require(maptools)
  theData$start <- as.POSIXct(theData$start,tz = "GMT")
  theData$start <- force_tz(theData$start, tzone = "GMT", roll = FALSE)
  theData$local_time <- theData$start+(theData$lon/15)*3600
  theData$local_time <- force_tz(theData$local_time, tzone = "Africa/Addis_Ababa", roll = FALSE)
  #theData$LOC.TIME <- as.POSIXct(strptime(paste('1970-01-01', format(theData$local_time, '%H:%M:%S')), '%Y-%m-%d %H:%M:%S'), tz='GMT')
  
  theData$JDay <- as.numeric(format(theData$local_time, '%j')) #My Julian days are based on local time (So probably are not called 'Julian days' any more and just 'day of year')
  
  # theData$sunrise.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
  #                                dateTime=theData$gmt, direction='sunrise', POSIXct.out=T)$day_frac*24
  theData$sunrise <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                              dateTime=theData$local_time, direction='sunrise', POSIXct.out=T)$time
  
  # theData$sunset.hr <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
  # dateTime=theData$local_time, direction='sunset', POSIXct.out=T)$day_frac*24
  theData$sunset <- sunriset(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                             dateTime=theData$local_time, direction='sunset', POSIXct.out=T)$time
  
  theData$dawn <- crepuscule(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),             # Astronomical dawn:solarDep = 18 degrees below horizon (e.g. sunset 16:30 dusk 18:10)
                             solarDep = 12, dateTime=theData$local_time, direction='dawn', POSIXct.out=T)$time  # Nautical dawn:solarDep = 12 degrees below horizon - Used by Mia PhD (e.g. sunset 16:30 dusk 17:35)
  # Civil dawn: solarDep = 6 degrees below horizon
  
  theData$dusk <- crepuscule(crds=as.matrix(theData[,match(c('lon', 'lat'), names(theData))]),
                             solarDep = 12, dateTime=theData$local_time, direction='dusk', POSIXct.out=T)$time
  
  theData$mirror.time <- 43200-abs(as.numeric(theData$local_time)-43200)
  
  theData$diel_phase[theData$local_time > theData$sunset & theData$local_time < theData$dusk] = 'Dusk'
  theData$diel_phase[theData$local_time > theData$dusk | theData$local_time < theData$dawn] = 'Night'
  theData$diel_phase[theData$local_time > theData$dawn & theData$local_time < theData$sunrise] = 'Dawn'
  theData$diel_phase[theData$local_time > theData$sunrise & theData$local_time < theData$sunset] = 'Day'
  theData$hour <- format(theData$local_time, "%H")
  
  theData
}