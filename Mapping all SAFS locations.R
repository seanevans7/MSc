##############################################################################################################################
##################### Sean_fseal_map.R: Is a file containg code to map fur seal locations.        ############################
##############################################################################################################################

##############################################################################################################################
#############################################     Mapping all SAFS     #######################################################
#############################################      By Sean Evans       #######################################################
##############################################################################################################################


# Load packages -----------------------------------------------------------

library(purrr)
library(broom)
library(move)
library(moveVis)
library(sf)
library(maps)
library(mapdata)
library(fields)
library(lattice)
library(effects)
library(trip)
library(Matrix)
library(tidyverse)
#library(plyr)
library(ggplot2)
library(diveMove)
library(pbapply)
library(wrapr)
library(stringr)
library(lubridate)
library(microbenchmark)
library(data.table)
library(ggplot2)
# library(ggstatsplot)
library(ncdf4)
library(raster)
library(magrittr)
library(dplyr)

rm(list = ls())
setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc")
tracks <- read.csv("Marion_FStracks_SSMresults_2009W_2015S.csv",sep = ";")
# str(tracks)
SealIDS = c(1,2,4,5,seq(13,25,1),31,47,seq(49,53,1),57,58,seq(62,70,1),109,110,112,113)


loc1 <-droplevels(subset(tracks,tracks$id == SealIDS))
loc1 <- tracks %>% filter(tracks$id %in% SealIDS)
sf_Marion <- data.frame("lon" = 37.746368, "lat" = -46.893361) %>% 
  st_as_sf(coords = c("lon", "lat"),crs=4326,remove = FALSE)

ggplot() + 
  geom_point(aes(x = lon , y = lat, color = season),data = diving %>% filter(sealID==19 & ft==3)) + #, pch = 19
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='green', data = sf_Marion) #+
  #ggtitle(paste('All SAFS trips   (',abs(round(min(loc1$lat),2)),'S,',
                #round(min(loc1$lon),2),'E',' - ',abs(round(max(loc1$lat),2)),'S,',round(max(loc1$lon),2),'E)',sep = ''))

# Add bathymetry data to this

# Single seal of interest
ggplot() + 
  geom_point(aes(x = lon , y = lat, color = season),data = loc1) + #, pch = 19
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='green', data = sf_Marion) +
  ggtitle(paste('All SAFS trips   (',abs(round(min(loc1$lat),2)),'S,',
                round(min(loc1$lon),2),'E',' - ',abs(round(max(loc1$lat),2)),'S,',round(max(loc1$lon),2),'E)',sep = ''))



(diving %>% filter(season=='summer'))$lat %>% min()
(diving %>% filter(season=='summer'))$lat %>% max()
(diving %>% filter(season=='summer'))$lon %>% min()
(diving %>% filter(season=='summer'))$lon %>% max()

(diving %>% filter(season=='winter'))$lat %>% min()
(diving %>% filter(season=='winter'))$lat %>% max()
(diving %>% filter(season=='winter'))$lon %>% min()
(diving %>% filter(season=='winter'))$lon %>% max()

### ft per seal diving dataframe
for (i in 1:length(SealIDS)) {
  print(i)
  s=SealIDS[i]
  furry=(diving %>% filter(sealID==s))
  for (f in 1:max(furry$ft)){
    if (furry$season[1]=='summer'){
      ggplot() + 
        geom_point(aes(x = lon , y = lat, colour = start),data = furry %>% filter(ft==f)) + #, pch = 19
        geom_point(aes(x = lon , y = lat),pch = 17,size=2, color='green', data = sf_Marion) +
        xlim(31.4149,45.45842) + ylim(-50.41736,-45.39571) +
        ggtitle(paste0('n points = ',as.character(furry %>% filter(ft==f) %>% group_by(lat,lon) %>% count() %>% NROW()),'  &  dives = ',as.character(furry %>% filter(ft==f) %>% NROW())))
      ggsave(paste0('C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc/Plots & Dive Tables/All seals/summer/seal',as.character(s),'ft',as.character(f),'.png'), width = 24, height = 15, units = "cm")
    }
    if (furry$season[1]=='winter'){
      ggplot() + 
        geom_point(aes(x = lon , y = lat, colour = start),data = furry %>% filter(ft==f)) + #, pch = 19
        geom_point(aes(x = lon , y = lat),pch = 17,size=2, color='green', data = sf_Marion) +
        xlim(20.18641,47.52896) + ylim(-51.50458,-41.80111) +
        ggtitle(paste0('n points = ',as.character(furry %>% filter(ft==f) %>% group_by(lat,lon) %>% count() %>% NROW()),'  &  dives = ',as.character(furry %>% filter(ft==f) %>% NROW())))
      ggsave(paste0('C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc/Plots & Dive Tables/All seals/winter/seal',as.character(s),'ft',as.character(f),'.png'), width = 24, height = 15, units = "cm")
    }
  }
}

furry %>% filter(ft==f) %>% group_by(lat,lon) %>% count() %>% NROW()
diving %>% group_by(sealID,ft,lat,lon) %>% count() %>% group_by(sealID,ft) %>% count() %>% view()




# Foraging trips stats ----------------------------------------------------

Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')
for(j in 1: (nrow(Fts_summaries))) {
  # print(paste(j,nrow(Fts_summaries_init)-1),sep=" ")
  tmin <- as.POSIXct(Fts_summaries$'ft.start'[j], tz = 'GMT')
  tmax <- as.POSIXct(Fts_summaries$'ft.end'[j], tz = 'GMT')+24*3600
  if (j==1){
    if (NROW(diving$start[diving$start>=tmin & diving$start<=tmax & diving$sealID==Fts_summaries$'id'[j]])>0){
      index = j
    }
  }
  else{
    if (NROW(diving$start[diving$start>=tmin & diving$start<=tmax & diving$sealID==Fts_summaries$'id'[j]])>0){
      index = c(index,j)
    }    
  }
}
my_fts = Fts_summaries[index,]
write.csv(my_fts,paste0('C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc/',"my_fts.csv"))

my_fts = read.csv(paste0('C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc/',"my_fts.csv"),sep = ';')

my_fts %>% summary()

dives %>% group_by(sealID,ft) %>% sum('hunting_time')
