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
SealIDS = c(1,2,4,5,seq(13,25,1),31,36,seq(47,53,1),57,58,seq(62,70,1),seq(109,113,1))

loc1 <-droplevels(subset(tracks,tracks$id == SealIDS))
loc1 <- tracks %>% filter(tracks$id %in% SealIDS)
sf_Marion <- data.frame("lon" = 37.746368, "lat" = -46.893361) %>% 
  st_as_sf(coords = c("lon", "lat"),crs=4326,remove = FALSE)

ggplot() + 
  geom_point(aes(x = lon , y = lat, color = season),data = loc1) + #, pch = 19
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='green', data = sf_Marion) +
  ggtitle(paste('All SAFS trips   (',abs(round(min(loc1$lat),2)),'S,',
                round(min(loc1$lon),2),'E',' - ',abs(round(max(loc1$lat),2)),'S,',round(max(loc1$lon),2),'E)',sep = ''))

# Add bathymetry data to this

# Single seal of interest
ggplot() + 
  geom_point(aes(x = lon , y = lat, color = season),data = loc1) + #, pch = 19
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='green', data = sf_Marion) +
  ggtitle(paste('All SAFS trips   (',abs(round(min(loc1$lat),2)),'S,',
                round(min(loc1$lon),2),'E',' - ',abs(round(max(loc1$lat),2)),'S,',round(max(loc1$lon),2),'E)',sep = ''))
