##############################################################################################################################
##################### Sean_fseal_VARS.R: Is a file containg code to process fur seal diving data. ############################
##############################################################################################################################

##############################################################################################################################
############################################# Creating large dataframe #######################################################
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
library(ggstatsplot)
# library(ncdf4)
library(raster)
library(magrittr)
library(dplyr)
library(rgl)
library(lme4)
#require(GGally)
#require(reshape2)
#require(compiler)
#require(parallel)
#require(boot)
rm(list = ls())

#SealIDS = c(1,2,4,5,seq(13,18,1),seq(20,25,1),31,49,50,51,52,57,63,65,68,69,110,111,112,113)
SealIDS = c(1,2,4,5,seq(13,25,1),31,47,seq(49,53,1),57,58,seq(62,70,1),109,110,112,113)

for (i in 1:length(SealIDS)) {
  print(i)
  sealID = SealIDS[i]
  save_filtered_divestats = paste("Plots & Dive Tables/Seal",sealID, sep = "")
  filtered_divestats <- read_rds(file.path(save_filtered_divestats,paste(sealID,"_filtered_divestats.rds",sep = '')))
  
  if (i==1) {
    dives = filtered_divestats
  }
  
  else{
    dives = rbind(dives,filtered_divestats)
  }
}


for (i in 1:length(SealIDS)) {
  print(i)
  sealID = SealIDS[i]
  save_loc = paste("Plots & Dive Tables/Seal",sealID, sep = "")
  loc1 <- read_rds(file.path(save_loc,paste(sealID,"_loc.rds",sep = '')))
  
  if (i==1) {
    locs = loc1
  }
  
  else{
    locs = rbind(locs,loc1)
  }
}


save_all = 'Plots & Dive Tables/'
saveRDS(dives,file.path(save_all,"dives.rds"))
saveRDS(locs,file.path(save_all,"locs.rds"))

