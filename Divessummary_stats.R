##############################################################################################################################
##################### Sean_fseal_VARS.R: Is a file containg code to process fur seal diving data. ############################
##############################################################################################################################

##############################################################################################################################
############################################# Post-processing of dives #######################################################
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

#fs_list <- list.files('divestats', pattern = ".rds")
#fs_list = fs_list[order(as.numeric(sub("([0-9]*).*", "\\1", fs_list)))]
SealIDS = c(1,2,4,5,seq(13,25,1),31,36,seq(47,53,1),57,58,seq(62,70,1),seq(109,113,1))
Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')

for (i in 1:len(SealIDS)) {
  sealID = SealIDS[i]
  save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")
  divessummary <- read_csv(file.path(save_divessummary,"divessummary.csv"))
  
  #################### Recalibrating divestats times #################
  #divessummary
  divessummary$local_time = force_tz(divessummary$local_time, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$sunrise = force_tz(divessummary$sunrise, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$sunset = force_tz(divessummary$sunset, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$dawn = force_tz(divessummary$dawn, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$dusk = force_tz(divessummary$dusk, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600

  # adding foraging trip to divestats ---------------------------------------

  #############
  Fts_summaries_init = Fts_summaries %>% filter(id == sealID) %>% dplyr::select(ft.start,ft.end,meandir,sddirDEG,ani_n)

  Fts_summaries_init$ft_ht <- NA # sum of hunting time for all dives within this foraging trip

  ##run a loop over the foraging trip data, grabbing the corresponding dive data
  system.time({
    for(i in 1:nrow(Fts_summaries_init)) {
      print(paste(i,nrow(Fts_summaries_init)-1),sep=" ")
      tmin <- Fts_summaries_init$'ft.start'[i]
      tmax <- Fts_summaries_init$'ft.end'[i]
      mvar <- divessummary[divessummary$start >= tmin & divessummary$start <= tmax,]
      Fts_summaries_init$ft_ht[i] <- mvar %>% filter(hunting_time>0) %>% dplyr::select(hunting_time) %>% sum()
    }
  })

  divessummary$ft <- NA # Adding ft column to divestats for later stats
  ##run a loop over the divessummary data, grabbing the corresponding foraging trip id
  system.time({
    for(i in 1: (nrow(Fts_summaries_init))) {
      print(paste(i,nrow(Fts_summaries_init)-1),sep=" ")
      tmin <- as.POSIXct(Fts_summaries_init$'ft.start'[i])
      tmax <- as.POSIXct(Fts_summaries_init$'ft.end'[i])+24*3600
      divessummary$ft[divessummary$start>=tmin & divessummary$start<=tmax] = i
    }
  })
  # Add column for bouts
  time_diff <- difftime(divessummary$start, lag(divessummary$start, default = divessummary$start[1]), units = "sec")
  divessummary$bout <- cumsum(ifelse(time_diff<3600*6,0,1))
  saveRDS(divessummary,file.path(save_divestats,paste(sealID,"_divestats.rds",sep = "")),compress = TRUE)
}
