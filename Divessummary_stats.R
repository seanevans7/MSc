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

#############################################################################
# Choosing seal files -----------------------------------------------------
#############################################################################
#fs_list <- list.files('divestats', pattern = ".rds")
#fs_list = fs_list[order(as.numeric(sub("([0-9]*).*", "\\1", fs_list)))]
SealIDS = c(1,2,4,5,seq(13,25,1),31,36,seq(47,53,1),57,58,seq(62,70,1),seq(109,113,1))

Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')

for (i in 1:len(SealIDS)) {
  sealID = SealIDS[i]
  save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")
  divessummary <- read.csv(file.path(save_divessummary,"divessummary.csv"),sep=',')
  
  #############################################################################
  # Add columns to divessummary -----------------------------------------------------
  #############################################################################
  
  #################### Recalibrating divestats times #################
  #divessummary
  divessummary$local_time = force_tz(divessummary$local_time, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$sunrise = force_tz(divessummary$sunrise, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$sunset = force_tz(divessummary$sunset, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$dawn = force_tz(divessummary$dawn, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$dusk = force_tz(divessummary$dusk, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600

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

  ##############################
  ### Add ft to divessummary ###
  ##############################
  
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
  
  ##############################
  ### Add therm_depth ###
  ##############################
  
  divessummary <- divessummary %>%
    mutate(therm_depth = ifelse(Therm_dep>0 & Tmld>0 & abs(Therm_dep-Tmld)<=10, (Therm_dep+Tmld)/2,
                                ifelse(Therm_dep > 0 & is.na(Tmld), Therm_dep,
                                       ifelse(Tmld > 0 & is.na(Therm_dep), Tmld,
                                              ifelse(Therm_dep>0 & Tmld>0 & abs(Therm_dep-Tmld)>10, Therm_dep,NA)))),
           diff_therm = ifelse(Therm_dep > 0 & Tmld >0, (Therm_dep-Tmld),NA),
           therm_depth_diff = Mdepth_hunting-therm_depth)
  
  ##############################
  ### Add hARS_mode ###
  ##############################
  
  # Adding hARS_mode column to divestats for later stats
  tmin <- c(loc1$gmt - 1.25*3600,force_tz(as.POSIXct(last(loc1$gmt) + 1.25*3600,tz='GMT'), tzone = "GMT", roll = FALSE))
  z <- seq(length(tmin)-1) ## Have to redefine z for divestats
  #z <- z[1:(length(z)-1)]
  divessummary$start <- force_tz(divessummary$start, tzone = "GMT", roll = FALSE)
  z <- cut(divessummary$start,breaks = tmin, labels = z)
  # divestats <- divestats %>% 
  #   mutate("X" = as.integer(shift(z,-1))) %>% 
  #   left_join(loc1 %>% select(X,lon,lat),by = "X")
  divessummary <- divessummary %>% 
    mutate("X" = as.integer(z)) %>% 
    left_join(loc1 %>% select(X,b.5),by = "X")
  colnames(divessummary)[which(names(divessummary) == "b.5")] <- "hARS_mode"
  
  ##############################
  ### saveRDS divessummary ###
  ##############################
  
  col_list = c('max.d','all.dur','mean_Temp','deltaT','sealID','dive_efficiency',' hunting_time','lon','lat','diel_phase','JDay','ht_rat','distances..m.','Thermocline','therm_depth_diff')
  filtered_divestats <- divessummary %>% filter(max.d >4, all.dur>60)
  filtered_divestats <- filtered_divestats %>% dplyr::select(col_list)
  #saveRDS(filtered_divestats,file.path(save_divestats,paste(sealID,"_filtered_divestats.rds",sep = "")),compress = TRUE)
  
  
  
  #############################################################################
  # Add columns to loc1 -----------------------------------------------------
  #############################################################################
  save_loc1 = "loc1"
  loc1 <- read_rds(file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")))
  
  ##############################
  ### Add strat_prop to loc1 ###
  ##############################
  
  loc1$strat_prop <- NA # No. of dives that are in stratified water masses
  
  ##run a loop over the location data, grabbing the corresponding dive data
  ##2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
  system.time({
    for(i in 1: (nrow(loc1))) {
      print(paste(i,nrow(loc1)-1),sep=" ")
      tmin <- loc1$gmt[i] - (3600*1.25)
      tmax <- loc1$gmt[i] + (3600*1.25)
      mvar <- filtered_divestats[filtered_divestats$start >= tmin & filtered_divestats$start <= tmax,]
      if (NROW(mvar %>% filter(Thermocline=='present'))>0) {
        new_df <- mvar %>% filter(Thermocline=='present')
        loc1$strat_prop[i] <- NROW(new_df)
      }
      else {
        loc1$strat_prop[i] <- 0
      }
    }
  })
  
  ##############################
  ### Add strat_prop to loc1 ###
  ##############################
  
  loc1$mean_Temp <- NA
  loc1$dive_efficiency <- NA
  loc1$deltaT <- NA
  
  ##run a loop over the location data, grabbing the corresponding dive data
  ##2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
  system.time({
    for(i in 1: (nrow(loc1))) {
      print(paste(i,nrow(loc1)-1),sep=" ")
      tmin <- loc1$gmt[i] - (3600*1.25)
      tmax <- loc1$gmt[i] + (3600*1.25)
      mvar <- filtered_divestats[filtered_divestats$start >= tmin & filtered_divestats$start <= tmax,]
      loc1$mean_Temp[i] <- mvar$mean_Temp %>% mean(na.rm=T)
      loc1$dive_efficiency[i] <- mvar$dive_efficiency %>% mean(na.rm=T)
      loc1$deltaT[i] <- mvar$deltaT %>% mean(na.rm=T)
    }
  })
  
  ##############################
  ### saveRDS loc1 ###
  ##############################
  col_list = list('for_effort','mean_max_depth' ,'therm_depth','dive_efficiency','deltaT','strat_prop', 'hunting_time','mean_Temp','all.dur','lon','lat','diel_phase','no_dives','JDay')
  loc1 <- loc1 %>% dplyr::select(col_list)
  #saveRDS(loc1,file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")),compress = TRUE)
  
}


















































sealID = SealIDS[1]
save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")
divessummary <- read.csv(file.path(save_divessummary,"divessummary.csv"),sep=',')
save_loc1 = "loc1"
loc1 <- read_rds(file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")))

#############################################################################
# Add columns to divessummary -----------------------------------------------------
#############################################################################

#################### Recalibrating divestats times #################
#divessummary
divessummary$start = as.POSIXct(divessummary$start)
divessummary$local_time = force_tz(as.POSIXct(divessummary$local_time), tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$sunrise = force_tz(as.POSIXct(divessummary$sunrise), tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$sunset = force_tz(as.POSIXct(divessummary$sunset), tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$dawn = force_tz(as.POSIXct(divessummary$dawn), tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$dusk = force_tz(as.POSIXct(divessummary$dusk), tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600

#############
Fts_summaries_init = Fts_summaries %>% filter(id == sealID) %>% dplyr::select(ft.start,ft.end,meandir,sddirDEG,ani_n)

Fts_summaries_init$ft_ht <- NA # sum of hunting time for all dives within this foraging trip

##run a loop over the foraging trip data, grabbing the corresponding dive data
system.time({
  for(i in 1:nrow(Fts_summaries_init)) {
    print(paste(i,nrow(Fts_summaries_init)-1),sep=" ")
    tmin <- as.POSIXct(Fts_summaries_init$'ft.start'[i])
    tmax <- as.POSIXct(Fts_summaries_init$'ft.end'[i])
    mvar <- divessummary[divessummary$start >= tmin & divessummary$start <= tmax,]
    Fts_summaries_init$ft_ht[i] <- mvar %>% filter(hunting_time>0) %>% dplyr::select(hunting_time) %>% sum()
  }
})

##############################
### Add ft to divessummary ###
##############################

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

##############################
### Add therm_depth ###
##############################

divessummary <- divessummary %>%
  mutate(therm_depth = ifelse(Therm_dep>0 & Tmld>0 & abs(Therm_dep-Tmld)<=10, (Therm_dep+Tmld)/2,
                              ifelse(Therm_dep > 0 & is.na(Tmld), Therm_dep,
                                     ifelse(Tmld > 0 & is.na(Therm_dep), Tmld,
                                            ifelse(Therm_dep>0 & Tmld>0 & abs(Therm_dep-Tmld)>10, Therm_dep,NA)))),
         diff_therm = ifelse(Therm_dep > 0 & Tmld >0, (Therm_dep-Tmld),NA),
         therm_depth_diff = Mdepth_hunting-therm_depth)

##############################
### Add hARS_mode ###
##############################

# Adding hARS_mode column to divestats for later stats
tmin <- c(loc1$gmt - 1.25*3600,force_tz(as.POSIXct(last(loc1$gmt) + 1.25*3600,tz='GMT'), tzone = "GMT", roll = FALSE))
z <- seq(length(tmin)-1) ## Have to redefine z for divestats
#z <- z[1:(length(z)-1)]
divessummary$start <- force_tz(divessummary$start, tzone = "GMT", roll = FALSE)
z <- cut(divessummary$start,breaks = tmin, labels = z)
# divestats <- divestats %>% 
#   mutate("X" = as.integer(shift(z,-1))) %>% 
#   left_join(loc1 %>% select(X,lon,lat),by = "X")
divessummary <- divessummary %>% 
  mutate("X" = as.integer(z)) %>% 
  left_join(loc1 %>% select(X,b.5),by = "X")
colnames(divessummary)[which(names(divessummary) == "b.5")] <- "hARS_mode"

##############################
### saveRDS divessummary ###
##############################

col_list = c('start','max.d','Mdepth_hunting','all.dur','mean_Temp','deltaT','sealID','dive_efficiency','lon','lat','diel_phase','JDay','ht_rat','distances..m.','Thermocline','therm_depth_diff')
filtered_divestats <- divessummary %>% filter(max.d >4, all.dur>60)
filtered_divestats <- filtered_divestats %>% dplyr::select(col_list)
#saveRDS(filtered_divestats,file.path(save_divestats,paste(sealID,"_filtered_divestats.rds",sep = "")),compress = TRUE)



#############################################################################
# Add columns to loc1 -----------------------------------------------------
#############################################################################

##############################
### Add strat_prop to loc1 ###
##############################

loc1$meandir <- NA
loc1$sddirDEG <- NA
loc1$ani_n <- NA
loc1$ft_ht <- NA

system.time({
  for(i in 1:nrow(Fts_summaries_init)) {
    print(paste(i,nrow(Fts_summaries_init)-1),sep=" ")
    tmin <- as.POSIXct(Fts_summaries_init$'ft.start'[i])
    tmax <- as.POSIXct(Fts_summaries_init$'ft.end'[i])
    meandir = Fts_summaries_init$meandir
    sddirDEG = Fts_summaries_init$sddirDEG
    ani_n = Fts_summaries_init$ani_n
    ft_ht = Fts_summaries_init$ft_ht
    mvar <- loc1[loc1$gmt >= tmin & loc1$gmt <= tmax,]
    for (j in 1:nrow(mvar)) {
      if (loc1$gmt >= tmin & loc1$gmt <= tmax){
        loc1$meandir[j] <- meandir
        loc1$sddirDEG[j] <- sddirDEG
        loc1$ani_n[j] <- ani_n
        loc1$ft_ht[j] <- ft_ht
      }
    }
  }
})

##############################
### Add strat_prop to loc1 ###
##############################

loc1$strat_prop <- NA # No. of dives that are in stratified water masses

##run a loop over the location data, grabbing the corresponding dive data
##2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
system.time({
  for(i in 1: (nrow(loc1))) {
    print(paste(i,nrow(loc1)-1),sep=" ")
    tmin <- loc1$gmt[i] - (3600*1.25)
    tmax <- loc1$gmt[i] + (3600*1.25)
    mvar <- filtered_divestats[filtered_divestats$start >= tmin & filtered_divestats$start <= tmax,]
    if (NROW(mvar %>% filter(Thermocline=='present'))>0) {
      new_df <- mvar %>% filter(Thermocline=='present')
      loc1$strat_prop[i] <- NROW(new_df)
    }
    else {
      loc1$strat_prop[i] <- 0
    }
  }
})

##############################
### Add strat_prop to loc1 ###
##############################

loc1$mean_Temp <- NA
loc1$dive_efficiency <- NA
loc1$deltaT <- NA

##run a loop over the location data, grabbing the corresponding dive data
##2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
system.time({
  for(i in 1: (nrow(loc1))) {
    print(paste(i,nrow(loc1)-1),sep=" ")
    tmin <- loc1$gmt[i] - (3600*1.25)
    tmax <- loc1$gmt[i] + (3600*1.25)
    mvar <- filtered_divestats[filtered_divestats$start >= tmin & filtered_divestats$start <= tmax,]
    loc1$mean_Temp[i] <- mvar$mean_Temp %>% mean(na.rm=T)
    loc1$dive_efficiency[i] <- mvar$dive_efficiency %>% mean(na.rm=T)
    loc1$deltaT[i] <- mvar$deltaT %>% mean(na.rm=T)
  }
})

##############################
### Add strat_prop to loc1 ###
##############################

loc1$for_effort = loc1$ht_rat*loc1$no_dives

##############################
### saveRDS loc1 ###
##############################
col_list = list('for_effort','mean_max_depth' ,'therm_depth','dive_efficiency','deltaT','strat_prop', 'hunting_time','mean_Temp','all.dur','lon','lat','diel_phase','no_dives','JDay')
loc1 <- loc1 %>% dplyr::select(col_list)
#saveRDS(loc1,file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")),compress = TRUE)





