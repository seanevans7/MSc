##############################################################################################################################
##################### Sean_fseal_VARS.R: Is a file containg code to process fur seal diving data. ############################
##############################################################################################################################

##############################################################################################################################
############################################# Post-processing of dives #######################################################
#############################################      By Sean Evans       #######################################################
##############################################################################################################################

To do list:
  Check therm_depth_diff is correct i.e. There shouldn't be negative values?'

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
# All but #36 and #48
SealIDS = c(1,2,4,5,seq(13,25,1),31,47,seq(49,53,1),57,58,seq(62,70,1),109,110,112,113)
#SealIDS = c(1,2,4,5,seq(13,18,1),seq(20,25,1),31,49,50,51,52,57,63,65,68,69,110,111,112,113)
#SealIDS = c(68,69,110,111,112,113)

Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')
source("Add_Mdepth_hunting_fun.R")

for (i in 1:length(SealIDS)) {
  
  sealID = SealIDS[i]
  print(sealID)
  
  save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")
  divessummary <- read.csv(file.path(save_divessummary,"divessummary.csv"),sep=',')
  save_loc1 = "loc1"
  loc1 <- read_rds(file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")))
  save_bsm_seg_df = "bsm_seg_df"
  bsm_seg_df <- read_rds(file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")))
  
  
  save_all_dives = "all_dives"
  all_dives <- read_rds(file.path(save_all_dives,paste(sealID,"_all_dives.rds",sep = "")))
  
  #############################################################################
  # Correcting start column -----------------------------------------------------
  #############################################################################
  divessummary <- (divessummary %>% dplyr::select(-start) %>% right_join(all_dives %>% 
                                                                     filter(row_number()==1) %>% 
                                                                     mutate("start" = as.POSIXct(strptime(gmt,format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))) %>% 
                                                                     select(start),
                                                                   by='num'))[-1,]
  
  
  #############################################################################
  # Add columns to divessummary -----------------------------------------------------
  #############################################################################
  
  #################### Recalibrating divestats times #################
  #divessummary
  divessummary$start = force_tz(as.POSIXct(ymd_hms(divessummary$start)), tzone = "GMT", roll = FALSE)
  divessummary$end = force_tz(as.POSIXct(ymd_hms(divessummary$end)), tzone = "GMT", roll = FALSE)
  divessummary$local_time = force_tz(as.POSIXct(ymd_hms(divessummary$local_time)), tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$sunrise = force_tz(as.POSIXct(ymd_hms(divessummary$sunrise)), tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$sunset = force_tz(as.POSIXct(ymd_hms(divessummary$sunset)), tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$dawn = force_tz(as.POSIXct(ymd_hms(divessummary$dawn)), tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
  divessummary$dusk = force_tz(as.POSIXct(ymd_hms(divessummary$dusk)), tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600
  
  #############
  divessummary <- Add_Mdepth_hunting(divessummary,bsm_seg_df)
  
  #############
  Fts_summaries_init = Fts_summaries %>% filter(id == sealID) %>% dplyr::select(ft.start,ft.end,meandir,sddirDEG,ani_n)
  
  # Fts_summaries_init$ft_ht <- NA # sum of hunting time for all dives within this foraging trip
  # 
  ##run a loop over the foraging trip data, grabbing the corresponding dive data
  # system.time({
  #   for(j in 1:nrow(Fts_summaries_init)) {
  #     print(paste(j,nrow(Fts_summaries_init)-1),sep=" ")
  #     tmin <- as.POSIXct(Fts_summaries_init$'ft.start'[j])
  #     tmax <- as.POSIXct(Fts_summaries_init$'ft.end'[j])
  #     mvar <- divessummary[divessummary$start >= tmin & divessummary$start <= tmax,]
  #     Fts_summaries_init$ft_ht[j] <- mvar %>% filter(hunting_time>0) %>% dplyr::select(hunting_time) %>% sum()
  #   }
  # })
  
  ##############################
  ### Add ft to divessummary ###
  ##############################
  
  divessummary$ft_all <- NA # Adding ft column to divestats for later stats
  ##run a loop over the divessummary data, grabbing the corresponding foraging trip id
  system.time({
    for(j in 1: (nrow(Fts_summaries_init))) {
      # print(paste(j,nrow(Fts_summaries_init)-1),sep=" ")
      tmin <- as.POSIXct(Fts_summaries_init$'ft.start'[j], tz = 'GMT')
      tmax <- as.POSIXct(Fts_summaries_init$'ft.end'[j], tz = 'GMT')+24*3600
      divessummary$ft_all[divessummary$start>=tmin & divessummary$start<=tmax] = j
    }
  })

  # Add column for bouts - differentiated by 6 hours for all dives
  time_diff <- difftime(divessummary$start, lag(divessummary$start, default = divessummary$start[1]), units = "sec")
  divessummary$bout_no <- cumsum(ifelse(time_diff<3600*6,0,1))
  
  #######################################################
  ### Add max_diff_Therm & hunt_diff_Therm ###
  #######################################################
  
  divessummary <- divessummary %>%
    mutate(therm_depth = ifelse(Therm_dep>0 & Tmld>0 & abs(Therm_dep-Tmld)<=10, (Therm_dep+Tmld)/2,
                                ifelse(Therm_dep > 0 & is.na(Tmld), NA, # was Therm_dep before NA
                                       ifelse(Tmld > 0 & is.na(Therm_dep), Tmld,
                                              ifelse(Therm_dep>0 & Tmld>0 & abs(Therm_dep-Tmld)>10, Therm_dep,NA)))),
           diff_therm = ifelse(Therm_dep > 0 & Tmld >0, (Therm_dep-Tmld),NA), #error estimate
           hunt_diff_Therm = Mdepth_hunting-therm_depth,
           max_diff_Therm = max.d-therm_depth)
  
  #####################
  ### Add hARS_mode ###
  #####################
  
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
  ### filtered_divestats ###
  ##############################
  
  filtered_divestats <- divessummary %>% filter(max.d >4, all.dur>60)
  
  # Add column for bouts - differentiated by 6 hours for filtered dives
  time_diff <- difftime(filtered_divestats$start, lag(filtered_divestats$start, default = filtered_divestats$start[1]), units = "sec")
  filtered_divestats$bout <- cumsum(ifelse(time_diff<3600*6,0,1))
  
  ##############################
  ### Add ft to divessummary ###
  ##############################
  
  filtered_divestats$ft <- NA # Adding ft column to divestats for later stats
  ##run a loop over the divessummary data, grabbing the corresponding foraging trip id
  system.time({
    for(j in 1: (nrow(Fts_summaries_init))) {
      # print(paste(j,nrow(Fts_summaries_init)-1),sep=" ")
      tmin <- as.POSIXct(Fts_summaries_init$'ft.start'[j], tz = 'GMT')
      tmax <- as.POSIXct(Fts_summaries_init$'ft.end'[j], tz = 'GMT')+24*3600
      filtered_divestats$ft[filtered_divestats$start>=tmin & filtered_divestats$start<=tmax] = j
    }
  })
  
  ############################
  ### saveRDS divessummary ###
  ############################
  
  col_list = c('sealID','ft','bout','start','lon','lat','diel_phase','JDay','all.dur','distances..m.','mean_Temp','deltaT','dive_efficiency','ht_rat','hARS_mode','max.d','Mdepth_hunting','Thermocline','hunt_diff_Therm','max_diff_Therm','X','Nt','pdsi','bottom_time')
  
  filtered_divestats <- filtered_divestats %>% dplyr::select(col_list)
  saveRDS(filtered_divestats,file.path(save_divessummary,paste(sealID,"_filtered_divestats.rds",sep = "")),compress = TRUE)
  
  
  
  #############################################################################
  # Add columns to loc1 -----------------------------------------------------
  #############################################################################
  
  ############################################
  ### Add meandir, sddirDEG, ani_n to loc1 ###
  ############################################
  
  loc1$meandir <- NA
  loc1$sddirDEG <- NA
  loc1$ani_n <- NA
  #loc1$ft_ht <- NA
  
  system.time({
    for(j in 1:nrow(Fts_summaries_init)) {
      # print(paste(j,nrow(Fts_summaries_init)-1),sep=" ")
      tmin <- as.POSIXct(Fts_summaries_init$'ft.start'[j], tz = 'GMT')
      tmax <- as.POSIXct(Fts_summaries_init$'ft.end'[j], tz = 'GMT')
      meandir = Fts_summaries_init$meandir
      sddirDEG = Fts_summaries_init$sddirDEG
      ani_n = Fts_summaries_init$ani_n
      #ft_ht = Fts_summaries_init$ft_ht
      mvar <- loc1[loc1$gmt >= tmin & loc1$gmt <= tmax,]
      for (z in 1:nrow(mvar)) {
        if (loc1$gmt >= tmin & loc1$gmt <= tmax){
          loc1$meandir[z] <- meandir
          loc1$sddirDEG[z] <- sddirDEG
          loc1$ani_n[z] <- ani_n
          #loc1$ft_ht[z] <- ft_ht
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
    for(j in 1: (nrow(loc1))) {
      # print(paste(j,nrow(loc1)-1),sep=" ")
      tmin <- loc1$gmt[j] - (3600*1.25)
      tmax <- loc1$gmt[j] + (3600*1.25)
      mvar <- filtered_divestats[filtered_divestats$start >= tmin & filtered_divestats$start <= tmax,]
      if (NROW(mvar %>% filter(Thermocline=='present'))>0) {
        new_df <- mvar %>% filter(Thermocline=='present')
        loc1$strat_prop[j] <- NROW(new_df)
      }
      else {
        loc1$strat_prop[j] <- 0
      }
    }
  })
  
  ##############################
  ### Add means to loc1 ###
  ##############################
  
  loc1$mean_Temp <- NA
  loc1$dive_efficiency <- NA
  loc1$deltaT <- NA
  
  ##run a loop over the location data, grabbing the corresponding dive data
  ##2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
  system.time({
    for(j in 1: (nrow(loc1))) {
      # print(paste(j,nrow(loc1)-1),sep=" ")
      tmin <- loc1$gmt[j] - (3600*1.25)
      tmax <- loc1$gmt[j] + (3600*1.25)
      mvar <- filtered_divestats[filtered_divestats$start >= tmin & filtered_divestats$start <= tmax,]
      loc1$mean_Temp[j] <- mvar$mean_Temp %>% mean(na.rm=T)
      loc1$dive_efficiency[j] <- mvar$dive_efficiency %>% mean(na.rm=T)
      loc1$deltaT[j] <- mvar$deltaT %>% mean(na.rm=T)
    }
  })
  
  ##############################
  ### Add for_effort to loc1 ###
  ##############################
  
  loc1$for_effort = loc1$ht_rat*loc1$no_dives
  
  ##############################
  ### saveRDS loc1 ###
  ##############################
  col_list = c('id','ft','meandir','for_effort','mean_max.d' ,'dive_efficiency','deltaT','strat_prop', 'hunting_time','mean_Temp','all.dur','lon','lat','diel_phase','no_dives','JDay')
  loc1 <- loc1 %>% dplyr::select(col_list)
  saveRDS(loc1,file.path(save_divessummary,paste(sealID,"_loc.rds",sep = "")),compress = TRUE)
  
  
}


########################################################################################################
########################################################################################################
########################################################################################################


# Fixing 'start' column in divestats and filtered_divestats ----------------------

for (i in 1:length(SealIDS)) {
  
  sealID = SealIDS[i]
  
  save_divestats = "divestats"
  divestats <- read_rds(file.path(save_divestats,paste(sealID,"_divestats.rds",sep = "")))
  
  save_all_dives = "all_dives"
  all_dives <- read_rds(file.path(save_all_dives,paste(sealID,"_all_dives.rds",sep = "")))
  
  divestats <- (divestats %>% dplyr::select(-start) %>% inner_join(all_dives %>% 
                                                                     filter(row_number()==1) %>% 
                                                                     mutate("start" = as.POSIXct(strptime(gmt,format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))) %>% 
                                                                     select(start),
                                                                   by='num'))
  
  save_new_divestats = 'new_divestats'
  saveRDS(divestats,file.path(save_new_divestats,paste(sealID,"_new_divestats.rds",sep = "")),compress = TRUE)
}



########################################################################################################
########################################################################################################
########################################################################################################

# Extracting all_dives from df_init_tmp2  ---------------------------------
SealIDS = c(1,2,4,5,seq(13,25,1),31,36,seq(47,53,1),57,58,seq(62,70,1),seq(109,113,1))

for (i in 1:length(SealIDS)) {
  print(i)
  sealID = SealIDS[i]
  save_df_init_tmp2 = "df_init_tmp2"
  df_init_tmp2 <- read_rds(file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")))
  save_all_dives = "all_dives"
  
  save_loc1 = "loc1"
  loc1 <- read_rds(file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")))
  
  save_bsm_seg_df = "bsm_seg_df"
  bsm_seg_df <- read_rds(file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")))
  
  if (length(which(colnames(df_init_tmp2) == 'Temperature' | colnames(df_init_tmp2) == 'External.Temp'))==1) {
    if (length(which(colnames(df_init_tmp2) == 'Temperature')==1)) {
      df_init_tmp2 <- df_init_tmp2 %>% rename("External.Temp" = "Temperature")
    }
  }
  
  all_dives = df_init_tmp2 %>% filter(max.d>4 & all.dur>60) %>% dplyr::select(max.d,all.dur,cor.depth,num,Time,External.Temp,gmt)
  
  if ((bsm_seg_df$num %>% unique() %>% length()) != all_dives$num %>% unique() %>% length()){
    all_dives <- all_dives %>% filter(gmt>first(loc1$gmt) & gmt<last(loc1$gmt))
  }
  
  saveRDS(all_dives,file.path(save_all_dives,paste(sealID,"_all_dives.rds",sep = "")),compress = TRUE)
}

# Single seal
sealID = 1
save_df_init_tmp2 = "df_init_tmp2"
df_init_tmp2 <- read_rds(file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")))
save_all_dives = "all_dives"

save_loc1 = "loc1"
loc1 <- read_rds(file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")))

save_bsm_seg_df = "bsm_seg_df"
bsm_seg_df <- read_rds(file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")))

if (length(which(colnames(df_init_tmp2) == 'Temperature' | colnames(df_init_tmp2) == 'External.Temp'))==1) {
  if (length(which(colnames(df_init_tmp2) == 'Temperature')==1)) {
    df_init_tmp2 <- df_init_tmp2 %>% rename("External.Temp" = "Temperature")
  }
}

all_dives = df_init_tmp2 %>% filter(max.d>4 & all.dur>60) %>% dplyr::select(max.d,all.dur,cor.depth,num,Time,External.Temp,gmt)

if ((bsm_seg_df$num %>% unique() %>% length()) != all_dives$num %>% unique() %>% length()){
  all_dives <- all_dives %>% filter(gmt>first(loc1$gmt) & gmt<last(loc1$gmt))
}


