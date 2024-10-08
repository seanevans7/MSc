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

















rm(list = ls())

Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')

sealID = 1



save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")
divessummary <- read.csv(file.path(save_divessummary,"divessummary.csv"),sep=',')
save_loc1 = "loc1"
loc1 <- read_rds(file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")))


save_all_dives = "all_dives"
all_dives <- read_rds(file.path(save_all_dives,paste(sealID,"_all_dives.rds",sep = "")))

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

divessummary$ft <- NA # Adding ft column to divestats for later stats
##run a loop over the divessummary data, grabbing the corresponding foraging trip id
system.time({
  for(j in 1: (nrow(Fts_summaries_init))) {
    print(paste(j,nrow(Fts_summaries_init)-1),sep=" ")
    tmin <- as.POSIXct(Fts_summaries_init$'ft.start'[j])
    tmax <- as.POSIXct(Fts_summaries_init$'ft.end'[j])+24*3600
    divessummary$ft[divessummary$start>=tmin & divessummary$start<=tmax] = j
  }
})
# Add column for bouts
time_diff <- difftime(divessummary$start, lag(divessummary$start, default = divessummary$start[1]), units = "sec")
divessummary$bout <- cumsum(ifelse(time_diff<3600*6,0,1))

#######################################################
### Add max_diff_Therm & hunt_diff_Therm ###
#######################################################

divessummary <- divessummary %>%
  mutate(therm_depth = ifelse(Therm_dep>0 & Tmld>0 & abs(Therm_dep-Tmld)<=10, (Therm_dep+Tmld)/2,
                              ifelse(Therm_dep > 0 & is.na(Tmld), Therm_dep,
                                     ifelse(Tmld > 0 & is.na(Therm_dep), Tmld,
                                            ifelse(Therm_dep>0 & Tmld>0 & abs(Therm_dep-Tmld)>10, Therm_dep,NA)))),
         diff_therm = ifelse(Therm_dep > 0 & Tmld >0, (Therm_dep-Tmld),NA),
         hunt_diff_Therm = Mdepth_hunting-therm_depth,
         max_diff_Therm = therm_depth-max.d)

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

############################
### saveRDS divessummary ###
############################

col_list = c('sealID','ft','bout','start','lon','lat','diel_phase','JDay','all.dur','distances..m.','mean_Temp','deltaT','dive_efficiency','ht_rat','hARS_mode','max.d','Mdepth_hunting','Thermocline','hunt_diff_Therm','max_diff_Therm','X')
filtered_divestats <- divessummary %>% filter(max.d >4, all.dur>60)
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
    print(paste(j,nrow(Fts_summaries_init)-1),sep=" ")
    tmin <- as.POSIXct(Fts_summaries_init$'ft.start'[j])
    tmax <- as.POSIXct(Fts_summaries_init$'ft.end'[j])
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
    print(paste(j,nrow(loc1)-1),sep=" ")
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
### Add strat_prop to loc1 ###
##############################

loc1$mean_Temp <- NA
loc1$dive_efficiency <- NA
loc1$deltaT <- NA

##run a loop over the location data, grabbing the corresponding dive data
##2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
system.time({
  for(j in 1: (nrow(loc1))) {
    print(paste(j,nrow(loc1)-1),sep=" ")
    tmin <- loc1$gmt[j] - (3600*1.25)
    tmax <- loc1$gmt[j] + (3600*1.25)
    mvar <- filtered_divestats[filtered_divestats$start >= tmin & filtered_divestats$start <= tmax,]
    loc1$mean_Temp[j] <- mvar$mean_Temp %>% mean(na.rm=T)
    loc1$dive_efficiency[j] <- mvar$dive_efficiency %>% mean(na.rm=T)
    loc1$deltaT[j] <- mvar$deltaT %>% mean(na.rm=T)
  }
})

##############################
### Add strat_prop to loc1 ###
##############################

loc1$for_effort = loc1$ht_rat*loc1$no_dives

##############################
### saveRDS loc1 ###
##############################
col_list = c('id','for_effort','mean_max.d' ,'dive_efficiency','deltaT','strat_prop', 'hunting_time','mean_Temp','all.dur','lon','lat','diel_phase','no_dives','JDay')
loc1 <- loc1 %>% dplyr::select(col_list)
saveRDS(loc1,file.path(save_divessummary,paste(sealID,"_loc.rds",sep = "")),compress = TRUE)


























# Checking and correcting single seal start column ----------------------------------------------------

# Change divestats.rds, divessummary.csv and filtered_divestats.rds
rm(list = ls())
sealID = 1

save_divestats = "divestats"
divestats <- read_rds(file.path(save_divestats,paste(sealID,"_divestats.rds",sep = "")))

save_df_init_tmp2 = "df_init_tmp2"
df_init_tmp2 <- read_rds(file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")))

save_bsm_seg_df = "bsm_seg_df"
bsm_seg_df <- read_rds(file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")))

save_filtered_divestats = paste("Plots & Dive Tables/Seal",sealID, sep = "")
filtered_divestats <- read_rds(file.path(save_filtered_divestats,paste(sealID,"_filtered_divestats.rds",sep = '')))

divestats <- (divestats %>% dplyr::select(-start) %>% right_join(df_init_tmp2 %>% 
  filter(row_number()==1) %>% 
  select(-Time,-Depth,-Temperature,-Light.Level) %>% 
  mutate("start" = as.POSIXct(strptime(gmt,format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))) %>% 
  select(start),
  by='num'))[-1,]

save_dbs = "dbs"
save_ndbs = "ndbs"
dbs <- read_rds(file.path(save_dbs,paste(sealID,"_dbs.rds",sep = "")))
ndbs <- read_rds(file.path(save_ndbs,paste(sealID,"_ndbs.rds",sep = "")))

if (nrow(dbs)!=0 | nrow(ndbs)!=0){
  bsm_seg_df1 <- rbind(dbs %>% ungroup(),ndbs %>% ungroup())
}


save_loc1 = "loc1"
loc1 <- read_rds(file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")))

tmin <- c(loc1$gmt - 1.25*3600,force_tz(as.POSIXct(last(loc1$gmt) + 1.25*3600,tz='GMT'), tzone = "GMT", roll = FALSE))
z <- seq(length(tmin)-1)
bsm_seg_df1$start <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + bsm_seg_df1$time_start
bsm_seg_df1$start <- force_tz(bsm_seg_df1$start, tzone = "GMT", roll = FALSE)
z <- cut(bsm_seg_df1$start,breaks = tmin, labels = z)

bsm_seg_df2 <- bsm_seg_df1 %>% 
  ungroup() %>%
  left_join(divestats %>% select(num,lon,lat),by = "num") %>% 
  ungroup() %>% 
  mutate("X" = as.integer(z))

bsm <- bsm_seg_df1 %>% filter(start > first(loc1$gmt) & start < last(loc1$gmt))

# new_divestats
save_new_divestats = 'new_divestats'
new_divestats <- read_rds(file.path(save_new_divestats,paste(sealID,"_new_divestats.rds",sep = "")))

#Adding local_times for seal 36

source("Mydielprep_fun.R")

dives <- divestats %>% filter(start > first(loc1$gmt) & start < last(loc1$gmt))
dives <- diel.divestats(dives)





save_all_dives = "all_dives"
all_dives <- read_rds(file.path(save_all_dives,paste(sealID,"_all_dives.rds",sep = "")))

divestats <- (divestats %>% dplyr::select(-start) %>% inner_join(all_dives %>% 
                                                                   filter(row_number()==1) %>% 
                                                                   mutate("start" = as.POSIXct(strptime(gmt,format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))) %>% 
                                                                   select(start),
                                                                 by='num'))

save_new_divestats = 'new_divestats'
saveRDS(divestats,file.path(save_new_divestats,paste(sealID,"_new_divestats.rds",sep = "")),compress = TRUE)

