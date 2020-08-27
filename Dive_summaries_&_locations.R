##############################################################################################################################
##################### Dive_summaries_&_locations.R: Is a file containing code to process fur seal diving data. ###############
#####################             A vertical area restricted search method is used.               ############################
##############################################################################################################################
# cntr+alt+r to run all

# setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc")
# setwd(choose.dir())
# fn <- choose.files()
# m <- read.csv(fn)

# Load packages -----------------------------------------------------------
# library(glob2rx)
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
library(dplyr)
library(ggplot2)
library(magrittr)
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

rm(list = ls())
sealID <- 1 # Used for saving file 
save_bsm_seg_df = "bsm_seg_df"
save_df_init_tmp2 = "df_init_tmp2"
save_divestats = "divestats"
save_filtered_divestats = "filtered_divestats"
save_loc1 = "loc1"
save_dbs = "dbs"
save_ndbs = "ndbs"


# dbs & ndbs --------------------------------------------------------------

# read_rds(file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")))
dbs <- read_rds(file.path(save_dbs,paste(sealID,"_dbs.rds",sep = "")))
ndbs <- read_rds(file.path(save_ndbs,paste(sealID,"_ndbs.rds",sep = "")))
### Foraging?: ################ NBBBBB!!!!!!!!! Check velocity before using threshold of 0.4m/s
#-----------
## Attribution of behaviour according to vertical sinuosity -- Remind that the sinuosity threshold used here was determined according
## to the histogram/density plot of vertical sinuosity for every BS segments of every dive
## so, before setting your threshold at 0.9, check if it suits your dataset (i.e after running the BS on all your dive)
if (nrow(dbs)!=0){
  dbs$velocity <- abs(((dbs$depth_end)-(dbs$depth_start))/dbs$dur) # Heerah et al (2015) - velocity of low res profile <0.4 = "hunting" 
  dbs$foraging <- "transit" ## "hunting" mode
  dbs$foraging[dbs$sinuosity < 0.9 & dbs$velocity < 0.4 & dbs$dur > 4] <- "hunting" ## According to Heerah, Hindell, Guinet, and Charrassin (2015) & Heerah (2014)
  dbs$foraging <- as.factor(dbs$foraging)
  dbs$bs_npoints <- "optimal"
  dbs$bs_npoints <- as.factor(dbs$bs_npoints)
}
if (nrow(ndbs)!=0){
  ndbs$velocity <- abs(((ndbs$depth_end)-(ndbs$depth_start))/ndbs$dur)
  ndbs$foraging <- "transit" ## "hunting" mode
  ndbs$foraging[ndbs$sinuosity < 0.9 & ndbs$velocity < 0.4 & ndbs$dur > 4] <- "hunting" ## According to Heerah, Hindell, Guinet, and Charrassin (2015) & Heerah (2014)
  ndbs$foraging <- as.factor(ndbs$foraging)
  # ndbs$bs_npoints <- paste(tm,"set",sep = "_")
  ndbs$bs_npoints <- paste(6,"set",sep = "_") #for use if loop stops (i.e. "tm" has not been defined). Here we define it as 6
  ndbs$bs_npoints <- as.factor(ndbs$bs_npoints)
}

# Add columns to dbs & ndbs ------------------------------------------------------

#########################################################################################################
######################### This is done for later analysis of segments within dives  #####################
#########################################################################################################
df_init_tmp2 <- read_rds(file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")))

################# adding "all.dur" - duration of the dives. ("dur" - is the duration of each segment within a dive)
if (nrow(dbs)!=0){
  dbs <- df_init_tmp2 %>% 
    select(max.d,bottom_depth,bottom_time,'%bt/dt') %>% 
    filter(row_number()==1) %>% 
    right_join(dbs %>% group_by(num) %>% dplyr::mutate("all.dur" = max(time_end) - min(time_start)),by = "num")
}
if (nrow(ndbs)!=0){
  ndbs <- df_init_tmp2 %>% 
    select(max.d,bottom_depth,bottom_time,'%bt/dt') %>% 
    filter(row_number()==1) %>% 
    right_join(ndbs %>% group_by(num) %>% dplyr::mutate("all.dur" = max(time_end) - min(time_start)),by = "num")
}
if (nrow(dbs)!=0){
  dbs$travel <- dbs$all.dur - dbs$bottom_time ## Travel time within a dive (transit time based on bottom phase method)
}
if (nrow(ndbs)!=0){
  ndbs$travel <- ndbs$all.dur - ndbs$bottom_time
}
if (nrow(dbs)!=0 | nrow(ndbs)!=0){
  bsm_seg_df <- rbind(dbs %>% ungroup(),ndbs %>% ungroup())
}
if (nrow(dbs)==0 & nrow(ndbs)!=0){
  bsm_seg_df <- ndbs
}
if (nrow(ndbs)==0 & nrow(dbs)!=0){
  bsm_seg_df <- dbs
}
bsm_seg_df$vdist <- abs(bsm_seg_df$depth_end - bsm_seg_df$depth_start)
saveRDS(bsm_seg_df,file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")),compress = TRUE)

# Saving important files  ------------------------------------------------------

#########################################################################################################
############################################## For csv files  ###########################################
#########################################################################################################
# setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc")
# saveRDS(bsm_seg_df,file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")),compress = TRUE)
# 
# 
system.time({
  bsm_seg_df <- read_csv(file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.csv",sep = "")))
  bsm_seg_df$X1 = NULL
  df_init_tmp2 <- read_csv(file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.csv",sep = "")))
  df_init_tmp2$X1 = NULL

  saveRDS(bsm_seg_df,file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")),compress = TRUE)
  saveRDS(df_init_tmp2,file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")),compress = TRUE)
})
# 
# setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc/df_init_tmp2")
# system.time{(
# readRDS(paste(sealID,"_df_init_tmp2.rds",sep = ""))
# })
# con <- gzfile(fil)
# readRDS(con)
# close(con)
# gzf
# Locations ---------------------------------------------------------------


#########################################################################################################
############################################## For rds files  ###########################################
#########################################################################################################

bsm_seg_df <- read_rds(file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")))
df_init_tmp2 <- read_rds(file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")))
bsm_seg_df <- bsm_seg_df %>% group_by(num)
df_init_tmp2 <- df_init_tmp2 %>% group_by(num) 
##############################################################################################################################
####################### Reading in seal tracks produced by Mia. This will be useful for horizontal   ###############
#######################         dives and averaging the dives that are > 60sec long.                           ###############
##############################################################################################################################
## Import tracks analysed by Mia
setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc")
tracks <- read.csv("Marion_FStracks_SSMresults_2009W_2015S.csv",sep = ";")
# str(tracks)

loc1 <-droplevels(subset(tracks,tracks$id == sealID)) %>% filter(gmt > first(df_init_tmp2$gmt) & gmt < last(df_init_tmp2$gmt))
# loc1$gmt <- strptime(loc1$gmt,format = "%Y-%m-%d %H:%M", tz = "GMT")
loc1$gmt <- as.POSIXct(loc1$gmt)
# diff(loc1$gmt)
# which(diff(loc1$gmt)>3) # where there is missing data


# Combine dive data with location data ------------------------------------

##############################################################################################################################
####################### Description: Combining potential foraging dives with locations. Collecting the nearest ###############
#######################         dives and averaging the dives that are > 60sec long.                           ###############
##############################################################################################################################

##########################  Restructuring and filtering dives first ########################## 
## Dive summary dataframe before adding information to the locations dataframe
## add column for transit and hunting time per dive according to BSM segments. 
## Add column for post dive surface interval (pdsi)
## Add start and end time of each dive

# NBBBBBBBB!!!!!!! gmt in df_init_tmp2 is not gmt. It is UTC!!! # But they are the same essentially


# divestats <- read_csv(file.path(save_divestats,paste(sealID,"_divestats.csv",sep = "")))
# divestats <- read_rds(file.path(save_divestats,paste(sealID,"_divestats.rds",sep = "")))

divestats <- bsm_seg_df %>% 
  group_by(num) %>%
  filter(foraging == "transit") %>%
  summarize("transit_time" = sum(dur),"Mvdist_err_transit"= mean(vdist)/2,"Mdepth_transit"=mean(mean_depth)) %>% 
  # Transit time different from travel time. Transit time = According to broken stick method. Travel time = According to bottom phase method
  # Mvdist_err_transit - mean error of the mean_depth per segment- across all transit segments of the dive
  # Mdepth_transit - mean of mean_depth of each transit segment for each dive - not really a good indicator of 
  full_join(bsm_seg_df %>% 
              group_by(num) %>%
              filter(foraging == "hunting") %>% #calculate the sum time of all hunting segments
              summarize("hunting_time" = sum(dur),"Mvdist_err_hunting"= mean(vdist)/2,"Mdepth_hunting"=mean(mean_depth))
            # Mvdist_err_transit - mean error of the mean_depth per segment- across all hunting segments of the dive
            # Mdepth_transit - mean of mean_depth of each hunting segment for each dive
            , by="num") %>% 
  arrange(num) %>% 
  #post dive surface interval between possible foraging dives (i.e. between dives >60s long & 4m deep)
  replace_na(list(hunting_time = 0, transit_time = 0, pdsi = NaN)) %>%  
  arrange(num) %>% 
  right_join(df_init_tmp2 %>% 
               filter(row_number()==1) %>% 
               select(-Time,-Depth,-Temperature,-Light.Level) %>% 
               mutate("start" = as.POSIXct(strptime(gmt,format = "%Y-%m-%d %H:%M", tz = "GMT"))) %>% 
               select(-gmt,-cor.depth)
             ,by = "num") %>% 
  mutate(hunting_time = replace_na(hunting_time, 0), transit_time = ifelse(is.na(transit_time),all.dur,transit_time)) %>% 
  full_join(tibble(df_init_tmp2 %>% 
                     filter(max.d > 4, all.dur > 20) %>% 
                     select(num) %>% 
                     unique(),
                   "pdsi" = (((df_init_tmp2 %>% filter(max.d > 4, all.dur > 20) %>% slice(1))$Time)[-1] - ((df_init_tmp2 %>% filter(max.d > 4, all.dur > 20) %>% slice(n()))$Time))))


divestats$end <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + (df_init_tmp2 %>% slice(n()))$Time
divestats$dive_efficiency <- (divestats$bottom_time)/(divestats$all.dur + divestats$pdsi) #According to Lee et al, 2015 on Gentoo penguins
# Check if any weird data - divestats %>% filter(transit_time==0,hunting_time>0)  

##########################  Adding lat, lon & X to divestats ########################## 
## Set up dataframes  to add columns lat, lon, X. X is the indexer to join lat lon from loc1 with that of divestats
# loc1$X <- as.integer(loc1$X) ## Should have been done already above
tmin <- loc1$gmt
z <- seq(length(tmin)) ## Have to redefine z for divestats
z <- z[1:(length(z)-1)]
z <- cut(divestats$start,breaks = tmin, labels = z)
divestats <- divestats %>% 
  mutate("X" = as.integer(z)) %>% 
  left_join(loc1 %>% select(X,lon,lat),by = "X")
divestats[is.na(divestats)] <- 0
divestats <- divestats %>% filter(lat !=0) 

##########################  Adding lat, lon to bsm_seg_df ############################################### 
## Set up dataframes  to add columns lat, lon. num is the indexer to join lat lon from divestats with that of bsm_seg_df
tmin <- loc1$gmt
z <- seq(length(tmin)) ## Have to redefine z for divestats
z <- z[1:(length(z)-1)]
bsm_seg_df$start <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + bsm_seg_df$time_start
z <- cut(bsm_seg_df$start,breaks = tmin, labels = z)
bsm_seg_df <- bsm_seg_df %>% 
  ungroup() %>%
  left_join(divestats %>% select(num,lon,lat),by = "num") %>% 
  ungroup() %>% 
  mutate("X" = as.integer(z))

#########################################################################################################

#########################################################################################################
############################################## Local time  ##############################################
#########################################################################################################
## Change time to local time using location columns
source("Mydielprep.R") # NOAA notes that for latitudes less than +/- 72 degrees accuracy is approximately one minute


loc1 <- diel.loc(loc1)
divestats <- diel.divestats(divestats)
bsm_seg_df <- diel.bsm(bsm_seg_df)

#########################################################################################################

## filtered dive summaries that may have foraging
filtered_divestats <-  divestats %>% filter(max.d > 4, all.dur>60)
## Adding whether dive is a hunting dive or not according to ht_rat (ratio of hunting time to dive time)
filtered_divestats$ht_rat <- filtered_divestats$hunting_time/filtered_divestats$all.dur  #hunting to transit ratio
divestats$ht_rat <- divestats$hunting_time/divestats$all.dur

h <- 0.25 #threshold for "ht_rat' above which seal is hunting in the dive (Spends > 25% of the dive hunting) 
# p = % of dives where hunting time is considerably large enough that the dive can be seen as a foraging dive
p <- length(filtered_divestats$ht_rat[filtered_divestats$ht_rat>h])/length(filtered_divestats$ht_rat)
# p should be around 80%

#### Adding "hunt_dive" column to divestats and filtered_divestats
divestats <- filtered_divestats %>% 
  filter(hunting_time>10) %>% 
  filter(hunting_time>30 | max.d>40 |ht_rat > h) %>% 
  mutate("hunt_dive" = "hunting") %>%
  select(num,hunt_dive) %>% 
  right_join(divestats,by="num") %>% 
  replace_na(list(hunt_dive = "transit"))

filtered_divestats <- filtered_divestats %>% 
  filter(hunting_time>15) %>% 
  filter(hunting_time>30 | max.d>40 |ht_rat > h) %>% 
  mutate("hunt_dive" = "hunting") %>% 
  select(num,hunt_dive) %>% 
  right_join(filtered_divestats,by="num") %>% 
  replace_na(list(hunt_dive = "transit"))




# filtered_divestats$max.d[filtered_divestats$hunt_dive=="transit"] %>% hist()
# filtered_divestats$max.d[filtered_divestats$hunt_dive=="transit"] %>% length()



##########################  Group dives closest to locations and perform summary stats ##########################

## get mean values for each variables 4 hours either side of each location
## the code below show how to it for bottom time. You need to repeat this for the other variables


#set up empty variables
loc1$all.dur <- NA  
loc1$mean_max.d <- NA
loc1$bottom_depth <- NA
loc1$bottom_time <- NA
loc1$'%bt/dt' <- NA
loc1$max_max.d <- NA
loc1$hunting_time <- NA
loc1$transit_time <- NA
loc1$ht_rat <- NA
loc1$no_dives <- NA

##run a loop over the location data, grabbing the corresponding dive data
##2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
system.time({
  for(i in 1: (nrow(loc1))) {
    print(paste(i,nrow(loc1)-1),sep=" ")
    tmin <- loc1$gmt[i] - (3600*1.25)
    tmax <- loc1$gmt[i] + (3600*1.25)
    mvar <- filtered_divestats[filtered_divestats$start >= tmin & filtered_divestats$start <= tmax,]
    loc1$'all.dur'[i] <- mvar$'all.dur' %>% mean(na.rm=T)
    loc1$mean_max.d[i] <- mvar$max.d %>% mean(na.rm=T)
    loc1$bottom_depth[i] <- mvar$bottom_depth %>% mean(na.rm=T)
    loc1$bottom_time[i] <- mvar$bottom_time %>% mean(na.rm=T)
    loc1$'%bt/dt'[i] <- mvar$'%bt/dt' %>% mean(na.rm=T)
    loc1$max_max.d[i] <- mvar$max.d %>% max() #replace_na(NaN)
    loc1$hunting_time[i] <- mvar$hunting_time %>% sum(na.rm=T)
    loc1$transit_time[i] <- mvar$transit_time %>% sum(na.rm=T)
    loc1$ht_rat[i] <- mvar$ht_rat %>% mean(na.rm=T)
    loc1$no_dives[i] <- NROW(mvar)
  }
})
loc1$max_max.d <- loc1$max_max.d %>% replace(loc1$max_max.d == -Inf,NaN) #replaces infinities created by loop with NaN values in max_max.d
loc1$heffort <- log(loc1$hunting_time/loc1$no_dives)
loc1$heffort <- loc1$heffort %>% replace(loc1$heffort == -Inf,NaN)
loc1[is.na(loc1)] <- 0

# Testing for errors in data and indicatoion of correct filtering ("filtered_divestats"). 
# Where has hunting time been calculated (for dives >4m & >15sec), but dives close to location are >4m and 60sec
# which(is.na(loc1$dur) & loc1$hunting_time!=0)



##############################################################################
## Map the distribution of behaviours
ggplot() + 
  geom_point(aes(x = lon , y = lat, color = hunting_time, size = heffort), data = loc1)



# Saving rest of files ----------------------------------------------------

#########################################################################################################
############################################## Saving files as rds ######################################
#########################################################################################################

saveRDS(bsm_seg_df,file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")),compress = TRUE)
saveRDS(df_init_tmp2,file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")),compress = TRUE)
saveRDS(divestats,file.path(save_divestats,paste(sealID,"_divestats.rds",sep = "")),compress = TRUE)
saveRDS(filtered_divestats,file.path(save_filtered_divestats,paste(sealID,"_filtered_divestats.rds",sep = "")),compress = TRUE)
saveRDS(loc1,file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")),compress = TRUE)




