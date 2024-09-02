## Assessing percentage of dives removed for final analysis. 

# Load packages -----------------------------------------------------------
library(purrr)
library(broom)
library(move)
#library(moveVis)
library(argosfilter)
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
# install.packages('nlme')
library(nlme)
# library(lmerTest)
library('mgcv')
library(car)
library(ggpubr)
#require(GGally)
rm(list = ls())

SealIDS = c(1,2,4,5,seq(13,25,1),31,47,seq(49,53,1),57,58,seq(62,70,1),109,110,112,113)
save_bsm_seg_df = "bsm_seg_df"
save_df_init_tmp2 = "df_init_tmp2"
save_divestats = "divestats"
save_loc1 = "loc1"
save_dbs = "dbs"
save_ndbs = "ndbs"
Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')
source("Add_Mdepth_hunting_fun.R")
all_dives_nrows <- data.frame("ID" = 0, "n_dives" = 0)
df_init_tmp2_nrows <- data.frame("ID" = 0, "n_dives" = 0)
divessummary_nrows <- data.frame("ID" = 0, "n_dives" = 0)
divestats_nrows <-  data.frame("ID" = 0, "n_dives" = 0)
bsm_seg_df_nrows <- data.frame("ID" = 0, "n_dives" = 0)
df_init_tmp2_nrows_4 <- data.frame("ID" = 0, "n_dives" = 0)

for (i in 1:length(SealIDS)) {
  
  sealID = SealIDS[i]
  print(sealID)
  
  save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")
  divessummary <- read.csv(file.path(save_divessummary,"divessummary.csv"),sep=',')
  divestats <- read_rds(file.path(save_divestats,paste(sealID,"_divestats.rds",sep = "")))
  save_loc1 = "loc1"
  loc1 <- read_rds(file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")))
  save_bsm_seg_df = "bsm_seg_df"
  bsm_seg_df <- read_rds(file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")))
  # Thermoclines1 <- read_csv(file.path(save_loc,paste("Thermoclines1.csv",sep = '')))
  df_init_tmp2 <- read_rds(file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")))
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
  
  
  #all_dives_nrowsi <- data.frame("sealID" = rep(0, (NROW(SealIDS) - 1)), "n_dives" = 0)
  #df_init_tmp2_nrowsi <- data.frame("sealID" = rep(0, (NROW(SealIDS) - 1)), "n_dives" = 0)
  #divessummary_nrowsi <- data.frame("sealID" = rep(0, (NROW(SealIDS) - 1)), "n_dives" = 0)
  all_dives_nrowsi <- data.frame("ID" = 0, "n_dives" = 0)
  df_init_tmp2_nrowsi <- data.frame("ID" = 0, "n_dives" = 0)
  divessummary_nrowsi <- data.frame("ID" = 0, "n_dives" = 0)
  divestats_nrowsi <- data.frame("ID" = 0, "n_dives" = 0)
  bsm_seg_df_nrowsi <-  data.frame("ID" = 0, "n_dives" = 0)
  
  all_dives_nrowsi$ID[1] <- sealID
  # all_dives_nrowsi$n_dives[1] <- NROW(all_dives)
  all_dives_nrowsi$n_dives[1] <- all_dives %>% ungroup() %>% group_by(num) %>% count() %>% NROW()
  all_dives_nrows = rbind(all_dives_nrows,all_dives_nrowsi)
  
  divestats_nrowsi$ID[1] <- sealID
  divestats_nrowsi$n_dives[1] <- NROW(divestats)
  divestats_nrows = rbind(divestats_nrows,divestats_nrowsi)
  
  df_init_tmp2_nrowsi$ID[1] <- sealID
  # df_init_tmp2_nrowsi$n_dives[1] <- NROW(df_init_tmp2)
  df_init_tmp2_nrowsi$n_dives[1] <- df_init_tmp2 %>%ungroup() %>% group_by(num) %>% count() %>% NROW()
  df_init_tmp2_nrows = rbind(df_init_tmp2_nrows,df_init_tmp2_nrowsi)
  
  divessummary_nrowsi$ID[1] <- sealID
  divessummary_nrowsi$n_dives[1] <- NROW(divessummary)
  divessummary_nrows = rbind(divessummary_nrows,divessummary_nrowsi)
  
  bsm_seg_df_nrowsi$ID[1] <- sealID
  # all_dives_nrowsi$n_dives[1] <- NROW(all_dives)
  bsm_seg_df_nrowsi$n_dives[1] <- bsm_seg_df %>% ungroup() %>% group_by(num) %>% count() %>% NROW()
  bsm_seg_df_nrows = rbind(bsm_seg_df_nrows,bsm_seg_df_nrowsi)
  
  df_init_tmp2_nrowsk <- data.frame("ID" = 0, "n_dives" = 0)
  
  df_init_tmp2_nrowsk$ID[1] <- sealID
  df_init_tmp2_nrowsk$n_dives[1] <- df_init_tmp2 %>% filter(max.d > 4) %>% ungroup() %>% group_by(num) %>% count() %>% NROW()
  df_init_tmp2_nrows_4 = rbind(df_init_tmp2_nrows_4,df_init_tmp2_nrowsk)
  
}
rm(bsm_seg_df,bsm_seg_df_nrowsi,divessummary,divessummary_nrowsi,all_dives,all_dives_nrowsi,df_init_tmp2,df_init_tmp2_nrowsi,divestats,divestats_nrowsi)
all_dives_nrows<-all_dives_nrows[2:40,]
write.csv(all_dives_nrows,file.path("all_dives_nrows.csv"))
bsm_seg_df_nrows<-bsm_seg_df_nrows[2:40,]
write.csv(bsm_seg_df_nrows,file.path("bsm_seg_df_nrows.csv"))
df_init_tmp2_nrows<-df_init_tmp2_nrows[2:40,]
write.csv(df_init_tmp2_nrows,file.path("df_init_tmp2_nrows.csv"))
divessummary_nrows<-divessummary_nrows[2:40,]
write.csv(divessummary_nrows,file.path("divessummary_nrows.csv"))
divestats_nrows<-divestats_nrows[2:40,]
write.csv(divestats_nrows,file.path("divestats_nrows.csv"))
df_init_tmp2_nrows_4<-df_init_tmp2_nrows[2:40,]
write.csv(df_init_tmp2_nrows_4,file.path("df_init_tmp2_nrows_4.csv"))

  
         
## All of the dives in raw data
sum(df_init_tmp2_nrows_4$n_dives)
## %dives retained after >4m and >60sec threshold filtering
mean(all_dives_nrows$n_dives/df_init_tmp2_nrows_4$n_dives*100)
sd(all_dives_nrows$n_dives/df_init_tmp2_nrows_4$n_dives*100)/sqrt(length(bsm_seg_df_nrows$n_dives))
sum(all_dives_nrows$n_dives)/sum(df_init_tmp2_nrows_4$n_dives)*100
sum(all_dives_nrows$n_dives)
## %dives retained after >4m and >60sec threshold filtering
mean(all_dives_nrows$n_dives/divestats_nrows$n_dives*100)
sd(all_dives_nrows$n_dives/divestats_nrows$n_dives*100)
sum(all_dives_nrows$n_dives)
## %dives retained after broken stick model
mean(bsm_seg_df_nrows$n_dives/all_dives_nrows$n_dives*100)
sd(bsm_seg_df_nrows$n_dives/all_dives_nrows$n_dives*100)/sqrt(length(all_dives_nrows$n_dives))
sum(bsm_seg_df_nrows$n_dives)
## %dives retained after filtering out foraging trips that are wrong
mean((dives %>% group_by(sealID) %>% count())$n/bsm_seg_df_nrows$n_dives*100)
bsm_seg_df_nrows$n_dives-(dives %>% group_by(sealID) %>% count())$n
sd((dives %>% group_by(sealID) %>% count())$n/bsm_seg_df_nrows$n_dives*100)/sqrt(length(bsm_seg_df_nrows$n_dives))
sum((dives %>% group_by(sealID) %>% count())$n)
## %dives retained after ignoring pdsi >=900sec (15min)
mean(100-(dives %>% filter(pdsi<900) %>% group_by(sealID) %>% count())$n/(dives %>% group_by(sealID) %>% count())$n*100)
sd(100-(dives %>% filter(pdsi<900) %>% group_by(sealID) %>% count())$n/(dives %>% group_by(sealID) %>% count())$n*100)/sqrt(length((dives %>% group_by(sealID) %>% count())$n))
sum((dives %>% filter(pdsi<900) %>% group_by(sealID) %>% count())$n)

# Adding ft summaries to my_fts -------------------------------------------

# Summarise and save
locs$dist_M <- as.numeric(st_distance(st_as_sf(locs,coords = c("lon", "lat"),crs=4326,remove = FALSE),sf_Marion))
ftcsv <- locs %>% group_by(season,sealID,ft) %>% summarise(mean_no_dives_per_loc = mean(no_dives),
                                                           sd_no_dives_per_loc = sd(no_dives),
                                                           cum_dives = sum(no_dives),
                                                           #Duration = max(JDay)-min(JDay),
                                                           Dist_trav = sum(distanceTrack(lat,lon)),
                                                           Dist_from_M = max(dist_M/1000),
                                                           prop_dist = Dist_from_M/Dist_trav,
                                                           cum_hunting_time = sum(hunting_time),
                                                           Duration_days = sum(diff(JDay))
                                                           ) 

# ftcsv <- ftcsv[ftcsv$mean_no_dives_per_loc!=0 & ftcsv$Dist_trav>20,]
write.csv(ftcsv,"ftcsv.csv")
## NB!! Fix fts manually in csv
# i.e. seal 64 ft 6 must be added.


##### Excluding foraging trips where distance from Marion never exceeds 40kms i.e. all overnight foraging trips including some other short trips.
dives <- dives %>% filter(!((sealID==21 & ft==1) | (sealID==53 & ft==2) | (sealID==57 & (ft==2 | ft==3 | ft==4 | ft==5)) |
                            (sealID==58 & ft==3) | (sealID==63 & ft==5) | 
                            (sealID==66 & (ft==2 | ft==3 | ft==4 | ft==5 | ft==6 | ft==7 | ft==8 | ft==9 | ft==10 | ft==11 | ft==12 | ft==13 | ft==14)) |
                            (sealID==67 & (ft==3 | ft==5)) | (sealID==113 & (ft==3 | ft==4 | ft==6 | ft==7 | ft==8 | ft==9))))
                            
                            


# Joining fts -------------------------------------------------------------
ftcsv$season <- NULL
colnames(my_fts)[1] <- 'sealID'
my_fts$sealID <- as.factor(my_fts$sealID)
ft_stats <- full_join(ftcsv,my_fts,by=c('sealID','ft'))
write.csv(ft_stats,"ft_stats.csv")

###### Simple ft table for thesis
sf_Marion <- data.frame("lon" = 37.746368, "lat" = -46.893361) %>% 
  st_as_sf(coords = c("lon", "lat"),crs=4326,remove = FALSE)
se <- function(x, ...) sqrt(var(x, ...)/length(x))
bouts = final_df %>% group_by(season,sealID,ft,bout) %>% count() %>% ungroup() %>% group_by(season,sealID,ft) %>% count() %>% ungroup() %>% group_by(season) %>% summarise(mean_bouts=mean(n), se_bouts = se(n))
Table <- ft_stats %>% ungroup() %>% group_by(season) %>%  filter(Dist_trav>40) %>% 
  summarise('Mean dist trav [km] +/- SE' = paste0(as.character(round(mean(Dist_trav),3)),' (',as.character(round(se(Dist_trav),3)),')'),
            'Mean trip length [days] +/- SE' = paste0(as.character(round(mean(trip.length),3)),' (',as.character(round(se(trip.length),3)),')'),
            'Median #dives per location'=as.character(round(median(mean_no_dives_per_loc),3)),
            'Mean ft #dives +/- SE' = paste0(as.character(round(mean(cum_dives),3)),' (',as.character(round(se(cum_dives),3)),')'),
            'Mean total hunting time +/- SE' = paste0(as.character(round(mean(cum_hunting_time),3)),' (',as.character(round(se(cum_hunting_time),3)),')')
            )
write.csv(Table,'ft_seasonal_table.csv')

# Simple seasonal stats in final analysis ---------------------------------------------------
## Dives per season and in totla before further analysis (PCA and Clustering)
NROW(dives)
dives %>% group_by(season) %>% count()

#ft per season
diving %>% group_by(season,sealID,ft) %>% count() %>% ungroup() %>% group_by(season) %>% count()
#Bouts per season
diving %>% group_by(season,sealID,ft,bout) %>% count() %>% ungroup() %>% group_by(season) %>% count()
#Dives per season
diving %>% group_by(season) %>% count()