SealIDS = c(1,2,4,5,seq(13,25,1),31,47,seq(49,53,1),57,58,seq(62,70,1),109,110,112,113)

# Reading data
save_all <- 'Plots & Dive Tables/'
dives <- readRDS(file.path(save_all,"dives.rds"))

Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')
source("Add_Mdepth_hunting_fun.R")

for (i in 1:length(SealIDS)) {
  
  sealID = SealIDS[i]
  print(sealID)
  save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")

  Fts_summaries_init = Fts_summaries %>% filter(id == sealID) %>% dplyr::select(ft.start,ft.end,meandir,sddirDEG,ani_n)
  save_loc1 = "loc1"
  loc1 <- read_rds(file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")))
  filtered_divestats <- dives[dives$sealID==sealID,]
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
        loc1$strat_prop[j] <- NROW(new_df)/NROW(mvar)
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
