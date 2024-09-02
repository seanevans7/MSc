##############################################################################################################################
##################### Sean_fseal_VARS.R: Is a file containg code to process fur seal diving data. ############################
#####################             A vertical area restricted search method is used.               ############################
##############################################################################################################################
# cntr+alt+r to run all

rm(list = ls())
# Which seal?
sealtag <- "A240" # used for accessing and loading file 
# (If error = "invalid description argument" then check that seal doesn't have two files associated with it)
sealID <- 56 # Used for saving file 


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

# Set Working directory and import myFunctions ------------------------------------
# directories
setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/Computing docs")
source("myFunctions.R")
setwd("~/2020/MSc/Computing/Fur seals/Dive data for Sean Evans")
data_path = "Extracted raw dive data - from instrument helper/Fur seal data/dt_before_TDR"
# Saving files in MSc
save_bsm_seg_df = "bsm_seg_df" #Broken Stick Model segments of shortlisted dives for both Optimal and Set npoint methods
save_df_init_tmp2 = "df_init_tmp2" #dataframe for all dives in total before shortlisting to analyze
save_divestats = "divestats" #shortlisted dives chosen based on duration and depth filters: max.d > 4, all.dur > 60
save_dbs = "dbs" #Broken stick dataframe - Optimal npoints
save_ndbs = "ndbs" #Broken stick dataframe - Set npoints
# Data Structuring -------------------------------------------------

##############################################################################################################################
##################### Description: Performing Zero offset correction analysis on dives to omit drift  ########################
##################### in pressure sensor. Using set offset method at 2hr intervals                ############################
##############################################################################################################################

## List of each seal ".Rda" files in data_path directory
fs_list <- list.files(data_path, pattern = ".Rda")
system.time({ 
  # Setting up Zero Offset Correction function
  zoc <- function(x, zval = 10, blim = -5) {
    h <- hist(x[x < zval & x > blim], breaks = seq(blim, zval, 1),plot = FALSE)
    x - h$breaks[which.max(h$counts)]
  }
  ## sets surface intervals within the "surface noise" value to 0
  sn <- 1
  ## Loading specified seal file
  load(paste(data_path,fs_list[grep(sealtag,fs_list)],sep = "/")) ## should be a dt dataframe
  #load('Extracted raw dive data - from instrument helper/Fur seal data/dt_before_TDR/70_MP_2013W_10L0221_A482_5sec_a.Rda')
  df <- dt
  rm(dt) ## remove dt for safekeeping 
  df$gmt <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + df$Time ## change time format
  
  zd <- df %.>% 
    cut(.$gmt, "2 hours") %.>% 
    unlist(tapply(df$Depth, ., zoc))  
  
  names(zd) <- NULL ## Names of ZD is the timestamp...
  
  nzd <- zd ## So this is just then the zoc'ed depth
  nzd[zd < sn] <- 0
  df$cor.depth <- nzd
  
  df$num <- cumsum(abs(c(1, diff(nzd > 4)))) 
  
  ## Adding "dur", "max.d", "bottom_depth" and "bottom_time" columns to dataframe 
  df_init_tmp1 <- df %>% #df_init_tmp1 used for joining to get df_init_tmp2
    group_by(num) %>%
    dplyr::mutate("all.dur" = max(Time) - min(Time), "max.d" = max(cor.depth), "bottom_depth" = max.d *0.8)  
  
  ## dataframe for all dives in total before shortlisting to analyze
  df_init_tmp2 <- df_init_tmp1 %>%
    filter(cor.depth > bottom_depth) %>%
    mutate("bottom_time" = max(Time)-min(Time)) %>%
    select("bottom_time") %>%
    unique() %>%
    right_join(df_init_tmp1,by = "num") %>% 
    movetolast(c("bottom_time")) %>% 
    mutate("%bt/dt" = bottom_time/all.dur)
  
  setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc")
  saveRDS(df_init_tmp2,file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")),compress = TRUE)
  
  ## Dives shortlisted for creating dbs and ndbs - i.e. dives to perform BS algorithm on 
  ## For faster analysis of foraging > 20m select those dives only for the loop
  all_dives <- df_init_tmp2 %>% # used in getting dbs and ndbs. 
    filter(max.d > 4, all.dur > 60) # shallower dives are assumed to be travelling or caused by waves Kirkman (2019)
  rm(df_init_tmp2,df_init_tmp1)
  
  
  # Run Gompertz modelled dives ---------------------------------------------
  #############################################################################################################################
  #############################################################################################################################
  ####################### Description: Model used to create broken stick dataframe for dives that follow ######################
  ####################### the Gompertz model and for dives that don't. These dives are extracted into    ######################
  ####################### dataframes using the optimal BSpoints and the 3BSpoint rule, respectively.     ######################
  ####################### The model creates two broken stick dataframes. The dbs dataframe is the        ######################
  ####################### segmented (interpolated) dives using the optimal number of BSpoints, while the ######################
  ####################### ndbs uses the 3 BSpoints rule.                                                 ######################
  #############################################################################################################################
  #############################################################################################################################
  
  numlist<-unique(all_dives$num)
  
  ## Broken stick dataframe - optimal npoints
  dbs <- data.frame("num" = rep(0,1), "time_start" = 0, "time_end" = 0, "depth_start" = 0,
                    "depth_end" = 0, "seg" = 0, "npoints" = 0, "dur" = 0, "coef" = 0, "mean_depth" = 0, 
                    "wiggle" = 0, "swim_speed" = 0, "sinuosity" = 0, "mean_err" = 0)
  ## Broken stick dataframe - Set npoints
  ndbs <- data.frame("num" = rep(0,1), "time_start" = 0, "time_end" = 0, "depth_start" = 0,
                     "depth_end" = 0, "seg" = 0, "npoints" = 0, "dur" = 0, "coef" = 0, "mean_depth" = 0, 
                     "wiggle" = 0, "swim_speed" = 0, "sinuosity" = 0, "mean_err" = 0)
  
  
  for (d in 1:length(numlist)) {  
    print(paste(d,"Out of",length(numlist),"dives",sep=" "))
    dive <- all_dives[all_dives$num == numlist[d],]
    ndive = numlist[d] #used later on in the model
    # Set up broken stick model for each dive
    np <- c(3:30) ## number of broken stick iterations to see which optimal number of points summarize your dive
    npe = rep(NA, 28) ## vector where the average distance between original and reconstructed dive profile is stored
    npo = rep(NA, 28) ## vector where the number of broken stick points describing the dive profile is stored
    
    for (k in 1:length(np)) { #selection of the depth and time for the 2 surface points and the maximum depth point - initializing BSM
      ref <- c(dive$cor.depth[1], max(dive$cor.depth), dive$cor.depth[nrow(dive)])
      tim <- c(as.numeric(dive$gmt[1]), as.numeric(dive$gmt[dive$cor.depth == max(dive$cor.depth)][1]), as.numeric(dive$gmt[nrow(dive)]))
      
      for (i in 1:np[k]) { # selection of the number of iteration: from 3 to 30
        interp <- approx(tim, ref, xout = dive$gmt, method = "linear") ## linear interpolation between broken stick points at TDR time interval
        dif_x <- as.numeric(interp$x - dive$gmt) ## time differences between original and reconstructed profiles
        dif_y <- interp$y - dive$cor.depth ## depth differences between original and reconstructed profiles
        dst <- sqrt(dif_x ^ 2 + dif_y ^ 2) ## calculate distances between original and reconstructed profiles in terms of time and vertcal space (depth)
        ii <- which(dst == max(dst))[1] ## index of the 'first'' data point of maximum difference between original and reconstructed profiles ('first' in case there are two of the same)
        tim <- c(as.numeric(tim), as.numeric(dive$gmt[ii])) ## add new broken stick point time
        tim <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "gmt") + tim
        ref <- c(ref, dive$cor.depth[ii]) ## add new broken stick point depth
      }
      npe[k] = mean(dst) ## average distance between original and reconstructed dive profiles
      npo[k] = length(tim) ## number of broken stick points describing the dive profile - 'np' selection of the number of iteration: from 3 to 30
    }
    
    ## 2. Defining the optimal number of broken stick points
    f <- data.frame(npe = npe, npo = npo)
    
    ## Use of a gompertz model to find the curve which best fit our data
    Asym <- 0
    b2 <- -5
    b3 <- 0.9
    fm1 <- -999
    try(fm1 <- nls(npe ~ SSgompertz(npo, Asym, b2, b3), data = f, control = nls.control(maxiter = 500)), TRUE) ## gompertz model to fit an asymptote
    ## curve to the mean distance between original and reconstructed dive profiles plot
    
    if (class(fm1) == "nls") { # if the model converged, we can go to the next steps
      tt <- predict(fm1, f$npe)
      ## linear approximation between the first and last point of the fitted curve
      t <- data.frame(npe = c(f$npe[1], f$npe[28]), npo = c(f$npo[1], f$npo[28]))
      interp <- approx(c(f$npo[1], f$npo[28]), c(tt[1], tt[28]), xout = f$npo, method = "linear") # linear interpolation 
      interp$x <- interp$x[!is.na(interp$x)] # This just makes sure nothing else is selected but x and y
      interp$y <- interp$y[!is.na(interp$y)]
      ## Looking for the inflexion point which is the furthest point between the fitted curve and the approximation 
      dif_x <- interp$x - na.omit(f$npo)
      dif_y <- interp$y - tt[1:28]
      dst <- sqrt(dif_x ^ 2 + dif_y ^ 2)
      dm <- f$npo[which(dst == max(dst))]
      
      ## 3. optimal broken stick method for each dive 
      
      ## The two lines below select the optimal number of broken stick points (in their order of appearance in the BS iteration)
      ## example: surface start point, max. depth point, surface end point + x other points
      tim = tim[1:dm]
      ref = ref[1:dm]
      
      tim2 <- sort(tim) ## sorts time for use in next loop
      dep_tim <- as.data.frame(cbind(ref, tim)) ## uses tim (unsorted)
      dep_tim <- dep_tim[order(tim),] ## then order tim so that tim and ref still correspond within the dataframe
      
      ## Set up dbs2 dataframe for next dive to append to previous dives
      dbs2 <- data.frame("num" = rep(0, (nrow(dep_tim) - 1)), "time_start" = 0, "time_end" = 0, "depth_start" = 0,
                         "depth_end" = 0, "seg" = 0, "npoints" = 0, "dur" = 0, "coef" = 0, "mean_depth" = 0, 
                         "wiggle" = 0, "swim_speed" = 0, "sinuosity" = 0, "mean_err" = 0)
      
      ## Loop to calculate the different metrics for each broken stick segments
      for (n in 1:(nrow(dep_tim) - 1)) {
        x1 = dep_tim$tim[n] ## start of BS segment
        x2 = dep_tim$tim[n + 1] ## end of BS segment
        dbs2$num[n] = ndive
        dbs2$time_start[n] = x1
        dbs2$time_end[n] = x2
        dbs2$depth_start[n] = dep_tim$ref[n] ## depth of start of BS segment
        dbs2$depth_end[n] = dep_tim$ref[n + 1] ## depth of end of BS segment
        dbs2$seg[n] = n ## segment number
        dbs2$npoints[n] = nrow(dep_tim) ## optimal BS points summarising the original dive profile
        dbs2$dur[n] = difftime(tim2[n + 1], tim2[n], tz, units = c("secs")) ## duration of the segment in sec.
        dbs2$coef[n] = (dep_tim$ref[n + 1] - dep_tim$ref[n]) / (x2 - x1) ## slope coefficient of the segment
        dbs2$mean_depth[n] = mean(dive$cor.depth[which(as.numeric(dive$gmt) == x1):which(as.numeric(dive$gmt) == x2)]) ## mean depth of the segment calculated from original profile depths
        ## Calculation of vertical sinuosity
        deuc = abs(dep_tim$ref[n + 1] - dep_tim$ref[n]) ## Vertical distance swum between 2 BS points
        dobs = sum(abs(diff(dive$cor.depth[which(dive$gmt == x1):which(dive$gmt == x2)]))) ## sum of all the vertical distances swum within a segment from the original profile
        ## profile between the two corresponding BS depth points
        dbs2$wiggle[n] = dobs #vertical distance swum by seal within segment
        dbs2$swim_speed[n] = dobs/dbs2$dur[n] # speed of swimming between broken stick points
        dbs2$sinuosity[n] = deuc / dobs ## vertical sinuosity index
        dbs2$mean_err[n] = f$npe[which(dst == max(dst))] ## mean distance between original and reconstructed dive profiles for the optimal
        ## number of BS points summarising the dive.
      }
      #-----------------------------------------------------------------------------------------------------------------------------------
      
      ## Append dataframe
      dbs <- rbind(dbs, dbs2)
    } else { 
      
      ## 3. optimal broken stick method for each dive 
      
      ## The two lines below select the number of broken stick points according to Fedak et al 2001 (Same method used to summarize SRDL data)
      ## i.e. surface start point, max.d point, surface end point + 3 other points
      tm <- 6 # using 6 points to simplify dives and calculate sinuosity 
      
      tim = tim[1:tm]
      ref = ref[1:tm]
      
      tim2 <- sort(tim) ## sorts time for use in next loop
      dep_tim <- as.data.frame(cbind(ref, tim)) ## uses tim (unsorted)
      dep_tim <- dep_tim[order(tim),] ## then order tim so that tim and ref still correspond within the dataframe
      
      ## Set up dbs2 dataframe for next dive to append to previous dives
      ndbs2 <- data.frame("num" = rep(0, (nrow(dep_tim) - 1)), "time_start" = 0, "time_end" = 0, "depth_start" = 0,
                          "depth_end" = 0, "seg" = 0, "npoints" = 0, "dur" = 0, "coef" = 0, "mean_depth" = 0, 
                          "wiggle" = 0, "swim_speed" = 0, "sinuosity" = 0, "mean_err" = 0)
      
      ## Loop to calculate the different metrics for each broken stick segments
      for (n in 1:(nrow(dep_tim) - 1)) {
        x1 = dep_tim$tim[n] ## start of BS segment
        x2 = dep_tim$tim[n + 1] ## end of BS segment
        ndbs2$num[n] = ndive
        ndbs2$time_start[n] = x1
        ndbs2$time_end[n] = x2
        ndbs2$depth_start[n] = dep_tim$ref[n] ## depth of start of BS segment
        ndbs2$depth_end[n] = dep_tim$ref[n + 1] ## depth of end of BS segment
        ndbs2$seg[n] = n ## segment number
        ndbs2$npoints[n] = nrow(dep_tim) ## optimal BS points summarising the original dive profile
        ndbs2$dur[n] = difftime(tim2[n + 1], tim2[n], tz, units = c("secs")) ## duration of the segment in sec.
        ndbs2$coef[n] = (dep_tim$ref[n + 1] - dep_tim$ref[n]) / (x2 - x1) ## slope coefficient of the segment
        ndbs2$mean_depth[n] = mean(dive$cor.depth[which(as.numeric(dive$gmt) == x1):which(as.numeric(dive$gmt) == x2)]) ## mean depth of the segment calculated from original profile depths
        ## Calculation of vertical sinuosity
        deuc = abs(dep_tim$ref[n + 1] - dep_tim$ref[n]) ## Vertical distance swum between 2 BS points
        dobs = sum(abs(diff(dive$cor.depth[which(dive$gmt == x1):which(dive$gmt == x2)]))) ## sum of all the vertical distances swum within a segment from the original profile
        ## profile between the two corresponding BS depth points
        ndbs2$wiggle[n] = dobs #vertical distance swum by seal within segment
        ndbs2$swim_speed[n] = dobs/ndbs2$dur[n] # speed of swimming between broken stick points
        ndbs2$sinuosity[n] = deuc / dobs ## vertical sinuosity index
        ndbs2$mean_err[n] = f$npe[which(dst == max(dst))] ## mean distance between original and reconstructed dive profiles for the optimal
        ## number of BS points summarising the dive.
      }
      
      ## Append dataframe
      ndbs <- rbind(ndbs, ndbs2) 
    } 
    if (d == NROW(numlist)) {
      dbs <- dbs[-1,]
      ndbs <- ndbs[-1,]
    }
    
  }
  ## end of if loop for max.d>4 & dur>60
  setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc")
  saveRDS(dbs,file.path(save_dbs,paste(sealID,"_dbs.rds",sep = "")),compress = TRUE)
  saveRDS(ndbs,file.path(save_ndbs,paste(sealID,"_ndbs.rds",sep = "")),compress = TRUE)
  
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
  ndbs$bs_npoints <- paste(tm,"set",sep = "_")
  # ndbs$bs_npoints <- paste(6,"set",sep = "_") #for use if loop stops (i.e. "tm" has not been defined). Here we define it as 6
  ndbs$bs_npoints <- as.factor(ndbs$bs_npoints)
  }
})




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
# Saving important files  ------------------------------------------------------

#########################################################################################################
######################### This is done for later analysis of segments within dives  #####################
#########################################################################################################
setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc")
saveRDS(bsm_seg_df,file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")),compress = TRUE)

