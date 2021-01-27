##############################################################################################################################
##################### Sean_fseal_VARS.R: Is a file containg code to process fur seal diving data. ############################
#####################             A vertical area restricted search method is used.               ############################
##############################################################################################################################


# Load packages -----------------------------------------------------------

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
library(ggstatsplot)
library(ncdf4)
library(raster)



# Set Working directory and import myFunctions ------------------------------------

rm(list = ls())
setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/Computing docs")
source("myFunctions.R")
setwd("~/2020/MSc/Computing/Fur seals/Dive data for Sean Evans")
## To setwd() press cntr + Shift + H
getwd()
read_path = "Extracted raw dive data - from instrument helper/Fur seal data"
save_dt = "Extracted raw dive data - from instrument helper/Fur seal data/dt_before_TDR"
data_path = "Extracted raw dive data - from instrument helper/Fur seal data/dt_before_TDR"
# save_zoc_df = "Extracted raw dive data - from instrument helper/Fur seal data/df" 
save_bsm_seg_df = "bsm_seg_df"
save_df_init_tmp2 = "df_init_tmp2"
save_divestats = "divestats"
save_loc1 = "loc1"
# Data frame structuring -----------------------------------------------------
#fs_list <- list.files(data_path)
#seal_no <- 1
#load(paste(data_path,fs_list[seal_no],sep = "/"))
## All dataframes have Time column as ISOdate not gmt => Convert Time to gmt and add column 'gmt'

#df$gmt <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + df$Time #turn that ISOdate number into something you can understand




# Data Structuring -------------------------------------------------

fs_list <- list.files(data_path, pattern = ".Rda") # List of ".Rda" files
system.time({
  # Setting up Zero Offset Correction
  zoc <- function(x, zval = 10, blim = -5) {
    h <- hist(x[x < zval & x > blim], breaks = seq(blim, zval, 1),plot = FALSE)
    x - h$breaks[which.max(h$counts)]
  }
  ## sets surface intervals within the "surface noise" value to 0
  sn <- 1
  
  ########## Loading seal file ##########
  sealID <- 1
  load(paste(data_path,fs_list[sealID],sep = "/"))
  
  df <- dt
  df$gmt <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + df$Time
  
  zd <- df %.>% 
    cut(.$gmt, "2 hours") %.>% 
    unlist(tapply(df$Depth, ., zoc))  
  
  
  names(zd) <- NULL ## Names of ZD is the timestamp...
  
  nzd <- zd ## So this is just then the zoc'ed depth
  nzd[zd < sn] <- 0
  df$cor.depth <- nzd
  
  
  df$num <- cumsum(abs(c(1, diff(nzd > 4)))) 
  
  #df %.>% filter(.,num == 2) %.>%  mutate(.,"dur" = rep(max(select(.,Time)) - min(select(.,Time)))) # for dive no 2 add col dur
  
  
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
    mutate("%bt/dt" = bottom_time/dur)


  #Select active foraging dives where dur > 60sec and max.d > 15m
  # df_init <- df_init_tmp2 %>% 
    # filter(max.d > 15, dur > 60)
  
  ## Dives shortlisted for creating dbs and ndbs - i.e. dives to perform BS algorithm on 
  all_dives <- df_init_tmp2 %>% # used in getting dbs and ndbs. 
    filter(max.d > 4, dur > 15) # shallower dives are assumed to be travelling or caused by waves Kirkman (2019)

  
}) 

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


# foreach () # install package doparallel
system.time({ 
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
        # dbs2$all.dur[n] = difftime(dive$gmt[nrow(dive)], dive$gmt[1], tz, units = c("secs")) ## dive duration
        dbs2$time_start[n] = x1
        dbs2$time_end[n] = x2
        dbs2$depth_start[n] = dep_tim$ref[n] ## depth of start of BS segment
        dbs2$depth_end[n] = dep_tim$ref[n + 1] ## depth of end of BS segment
        dbs2$seg[n] = n ## segment number
        dbs2$npoints[n] = nrow(dep_tim) ## optimal BS points summarising the original dive profile
        dbs2$dur[n] = difftime(tim2[n + 1], tim2[n], tz, units = c("secs")) ## duration of the segment in sec.
        # dbs2$dur.per[n] = (dbs2$dur[n] / dbs2$all.dur[n]) * 100 ## % of segment duration according to total dive duration
        dbs2$coef[n] = (dep_tim$ref[n + 1] - dep_tim$ref[n]) / (x2 - x1) ## slope coefficient of the segment
        dbs2$mean_depth[n] = mean(dive$cor.depth[which(as.numeric(dive$gmt) == x1):which(as.numeric(dive$gmt) == x2)]) ## mean depth of the segment 
        ## calculated from original profile depths
        # dbs2$max.depth[n] = max(dive$cor.depth) ## dive max. depth
        
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
        # ndbs2$all.dur[n] = difftime(dive$gmt[nrow(dive)], dive$gmt[1], tz, units = c("secs")) ## dive duration
        ndbs2$time_start[n] = x1
        ndbs2$time_end[n] = x2
        ndbs2$depth_start[n] = dep_tim$ref[n] ## depth of start of BS segment
        ndbs2$depth_end[n] = dep_tim$ref[n + 1] ## depth of end of BS segment
        ndbs2$seg[n] = n ## segment number
        ndbs2$npoints[n] = nrow(dep_tim) ## optimal BS points summarising the original dive profile
        ndbs2$dur[n] = difftime(tim2[n + 1], tim2[n], tz, units = c("secs")) ## duration of the segment in sec.
        # ndbs2$dur.per[n] = (ndbs2$dur[n] / ndbs2$all.dur[n]) * 100 ## % of segment duration according to total dive duration
        ndbs2$coef[n] = (dep_tim$ref[n + 1] - dep_tim$ref[n]) / (x2 - x1) ## slope coefficient of the segment
        ndbs2$mean_depth[n] = mean(dive$cor.depth[which(as.numeric(dive$gmt) == x1):which(as.numeric(dive$gmt) == x2)]) ## mean depth of the segment 
        ## calculated from original profile depths
        # ndbs2$max.depth[n] = max(dive$cor.depth) ## dive max. depth
        
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
      #-----------------------------------------------------------------------------------------------------------------------------------
      
      ## Append dataframe
      ndbs <- rbind(ndbs, ndbs2) 
      
      }
    
    if (d == NROW(numlist)) {
      dbs <- dbs[-1,]
      ndbs <- ndbs[-1,]
    }
    # if (d == NROW(num.list)) {
    #   dbs <- dbs[-1,]
    # }
  }
  ## end of if loop for dur>15 & max.d>4
  
  #save(dbs, file = "17_VDB_2011W_0990468_A160_BSP.Rda")
  
  ### Foraging?: ################ NBBBBB!!!!!!!!! Check velocity before using threshold of 0.4m/s
  #-----------
  ## Attribution of behaviour according to vertical sinuosity -- Remind that the sinuosity threshold used here was determined according
  ## to the histogram/density plot of vertical sinuosity for every BS segments of every dive
  ## so, before setting your threshold at 0.9, check if it suits your dataset (i.e after running the BS on all your dive)
  
  dbs$velocity <- abs(((dbs$depth_end)-(dbs$depth_start))/dbs$dur) # Heerah et al (2015) - velocity of low res profile <0.4 = "hunting" 
  dbs$foraging <- "transit" ## "hunting" mode
  dbs$foraging[dbs$sinuosity < 0.9 & dbs$velocity < 0.4 & dbs$dur > 4] <- "hunting" ## According to Heerah, Hindell, Guinet, and Charrassin (2015) & Heerah (2014)
  dbs$foraging <- as.factor(dbs$foraging)
  dbs$bs_npoints <- "optimal"
  dbs$bs_npoints <- as.factor(dbs$bs_npoints)
  
  ndbs$velocity <- abs(((ndbs$depth_end)-(ndbs$depth_start))/ndbs$dur)
  ndbs$foraging <- "transit" ## "hunting" mode
  ndbs$foraging[ndbs$sinuosity < 0.9 & ndbs$velocity < 0.4 & ndbs$dur > 4] <- "hunting" ## According to Heerah, Hindell, Guinet, and Charrassin (2015) & Heerah (2014)
  ndbs$foraging <- as.factor(ndbs$foraging)
  ndbs$bs_npoints <- paste(tm,"set",sep = "_")
  ndbs$bs_npoints <- as.factor(ndbs$bs_npoints)
})




# Add columns to dbs ------------------------------------------------------

######################################################################################################
######################### This is done for later analysis of segmented dives #########################
######################################################################################################


################# add column for "all.dur" - duration of the dives. ("dur" - is the duration of each segment within a dive)


dbs <- df_init_tmp2 %>% 
  select(max.d,bottom_depth,bottom_time,'%bt/dt') %>% 
  filter(row_number()==1) %>% 
  right_join(dbs %>% group_by(num) %>% dplyr::mutate("all.dur" = max(time_end) - min(time_start)),by = "num")

ndbs <- df_init_tmp2 %>% 
  select(max.d,bottom_depth,bottom_time,'%bt/dt') %>% 
  filter(row_number()==1) %>% 
  right_join(ndbs %>% group_by(num) %>% dplyr::mutate("all.dur" = max(time_end) - min(time_start)),by = "num")

dbs$travel <- dbs$all.dur - dbs$bottom_time ## Travel time within a dive (transit time based on bottom phase method)
ndbs$travel <- ndbs$all.dur - ndbs$bottom_time

# dbs_tmp <- dbs_tmp %>%
#   filter(mean_depth > bottom_depth) %>%
#   mutate("wiggle_rate" = sum(wiggle)/(bottom_time/60)) %>% 
#   select("wiggle_rate") %>% 
#   unique() %>% 
#   right_join(dbs_tmp,by = "num")


################# add column for wiggle rate - another proxy for foraging effort according to Krause (2016)
# >2 wiggles/min is considered as indicating foraging behaviour
##########
# dbs_tmp <- dbs_tmp %>%
#   filter(mean_depth > bottom_depth) %>%
#   mutate("wiggle_rate" = sum(wiggle)/(sum(dur))) %>% 
#   select("wiggle_rate") %>% 
#   unique() %>% 
#   right_join(dbs_tmp,by = "num")
# 
# ndbs_tmp <- ndbs_tmp %>%
#   filter(mean_depth > bottom_depth) %>%
#   mutate("wiggle_rate" = sum(wiggle)/(sum(dur))) %>% 
#   select("wiggle_rate") %>% 
#   unique() %>% 
#   right_join(ndbs_tmp,by = "num")
##########
# dbs_tmp <- dbs_tmp %>%
#   filter(mean_depth > bottom_depth) %>%
#   mutate("wiggle_rate" = sum(wiggle)/(bottom_time/60)) %>% 
#   select("wiggle_rate") %>% 
#   unique() %>% 
#   right_join(dbs_tmp,by = "num")

# ndbs_tmp <- ndbs_tmp %>%
#   filter(mean_depth > bottom_depth) %>%
#   mutate("wiggle_rate" = sum(wiggle)/(bottom_time/60)) %>% 
#   select("wiggle_rate") %>% 
#   unique() %>% 
#   right_join(ndbs_tmp,by = "num")

bsm_seg_df <-bind_rows(dbs,ndbs) %>% arrange(num)
# densityplot(bsm_seg_df$wiggle_rate)





# Examine dive parameters -------------------------------------------------


################################################################################################################################################

dbs$travel <- dout$duration - dout$bottom.duration ## one last variable - the travel time within a dive 
plot(dbs$max.ddepth,dout$duration)
abline(lm(dout$duration~dout$depth),col="red")

#generalise the variables   - you can then easily plot each variable
x <- dout$gmt
y <- dout$depth # change to the variable of interest
hour <- format(dout$gmt, "%H", tz="GMT")

# ##this plots the variable as a function of time of day
# group <- floor(y/.10)*10 #consider changing the scaling factor
# mat <- tapply(x, list(hour, group*-1), length)
# mat[is.na(mat)] <- 0
# title.1 <- paste(seal, "depth,  n= ", max(dout$divenum))
# 
# par(mfrow=c(2,2))
# hist(y[y>6], breaks=50, main=title.1)  ##plot a frequency distribution for a particular variable
# plot(x,y*-1,  type="l") # Plots two variable against each other
# image.plot(mat, xlab="time of Day", ylab="depth") # does the time of day image plot
# 
# #Basic stats - need to change the variables- run this for
# ## depth, duration,  bottom time, and the number of wiggles
# mean(dout$depth)
# sd(dout$depth)
# range(dout$depth)
# 
# #how are the variables related?
# 
# splom(dout[,c(4,5,6,7,8,10)], panel = function(x, y, ...) {
#          panel.xyplot(x, y, ...)
# panel.lmline(x,y,...)
#  }
# )





# Combine dive data with location data ------------------------------------

##############################################################################################################################
####################### Description: Combining potential foraging dives with locations. Collecting the nearest ###############
#######################         dives and averaging the dives that are > 20m deep and > 60sec long.            ###############
##############################################################################################################################

##########################  Restructuring and filtering dives first ########################## 

## Dive summary dataframe before adding information to the locations dataframe

# divestats <- df_init_tmp2 %>% filter(row_number()==1) %>% select(-Time,-Depth,-Temperature,-External.Temp,-Light.Level) %>% mutate("start" = gmt) %>% select(-gmt)

#Need to mutate on a column - "optimalBSM" vs "LowResBSM6"
################# add column for transit and hunting time per dive according to BSM segments - Might want to revise this to use df_init_tmp2

divestats <- bsm_seg_df %>% 
  group_by(num) %>%
  filter(foraging == "transit") %>%
  summarize("transit_time" = sum(dur)) %>% 
  full_join(bsm_seg_df %>% 
              group_by(num) %>%
              filter(foraging == "hunting") %>% #calculate the sum time of all hunting segments
              summarize("hunting_time" = sum(dur)), by="num") %>% 
  arrange(num) %>% 
  full_join(tibble(all_dives %>% 
                     select(num) %>% 
                     unique(),
                   "pdsi" = (((all_dives %>% slice(1))$Time)[-1] - ((all_dives %>% slice(n()))$Time)))) %>% 
  replace_na(list(hunting_time = 0, transit_time = 0, pdsi = NaN)) %>% 
  arrange(num) %>% 
  right_join(df_init_tmp2 %>% 
               filter(row_number()==1) %>% 
               select(-Time,-Depth,-Temperature,-External.Temp,-Light.Level) %>% 
               mutate("start" = gmt) %>% 
               select(-gmt,-cor.depth)
             ,by = "num") %>% 
  mutate(hunting_time = replace_na(hunting_time, 0), transit_time = ifelse(is.na(transit_time),dur,transit_time))
divestats$end <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + (df_init_tmp2 %>% slice(n()))$Time


# Check if any weird data - divestats %>% filter(transit_time==0,hunting_time>0)

# Dive rate according to Kirkman (2019) per seal - dive rate (m/h) were modelled against predictor variables of year & body mass (kg)
# Not accurate for our data (is an under-estimate) because there is missing data between dives 
dive_rate <- (sum(divestats %>% filter(max.d > 4) %>% select(max.d))*2)/(as.numeric(last(df_init_tmp2$gmt)-first(df_init_tmp2$gmt))*24)

#SES - (15m,60sec); Weddels - (4m,60sec)()

# Frequency dist of dive depths bi-modal? Two groups of dive depth separated at how many meters?

# Can we exclude dives < than a certain depth, from further analysis (% of dives longer than 60 sec?)
# as they may indicate non-foraging activities?

# According to Arthur (2016) - Only excursions to >6 m were analysed (Staniland & Robinson 2008) as we 
# found the broken stick algorithm typically did not fit dives shallower than this (see results).
# We found the BSt algorithm typically did not fit short and shallow dives, as the model could not detect 
# an inflection point (see Results). Consequently, only dives >40 s of duration were included in the analysis.
##########################  Prepping dives further for location dive summaries ########################## 


## filtered dive summaries that may have foraging
filtered_divestats <-  divestats %>% filter(max.d > 4,dur>60) #Probably foraging dives according to Heerah (2014), Heerah (2015), 
#SES - (15m,60sec); Weddels - (4m,60sec)()

# Frequency dist of dive depths bi-modal? Two groups of dive depth separated at how many meters?

# Can we exclude dives < than a certain depth, from further analysis (% of dives longer than 60 sec?)
# as they may indicate non-foraging activities?

# According to Arthur (2016) - Only excursions to >6 m were analysed (Staniland & Robinson 2008) as we 
# found the broken stick algorithm typically did not fit dives shallower than this (see results).
# We found the BSt algorithm typically did not fit short and shallow dives, as the model could not detect 
# an inflection point (see Results). Consequently, only dives >40 s of duration were included in the analysis.

# Is the seal hunting in this dive or not?
filtered_divestats$ht_rat <- filtered_divestats$hunting_time/filtered_divestats$transit_time  #hunting to transit ratio
#visually decide on threshold h below
densityplot(filtered_divestats$ht_rat[filtered_divestats$ht_rat<4 & filtered_divestats$ht_rat>0])


h <- 0.4 #threshold for "ht_rat' above which hunting time is large enough relative to transit time that the dive can be seen as a foraging dive
# p = % of dives where hunting time is considerably small
p <- length(filtered_divestats$ht_rat[filtered_divestats$ht_rat>0 & filtered_divestats$ht_rat<h])/length(filtered_divestats$ht_rat)
# if p < 0.40 , hunting is probably where "hunting" > h

# filtered_divestats$dur == (filtered_divestats$hunting_time + filtered_divestats$transit_time) #Therefore, dur != transit+foraging 
# This is because we ignore foraging segments <= 4sec long 

#### Adding "hunt_dive" column to divestats and filtered_divestats
divestats <- filtered_divestats # Needed filtered_divestats to get column data for both dataframes
  filter(ht_rat > h) %>% 
  mutate("hunt_dive" = "hunting") %>% 
  select(num,hunt_dive) %>% 
  right_join(divestats,by="num") %>% 
  replace_na(list(hunt_dive = "transit"))

filtered_divestats <- filtered_divestats %>% 
  filter(ht_rat > h) %>% 
  mutate("hunt_dive" = "hunting") %>% 
  select(num,hunt_dive) %>% 
  right_join(filtered_divestats,by="num") %>% 
  replace_na(list(hunt_dive = "transit"))

# Can get mean hunting depth for the dive .... (Using mean(dbs$mean_depth[dbs$foraging == "hunting])). Can use this for location summaries

##########################  Group dives closest to locations and perform summary stats ##########################

## get mean values for each variables 4 hours either side of each location
## the code below show how to it for bottom time. You need to repeat this for the other variables

## Import tracks analysed by Mia
tracks <- read.csv("Marion_FStracks_SSMresults_2009W_2015S.csv",sep = ";")
# str(tracks)

loc1 <-droplevels(subset(tracks,tracks$id == sealID)) %>% filter(gmt > first(df_init_tmp2$gmt) & gmt < last(df_init_tmp2$gmt))
loc1$gmt <- strptime(loc1$gmt,format = "%Y-%m-%d %H:%M", tz = "GMT")

diff(loc1$gmt)
which(diff(loc1$gmt)>3) # where there is missing data

# summary_dive_loc <- data.frame("num" = rep(0, 1), "all.dur" = 0, "start" = 0, "end" = 0, "depth_start" = 0, "depth_end" = 0, "seg" = 0, "npoints" = 0,
#                                     "dur" = 0, "dur.per" = 0, "coef" = 0, "mean_depth" = 0, "max.depth" = 0, "wiggle" = 0, "sinuosity" = 0, "mean_err" = 0, "foraging" = 0)


#set up empty variables
loc1$dur <- NA  
loc1$mean_max.d <- NA
loc1$bottom_depth <- NA
loc1$bottom_time <- NA
loc1$'%bt/dt' <- NA
loc1$max_max.d <- NA
loc1$hunting_time <- NA
loc1$transit_time <- NA
#2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
# system.time({
# for(i in 1:nrow(loc1)) {
#   print(paste(i,nrow(loc1)),sep=" ")
#   if (length(which(between(shortlist$start,loc1$gmt[i] - (3600*1.25),loc1$gmt[i] + (3600*1.25),incbounds = FALSE) == TRUE)) > 0){
#   loc_tmp <- shortlist %>% 
#     filter(between(start,loc1$gmt[i] - (3600*1.25),loc1$gmt[i] + (3600*1.25),incbounds = FALSE)) %>%
#     mutate("X" = i) #dives that fall into 1.25 hours either side of the location
# 
#   loc1$dur[i] <- loc_tmp$dur %>% mean()
#   loc1$max.d[i] <- loc_tmp$max.d %>% mean()
#   loc1$bottom_depth[i] <- loc_tmp$bottom_depth %>% mean()
#   loc1$bottom_time[i] <- loc_tmp$bottom_time %>% mean()
#   loc1$'%bt/dt'[i] <- loc_tmp$'%bt/dt' %>% mean()
#   } else{
#     loc1$dur[i] <- NaN
#     loc1$max.d[i] <- NaN
#     loc1$bottom_depth[i] <- NaN
#     loc1$bottom_time[i] <- NaN
#     loc1$'%bt/dt'[i] <- NaN
#   }
# }
# })

##run a loop over the location data, grabbing the corresponding dive data
##2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
system.time({ 
for(i in 1: (nrow(loc1))) {
  print(paste(i,nrow(loc1)-1),sep=" ")
  tmin <- loc1$gmt[i] - (3600*1.25)
  tmax <- loc1$gmt[i] + (3600*1.25)
  mvar <- filtered_divestats[filtered_divestats$start >= tmin & filtered_divestats$start <= tmax,]
  loc1$dur[i] <- mvar$dur %>% mean(na.rm=T)
  loc1$mean_max.d[i] <- mvar$max.d %>% mean(na.rm=T)
  loc1$bottom_depth[i] <- mvar$bottom_depth %>% mean(na.rm=T)
  loc1$bottom_time[i] <- mvar$bottom_time %>% mean(na.rm=T)
  loc1$'%bt/dt'[i] <- mvar$'%bt/dt' %>% mean(na.rm=T)
  loc1$max_max.d[i] <- mvar$max.d %>% max() #replace_na(NaN)
  loc1$hunting_time[i] <- mvar$hunting_time %>% sum(na.rm=T)
  loc1$transit_time[i] <- mvar$transit_time %>% sum(na.rm=T)
  loc1$hunting[i] <- mvar$hunting %>% mean(na.rm=T)
}
})
loc1$max_max.d <- loc1$max_max.d %>% replace(loc1$max_max.d == -Inf,NaN) #replaces infinities created by loop with NaN values in max_max.d
# Testing for errors in data and indicatoion of correct filtering ("filtered_divestats"). 
# Where has hunting time been calculated (for dives >4m & >15sec), but dives close to location are >4m and 60sec
which(is.na(loc1$dur) & loc1$hunting_time!=0)




# Saving dataframes -------------------------------------------------------
setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc")
write.csv(df_init_tmp2,file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.csv",sep = "")))
write.csv(bsm_seg_df,file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.csv",sep = "")))
write.csv(divestats,file.path(save_divestats,paste(sealID,"_divestats.csv",sep = "")))
write.csv(loc1,file.path(save_loc1,paste(sealID,"_loc1.csv")))
