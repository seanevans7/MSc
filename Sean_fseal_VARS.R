##############################################################################################################################
##################### Sean_fseal_VARS.R: Is a file containg code to process fur seal diving data. ############################
#####################             A vertical area restricted search method is used.               ############################
##############################################################################################################################

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
save_zoc_df = "Extracted raw dive data - from instrument helper/Fur seal data/df" 
save_dbs = "Extracted raw dive data - from instrument helper/Fur seal data/df/dbs"
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
  
  
  load(paste(data_path,fs_list[1],sep = "/"))
  
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
    dplyr::mutate("dur" = max(Time) - min(Time), "max.d" = max(cor.depth), "bottom_depth" = max.d *0.8)  
  
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
  
  # df_bsm <- df_init %>% 
    # select(gmt,cor.depth) 
  

  ## Original dive profiles for all possible foraging dives 
  shortlist <- divestats %>% #Probably foraging dives according to Heerah (2014), Heerah (2015), 
    filter(max.d > 4, dur > 60) #SES - (15m,60sec); Weddels - (4m,60sec)()
  
  # Frequency dist of dive depths bi-modal? Two groups of dive depth separated at how many meters?
  
  # Can we exclude dives < than a certain depth, from further analysis (% of dives longer than 60 sec?)
  # as they may indicate non-foraging activities?
  
  # According to Arthur (2016) - Only excursions to >6 m were analysed (Staniland & Robinson 2008) as we 
  # found the broken stick algorithm typically did not fit dives shallower than this (see results).
  # We found the BSt algorithm typically did not fit short and shallow dives, as the model could not detect 
  # an inflection point (see Results). Consequently, only dives >40 s of duration were included in the analysis.
  
}) 

# Basic Stats per seal -------------------------------------------------------------

############################################################################################################################## 
######################## Description: Some basic stats for diving parameters for each seal.  #################################
##############################################################################################################################


## Checking for sampling interval difference between and within dives.
boxplot(data.frame(diff(df_init_tmp2$Time)) %>% filter(diff.df_init_tmp2.Time. >10,diff.df_init_tmp2.Time.<30))# where is there missing data? and how much is missing?
df_init_tmp2[which(diff(df_init_tmp2$Time)>1 & df_init_tmp2$max.d<4),] # Plenty of dives with interval > 1
n_int_err <- df_init_tmp2[which(diff(df_init_tmp2$Time)>1 & df_init_tmp2$max.d>4),] # In this seal only 2 points in dives with interval > 1


# n_all = All dives 
n_all <- df_init_tmp2 %.>% unique(.$num) %>% length()

# n_dives = number of dives used in analysis for foraging (sinuosity using BSM - optimal vs low res methods)
n_dives <- all_dives %.>% unique(.$num) %>% length()
# n_short = shortlisted dives assumed to include foraging activity
n_short <- shortlist %.>% unique(.$num) %>% length()
# n_trans = not actually dives - dives that are part of transit mode
n_trans <- df_init_tmp2 %.>% unique(.$num) %>% length()


# Dive rate according to Kirkman (2019) per seal - dive rate (m/h) were modelled against predictor variables of year & body mass (kg)

dive_rate <- (sum(divestats %>% filter(max.d > 4) %>% select(max.d))*2)/(as.numeric(last(df_init_tmp2$gmt)-first(df_init_tmp2$gmt))*24)


# Time of day


# #Percentage of foraging dives to resolved dives
# all_dive_no <- length(unique(all_dives$num))
# dive_no <- df_init %.>% unique(.$num) %>% length()
# perc <- dive_no/all_dive_no *100

# # mean max depth
# mean_max_d <- df_init %.>% mean(.$max.d)

# # mean dive duration
# mean_dur <- df_init %.>% mean(.$dur)
# mean_no_obs_per_dive <- df_init %>% count() %>% ungroup() %>% select(n) %.>% mean(.$n)

# if mean_no_obs_per_dive != mean_dur or "is not a factor of" - then sampling interval is not constant

# df_init %>% filter(max.d >4) %>% count(max.d) %>% 
#   ggplot(aes(x = num , y = max.d)) +
#   geom_count()





# Model initialization -------------------------------------------------------------------

############################################################################################################################## 
######################## Description: Model used to access first 100 dives. Find summary of BSpoints. ########################
##############################################################################################################################


num.list<-unique(df_bsm$num)
### Finding the optimal number of Broken points for each dive
#length(unique(df[df$dur >30 & df$max.d <15 & df$max.d >4,]$num)) # = 367 dives for sealID == 1

########
## dataframe in which the dives for which the fit doesn't work will be stored.
# ncdv = data.frame("num" = 0, "Time" = 0, "Time" = 0, "Depth" = 0, "Temperature" = 0, "External.Temp" = 0,
#                   "Light.Level" = 0, "sealID" = 0, "species" = 0, "cor.depth" = 0, "beach" = 0, "Temp_int" = 0,
#                   "Season" = 0, "seal_tag" = 0, "TDR/SPLASH" = 0, "gmt" = 0, "cor.depth" = 0, "dur" = 0,
#                   "max.d" = 0, "bottom_depth" = 0, "bottom_time" = 0) 

ncdv <- data.frame(matrix(integer(), ncol = 20, nrow = 0), stringsAsFactors = FALSE) %>% 
  setNames(nm = c(colnames(df_init_tmp2)))


dbs <- data.frame("num" = rep(0, 1), "all.dur" = 0, "start" = 0, "end" = 0, "depth_start" = 0, "depth_end" = 0, "seg" = 0, "npoints" = 0,
                  "dur" = 0, "dur.per" = 0, "coef" = 0, "mean_depth" = 0, "max.depth" = 0, "wiggle" = 0, "sinuosity" = 0, "mean_err" = 0, "foraging" = 0) ## Broken stick dataframe

# df[df$cor.depth == max(df$cor.depth),]
########


# foreach () # install package doparallel
system.time({
num_seq <- seq(1,100) #first 100 dives    #length(num.list))   ## Make it 'for (d in 100){' to just play with dive num 100

for (d in 1:length(num_seq)) {  ## Make it 'for (d in 100){' to just play with dive num 100
  print(paste(d,"Out of",length(num_seq),sep=" "))
  dive <- df_bsm[df_bsm$num == num.list[d],]
  ndive = num.list[d] #used later on in the model
  # Set up broken stick model for each dive
  np <- c(3:30) ## number of broken stick iterations to see which optimal number of points summarize your dive
  npe = rep(NA, 28) ## vector where the average distance between original and reconstructed dive profile is stored
  npo = rep(NA, 28) ## vector where the number of broken stick points describing the dive profile is stored
  
  for (k in 1:length(np)) {
    ## 2 lines below: selection of the depth and time for the 2 surface points and the maximum depth point - initializing BSM
    ref <- c(dive$cor.depth[1], max(dive$cor.depth), dive$cor.depth[nrow(dive)])
    tim <- c(as.numeric(dive$gmt[1]), as.numeric(dive$gmt[dive$cor.depth == max(dive$cor.depth)][1]), as.numeric(dive$gmt[nrow(dive)]))
    
    for (i in 1:np[k]) { # selection of the number of iteration: from 3 to 30
      #plot(as.numeric(dive$gmt), dive$cor.depth, ylim=c(max(dive$cor.depth),0), t="l", ylab="depth (m)", xlab="",xaxt="n")
      ## plot only if you want to see how the broken stick algorithm is working
      #points(tim,ref, pch=19, cex=1, col="red")
      #idem
      interp <- approx(tim, ref, xout = dive$gmt, method = "linear") ## linear interpolation between broken stick points at TDR time interval
      #lines(interp,col="red")
      #idem
      dif_x <- as.numeric(interp$x - dive$gmt) ## time differences between original and reconstructed profiles
      dif_y <- interp$y - dive$cor.depth ## depth differences between original and reconstructed profiles
      dst <- sqrt(dif_x ^ 2 + dif_y ^ 2) ## calculate distances between original and reconstructed profiles in terms of time and vertcal space (depth)
      
      ii <- which(dst == max(dst))[1] ## index of the 'first'' data point of maximum difference between original and reconstructed profiles ('first' in case there are two of the same)
      #points(dive$gmt[ii],dive$cor.depth[ii],col="blue",pch=19,cex=1)
      ## idem
      tim <- c(as.numeric(tim), as.numeric(dive$gmt[ii])) ## add new broken stick point time
      tim <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "gmt") + tim
      ref <- c(ref, dive$cor.depth[ii]) ## add new broken stick point depth
    }
    npe[k] = mean(dst) ## average distance between original and reconstructed dive profiles
    npo[k] = length(tim) ## number of broken stick points describing the dive profile - 'np' selection of the number of iteration: from 3 to 30
  }
  
  ## 2. Defining the optimal number of broken stick points
  
  f <- data.frame(npe = npe, npo = npo)
  #plot(f$npo, f$npe,xlab="nb of points", ylab="mean error") #plot of mean distance between original and reconstructed dive profiles
  ## according to the number of broken stick points describing the dive
  ## activate only if you want to check
  
  ## Use of a gompertz model to find the curve which best fit our data
  Asym <- 0;
  b2 <- -5;
  b3 <- 0.9
  fm1 <- -999
  try(fm1 <- nls(npe ~ SSgompertz(npo, Asym, b2, b3), data = f, control = nls.control(maxiter = 500)), TRUE) ## gompertz model to fit an asymptote
  ## curve to the mean distance between original and reconstructed dive profiles plot
  ## Some of these dive are being left out, although they are looking fine?
  if (class(fm1) == "nls") {
    ## if the model converged, we can go to the next steps
    #summary(fm1)
    tt <- predict(fm1, f$npe)
    
    
    ## plot of the mean distance 
    #png(paste(fig_path, "WED_BS_", ndive, "_", substr(dive$gmt[1], 1, 10), ".png", sep = ""), pointsize = 12 * 1.5, height = 480 * 1.5, widiveh = 480 * 1.5)
    
    #par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))
    #tit = paste("BS_WED08_", ndive, "_", substr(dive$gmt[1], 1, 10))
    #plot(f$npo, f$npe, xlab = "nb of points", ylab = "mean error", main = tit)
    #lines(na.omit(f$npo), tt[1:28], col = "red")
    
    ## Plot the linear approximation between the first and last point of the fitted curve
    t <- data.frame(npe = c(f$npe[1], f$npe[28]), npo = c(f$npo[1], f$npo[28]))
    interp <- approx(c(f$npo[1], f$npo[28]), c(tt[1], tt[28]), xout = f$npo, method = "linear") # linear interpolation 
    interp$x <- interp$x[!is.na(interp$x)] # This just makes sure nothing else is selected but x and y
    interp$y <- interp$y[!is.na(interp$y)]
    #lines(interp$x, interp$y, col = "blue")
    
    ## Looking for the inflexion point which is the furthest point between the fitted curve and the approximation 
    dif_x <- interp$x - na.omit(f$npo)
    dif_y <- interp$y - tt[1:28]
    dst <- sqrt(dif_x ^ 2 + dif_y ^ 2)
    dm <- f$npo[which(dst == max(dst))]
    
    #points(f$npo[which(dst == max(dst))], f$npe[which(dst == max(dst))], pch = 19, col = "red") ## inflexion point
    
    
    ## 3. optimal broken stick method for each dive 
    
    ## The two lines below select the optimal number of broken stick points (in their order of appearance in the BS iteration)
    ## example: surface start point, max. depth point, surface end point + x other points
    tim = tim[1:dm]
    ref = ref[1:dm]
    
    tim2 <- sort(tim) ## sorts time for use in next loop
    dep_tim <- as.data.frame(cbind(ref, tim)) ## uses tim (unsorted)
    dep_tim <- dep_tim[order(tim),] ## then order tim so that tim and ref still correspond within the dataframe
    
    #     }
    #   }
    # }
    dbs2 <- data.frame("num" = rep(0, (nrow(dep_tim) - 1)), "all.dur" = 0, "start" = 0, "end" = 0, "depth_start" = 0,
                       "depth_end" = 0, "seg" = 0, "npoints" = 0, "dur" = 0, "dur.per" = 0, "coef" = 0, "mean_depth" = 0, 
                       "max.depth" = 0, "wiggle" = 0, "sinuosity" = 0, "mean_err" = 0, "foraging" = 0)
    
    ## Loop to calculate the different metrics for each broken stick segments
    for (n in 1:(nrow(dep_tim) - 1)) {
      x1 = dep_tim$tim[n] ## start of BS segment
      x2 = dep_tim$tim[n + 1] ## end of BS segment
      dbs2$num[n] = ndive
      # dbs2$all.dur[n] = difftime(dive$gmt[nrow(dive)], dive$gmt[1], tz, units = c("secs")) ## dive duration
      dbs2$start[n] = x1
      dbs2$end[n] = x2
      dbs2$depth_start[n] = dep_tim$ref[n] ## depth of start of BS segment
      dbs2$depth_end[n] = dep_tim$ref[n + 1] ## depth of end of BS segment
      dbs2$seg[n] = n ## segment number
      dbs2$npoints[n] = nrow(dep_tim) ## optimal BS points summarising the original dive profile
      dbs2$dur[n] = difftime(tim2[n + 1], tim2[n], tz, units = c("secs")) ## duration of the segment in sec.
      dbs2$dur.per[n] = (dbs2$dur[n] / dbs2$all.dur[n]) * 100 ## % of segment duration according to total dive duration
      dbs2$coef[n] = (dep_tim$ref[n + 1] - dep_tim$ref[n]) / (x2 - x1) ## slope coefficient of the segment
      dbs2$mean_depth[n] = mean(dive$cor.depth[which(as.numeric(dive$gmt) == x1):which(as.numeric(dive$gmt) == x2)]) ## mean depth of the segment 
      ## calculated from original profile depths
      # dbs2$max.depth[n] = max(dive$cor.depth) ## dive max. depth
      
      ## Calculation of vertical sinuosity
      deuc = abs(dep_tim$ref[n + 1] - dep_tim$ref[n]) ## Vertical distance swum between 2 BS points
      dobs = sum(abs(diff(dive$cor.depth[which(dive$gmt == x1):which(dive$gmt == x2)]))) ## sum of all the vertical distances swum within a segment from the original profile
      ## profile between the two corresponding BS depth points
      dbs2$wiggle[n] = dobs #vertical distance swum by seal within segment
      dbs2$velocity[n] = dobs/dbs2$dur[n] # speed of swimming between broken stick points
      dbs2$sinuosity[n] = deuc / dobs ## vertical sinuosity index
      dbs2$mean_err[n] = f$npe[which(dst == max(dst))] ## mean distance between original and reconstructed dive profiles for the optimal
      ## number of BS points summarising the dive.
    }

    #-----------------------------------------------------------------------------------------------------------------------------------      
    # Add '& dbs2$mean_depth <= 6'
    ## Dive plot: original dive profile and Broken stick reconstructed profile
    #sg <- unique(dbs2$seg)
    #cl <- c("blue", "red")
    #dbs2$code[dbs2$sinuosity]
    
    #plot(as.numeric(dive$gmt), dive$cor.depth, ylim = c(max(dive$cor.depth), 0), t = "l", ylab = "depth (m)", xlab = "", xaxt = "n")
    #points(tim, ref, pch = 19, cex = 1, col = "black")
    #lines(approx(tim, ref, xout = dive$gmt, method = "linear"), col = "black")
    #for (i in 1:length(sg)) {
    #   lines(c(dbs2$start[dbs2$seg == sg[i]], dbs2$end[dbs2$seg == sg[i]]), c(dbs2$depth_start[dbs2$seg == sg[i]],
    #   dbs2$depth_end[dbs2$seg == sg[i]]), col = cl[dbs2$foraging][dbs2$seg == sg[i]], lwd = 2.5)
    #}
    #axis.POSIXct(1, x = dive$gmt, format = "%H:%M:%S", labels = TRUE, cex.lab = 0.5)
    #dev.off()
    #dbs <- dplyr::bind_rows(dbs, dbs2)
    dbs <- rbind(dbs, dbs2)
  } else { ncdv <- rbind(ncdv, dive) }
  
  if (d == NROW(num_seq)) {
    dbs <- dbs[-1,]
  }
  # if (d == NROW(num.list)) {
  #   dbs <- dbs[-1,]
  # }
}
## end of if loop for dur>15 & max.d>4

  #save(dbs, file = "17_VDB_2011W_0990468_A160_BSP.Rda")
### Foraging?:
#-----------
## Attribution of behaviour according to vertical sinuosity -- Remind that the sinuosity threshold used here was determined according
## to the histogram/density plot of vertical sinuosity for every BS segments of every dive
## so, before setting your threshold at 0.9, check if it suits your dataset (i.e after running the BS on all your dive)

dbs$velocity <- abs(((dbs$depth_end)-(dbs$depth_start))/dbs$dur)
dbs$foraging <- "hunting" ## "hunting" mode
dbs$foraging[dbs$sinuosity >= 0.9 & dbs$sinuosity <= 1 & dbs$velocity < 0.4] <- "transit" #NBBBBBBB!!! Check 0.4
## According to Heerah, Hindell, Guinet, and Charrassin (2015) & Heerah (2014)

})

#summary of first 100 dives' optimal no of BSPs to use in entire seal dataset
dbs %>% group_by(num) %>% select(npoints) %>% unique() %>% summary()

dbs %>% group_by(num) %>% select(npoints) %>% unique() %.>% hist(.$npoints)


max.npoints <- dbs %>% group_by(num) %>% select(npoints) %>% unique() %.>% max(.$npoints)


















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

ndbs <- data.frame("num" = rep(0, 1), "all.dur" = 0, "start" = 0, "end" = 0, "depth_start" = 0, "depth_end" = 0, "seg" = 0, "npoints" = 0,
                          "dur" = 0, "dur.per" = 0, "coef" = 0, "mean_depth" = 0, "max.depth" = 0, "wiggle" = 0, "sinuosity" = 0, "mean_err" = 0, "foraging" = 0)

dbs <- data.frame("num" = rep(0, 1), "all.dur" = 0, "start" = 0, "end" = 0, "depth_start" = 0, "depth_end" = 0, "seg" = 0, "npoints" = 0,
                  "dur" = 0, "dur.per" = 0, "coef" = 0, "mean_depth" = 0, "max.depth" = 0, "wiggle" = 0, "sinuosity" = 0, "mean_err" = 0, "foraging" = 0) ## Broken stick dataframe


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
      dbs2 <- data.frame("num" = rep(0, (nrow(dep_tim) - 1)), "all.dur" = 0, "start" = 0, "end" = 0, "depth_start" = 0,
                         "depth_end" = 0, "seg" = 0, "npoints" = 0, "dur" = 0, "dur.per" = 0, "coef" = 0, "mean_depth" = 0, 
                         "max.depth" = 0, "wiggle" = 0, "sinuosity" = 0, "mean_err" = 0, "foraging" = 0)
      
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
        dbs2$dur.per[n] = (dbs2$dur[n] / dbs2$all.dur[n]) * 100 ## % of segment duration according to total dive duration
        dbs2$coef[n] = (dep_tim$ref[n + 1] - dep_tim$ref[n]) / (x2 - x1) ## slope coefficient of the segment
        dbs2$mean_depth[n] = mean(dive$cor.depth[which(as.numeric(dive$gmt) == x1):which(as.numeric(dive$gmt) == x2)]) ## mean depth of the segment 
        ## calculated from original profile depths
        # dbs2$max.depth[n] = max(dive$cor.depth) ## dive max. depth
        
        ## Calculation of vertical sinuosity
        deuc = abs(dep_tim$ref[n + 1] - dep_tim$ref[n]) ## Vertical distance swum between 2 BS points
        dobs = sum(abs(diff(dive$cor.depth[which(dive$gmt == x1):which(dive$gmt == x2)]))) ## sum of all the vertical distances swum within a segment from the original profile
        ## profile between the two corresponding BS depth points
        dbs2$wiggle[n] = dobs #vertical distance swum by seal within segment
        dbs2$velocity[n] = dobs/dbs2$dur[n] # speed of swimming between broken stick points
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
      dbs2 <- data.frame("num" = rep(0, (nrow(dep_tim) - 1)), "all.dur" = 0, "start" = 0, "end" = 0, "depth_start" = 0,
                         "depth_end" = 0, "seg" = 0, "npoints" = 0, "dur" = 0, "dur.per" = 0, "coef" = 0, "mean_depth" = 0, 
                         "max.depth" = 0, "wiggle" = 0, "sinuosity" = 0, "mean_err" = 0, "foraging" = 0)
      
      ## Loop to calculate the different metrics for each broken stick segments
      for (n in 1:(nrow(dep_tim) - 1)) {
        x1 = dep_tim$tim[n] ## start of BS segment
        x2 = dep_tim$tim[n + 1] ## end of BS segment
        dbs2$num[n] = ndive
        # dbs2$all.dur[n] = difftime(dive$gmt[nrow(dive)], dive$gmt[1], tz, units = c("secs")) ## dive duration
        dbs2$start[n] = x1
        dbs2$end[n] = x2
        dbs2$depth_start[n] = dep_tim$ref[n] ## depth of start of BS segment
        dbs2$depth_end[n] = dep_tim$ref[n + 1] ## depth of end of BS segment
        dbs2$seg[n] = n ## segment number
        dbs2$npoints[n] = nrow(dep_tim) ## optimal BS points summarising the original dive profile
        dbs2$dur[n] = difftime(tim2[n + 1], tim2[n], tz, units = c("secs")) ## duration of the segment in sec.
        dbs2$dur.per[n] = (dbs2$dur[n] / dbs2$all.dur[n]) * 100 ## % of segment duration according to total dive duration
        dbs2$coef[n] = (dep_tim$ref[n + 1] - dep_tim$ref[n]) / (x2 - x1) ## slope coefficient of the segment
        dbs2$mean_depth[n] = mean(dive$cor.depth[which(as.numeric(dive$gmt) == x1):which(as.numeric(dive$gmt) == x2)]) ## mean depth of the segment 
        ## calculated from original profile depths
        # dbs2$max.depth[n] = max(dive$cor.depth) ## dive max. depth
        
        ## Calculation of vertical sinuosity
        deuc = abs(dep_tim$ref[n + 1] - dep_tim$ref[n]) ## Vertical distance swum between 2 BS points
        dobs = sum(abs(diff(dive$cor.depth[which(dive$gmt == x1):which(dive$gmt == x2)]))) ## sum of all the vertical distances swum within a segment from the original profile
        ## profile between the two corresponding BS depth points
        dbs2$wiggle[n] = dobs #vertical distance swum by seal within segment
        dbs2$velocity[n] = dobs/dbs2$dur[n] # speed of swimming between broken stick points
        dbs2$sinuosity[n] = deuc / dobs ## vertical sinuosity index
        dbs2$mean_err[n] = f$npe[which(dst == max(dst))] ## mean distance between original and reconstructed dive profiles for the optimal
        ## number of BS points summarising the dive.
      }
      #-----------------------------------------------------------------------------------------------------------------------------------
      
      ## Append dataframe
      ndbs <- rbind(ndbs, dbs2) 
      
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
  
  dbs$velocity <- abs(((dbs$depth_end)-(dbs$depth_start))/dbs$dur)
  dbs$foraging <- "hunting" ## "hunting" mode
  dbs$foraging[dbs$sinuosity >= 0.9 & dbs$sinuosity <= 1 & dbs$velocity < 0.4] <- "transit" ## According to Heerah, Hindell, Guinet, and Charrassin (2015) & Heerah (2014)
  dbs$bs_npoints <- "optimal"
  
  ndbs$velocity <- abs(((ndbs$depth_end)-(ndbs$depth_start))/ndbs$dur)
  ndbs$foraging <- "hunting" ## "hunting" mode
  ndbs$foraging[ndbs$sinuosity >= 0.9 & ndbs$sinuosity <= 1 & ndbs$velocity < 0.4] <- "transit" ## According to Heerah, Hindell, Guinet, and Charrassin (2015) & Heerah (2014)
  ndbs$bs_npoints <- paste(tm,"set",sep = "_")
  
})




# Add columns to dbs ------------------------------------------------------

######################################################################################################
######################### This is done for later analysis of segmented dives #########################
######################################################################################################


################# add column for "all.dur" - duration of the dives. ("dur" - is the duration of each segment within a dive)


dbs_tmp <- df_init_tmp2 %>% 
  select(max.d,bottom_depth,bottom_time,'%bt/dt') %>% 
  filter(row_number()==1) %>% 
  right_join(dbs %>% group_by(num) %>% dplyr::mutate("all.dur" = max(end) - min(start)),by = "num")

ndbs_tmp <- df_init_tmp2 %>% 
  select(max.d,bottom_depth,bottom_time,'%bt/dt') %>% 
  filter(row_number()==1) %>% 
  right_join(ndbs %>% group_by(num) %>% dplyr::mutate("all.dur" = max(end) - min(start)),by = "num")

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

bsm_seg_df <-bind_rows(dbs_tmp,ndbs_tmp) %>% arrange(num)
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
divestats <- df_init_tmp2 %>% 
  filter(row_number()==1) %>% 
  select(-Time,-Depth,-Temperature,-External.Temp,-Light.Level) %>% 
  mutate("start" = gmt) %>% 
  select(-gmt,-cor.depth)

#Need to mutate on a column - "optimalBSM" vs "LowResBSM6"
################# add column for transit and hunting time per dive according to BSM segments - Might want to revise this to use df_init_tmp2
ht_dbs <- dbs %>%
  group_by(num) %>%
  filter(foraging == "hunting",dur>4) %>% #calculate the sum time of all hunting segments, excluding those that are <=4sec long.
  summarize("hunting_time" = sum(dur))


time_cols_dbs <- dbs %>%
  group_by(num) %>%
  filter(foraging == "transit") %>%
  summarize("transit_time" = sum(dur)) %>% 
  full_join(ht_dbs)

ht_ndbs <- ndbs %>%
  group_by(num) %>%
  filter(foraging == "hunting",dur>4) %>% #calculate the sum time of all hunting segments, excluding those that are <=4sec long.
  summarize("hunting_time" = sum(dur))


time_cols_ndbs <- ndbs %>%
  group_by(num) %>%
  filter(foraging == "transit") %>%
  summarize("transit_time" = sum(dur)) %>% 
  full_join(ht_ndbs) %>% 
  arrange(num)

# Add pdsi to divestats
pdsi <- (((all_dives %>% filter(row_number() == 1))$Time)[-1] - ((all_dives %>% slice(n()))$Time))
pdsi_df <- tibble(all_dives %>% select(num) %>% unique(),"pdsi" = pdsi)

divestats_ <- bind_rows(time_cols_dbs,time_cols_ndbs) %>% 
  arrange(num) %>% 
  replace_na(list(hunting_time = 0, transit_time = 0)) %>% 
  right_join(divestats,by = "num") %>% 
  arrange(num) %>% 
  movetolast(c("hunting_time","transit_time")) %>% 
  mutate(hunting_time = replace_na(hunting_time, 0), transit_time = ifelse(is.na(transit_time),dur,transit_time)) %>% 
  left_join(pdsi_df,by="num")
  
divestats <- divestats %>% left_join(pdsi_df,by="num")



# Check if any weird data - divestats %>% filter(transit_time==0,hunting_time>0)

##########################  Group dives closest to locations and perform summary stats ##########################

## get mean values for each variables 4 hours either side of each location
## the code below show how to it for bottom time. You need to repeat this for the other variables

## Import tracks analysed by Mia
tracks <- read.csv("Marion_FStracks_SSMresults_2009W_2015S.csv",sep = ";")
# str(tracks)
sealID <- 1
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
  
    
filtered_divestats <-  divestats %>% filter(max.d > 4,dur>60) # filtered dive summaries that may have foraging

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
}
})
loc1$max_max.d <- loc1$max_max.d %>% replace(loc1$max_max.d == -Inf,NaN) #replaces infinities created by loop with NaN values in max_max.d
# Testing for errors in data and indicatoion of correct filtering ("filtered_divestats"). 
# Where has hunting time been calculated (for dives >4m & >15sec), but dives close to location are >4m and 60sec
which(is.na(loc1$dur) & loc1$hunting_time!=0)


# Is the seal hunting in this dive or not?
filtered_divestats$hunting_time/filtered_divestats$transit_time >0.1 #hunting





