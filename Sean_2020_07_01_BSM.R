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



# Set Working directory and import myFunctions ------------------------------------

rm(list = ls())
setwd("C:/Users/Sean Evans/Documents/2020/MSc/Computing/Computing docs")
source("myFunctions.R")

## To setwd() press cntr + Shift + H
getwd()
read_path = "C:/Users/Sean Evans/Documents/2020/MSc/Computing/Fur seals/Dive data for Sean Evans/Extracted raw dive data - from instrument helper/Fur seal data"
save_dt = "C:/Users/Sean Evans/Documents/2020/MSc/Computing/Fur seals/Dive data for Sean Evans/Extracted raw dive data - from instrument helper/Fur seal data/dt_before_TDR"
data_path = "C:/Users/Sean Evans/Documents/2020/MSc/Computing/Fur seals/Dive data for Sean Evans/Extracted raw dive data - from instrument helper/Fur seal data/dt_before_TDR"
save_zoc_df = "C:/Users/Sean Evans/Documents/2020/MSc/Computing/Fur seals/Dive data for Sean Evans/Extracted raw dive data - from instrument helper/Fur seal data/df" 
save_dbs = "C:/Users/Sean Evans/Documents/2020/MSc/Computing/Fur seals/Dive data for Sean Evans/Extracted raw dive data - from instrument helper/Fur seal data/df/dbs"
# Data frame structuring -----------------------------------------------------
#fs_list <- list.files(data_path)
#seal_no <- 1
#load(paste(data_path,fs_list[seal_no],sep = "/"))
## All dataframes have Time column as ISOdate not gmt => Convert Time to gmt and add column 'gmt'

#df$gmt <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + df$Time #turn that ISOdate number into something you can understand




# Looped Data Structuring -------------------------------------------------

fs_list <- list.files(data_path, pattern = ".Rda") # List of ".Rda" files

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
df<-df %>% 
  group_by(num) %>%
  dplyr::mutate("dur" = max(Time) - min(Time), "max.d" = max(cor.depth), "bottom_depth" = max.d *0.8)  


df <- df %>%
  filter(cor.depth > bottom_depth) %>%
  mutate("bottom_time" = max(Time)-min(Time)) %>%
  select("bottom_time") %>%
  unique() %>%
  right_join(df,by = "num") %>%
  movetolast(c("bottom_time"))


# Basic Stats per seal -------------------------------------------------------------
# # Create another data frame for basic dive statistics for each seal
# # no of all dives
# all_dive_no <- length(unique(df$num))
# # select actual dives where df$dur >15 & df$max.d >4
# dives <- df %>% filter(dur>15,max.d>4)
# dive_no <- dives %.>% unique(.$num) %>% length()
# perc <- dive_no/all_dive_no *100
# # mean max depth
# mean_max_d <- dives %.>% mean(.$max.d)
# # mean dive duration
# mean_dur <- df %>% filter(dur>15,max.d>4) %.>% mean(.$dur)# df <- movetolast(df,c("bottom_time"))
# mean_no_obs_per_dive <- dives %>% count() %>% ungroup() %>% select(n) %.>% mean(.$n)

# if mean_no_obs_per_dive != mean_dur or "is not a factor of" - then sampling interval is not constant

# df2 %>% filter(max.d >4) %>% count(max.d) %>% 
#   ggplot(aes(x = num , y = max.d)) +
#   geom_count()


num.list<-unique(df$num)
### Finding the optimal number of Broken points for each dive
#length(unique(df[df$dur >30 & df$max.d <15 & df$max.d >4,]$num)) # = 367 dives for sealID == 1

########
## dataframe in which the dives for which the fit doesn't work will be stored.
# ncdv = data.frame("num" = 0, "Time" = 0, "Time" = 0, "Depth" = 0, "Temperature" = 0, "External.Temp" = 0,
#                   "Light.Level" = 0, "sealID" = 0, "species" = 0, "cor.depth" = 0, "beach" = 0, "Temp_int" = 0,
#                   "Season" = 0, "seal_tag" = 0, "TDR/SPLASH" = 0, "gmt" = 0, "cor.depth" = 0, "dur" = 0,
#                   "max.d" = 0, "bottom_depth" = 0, "bottom_time" = 0) 

ncdv <- data.frame(matrix(integer(), ncol = 19, nrow = 0), stringsAsFactors = FALSE) %>% 
  setNames(nm = c(colnames(df)))

dbs <- data.frame("num" = rep(0, 1), "all.dur" = 0, "start" = 0, "end" = 0, "depth_start" = 0, "depth_end" = 0, "seg" = 0, "npoints" = 0,
                  "dur" = 0, "dur.per" = 0, "coef" = 0, "mean_depth" = 0, "max.depth" = 0, "sinuosity" = 0, "mean_err" = 0, "foraging" = 0) ## Broken stick dataframe

# df[df$cor.depth == max(df$cor.depth),]
########

# foreach () # install package doparallel
for (d in 1:length(num.list)) {  ## Make it 'for (d in 100){' to just play with dive num 100
  print(paste(d,"Out of",length(num.list),sep=" "))
  dive <- df[df$num == num.list[d],] # Dive_Filter(d) is much slower - using a predefined function (triple the time)
  if (dive$dur[1] >15 & dive$max.d[1] >4){
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
                         "depth_end" = 0, "seg" = 0, "npoints" = 0, "dur" = 0, "dur.per" = 0, "coef" = 0, "mean_depth" = 0, "max.depth" = 0,
                         "sinuosity" = 0, "mean_err" = 0, "foraging" = 0)
      
      ## Loop to calculate the different metrics for each broken stick segments
      for (n in 1:(nrow(dep_tim) - 1)) {
        x1 = dep_tim$tim[n] ## start of BS segment
        x2 = dep_tim$tim[n + 1] ## end of BS segment
        dbs2$num[n] = ndive
        dbs2$all.dur[n] = difftime(dive$gmt[nrow(dive)], dive$gmt[1], tz, units = c("secs")) ## dive duration
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
        dbs2$max.depth[n] = max(dive$cor.depth) ## dive max. depth
        
        ## Calculation of vertical sinuosity
        deuc = abs(dep_tim$ref[n + 1] - dep_tim$ref[n]) ## Vertical distance swum between 2 BS points
        dobs = sum(abs(diff(dive$cor.depth[which(dive$gmt == x1):which(dive$gmt == x2)]))) ## sum of all the vertical distances from the original
        ## profile between the two corresponding BS depth points
        dbs2$wiggle = dobs
        dbs2$sinuosity[n] = deuc / dobs ## vertical sinuosity index
        dbs2$mean_err[n] = f$npe[which(dst == max(dst))] ## mean distance between original and reconstructed dive profiles for the optimal
        ## number of BS points summarising the dive.
      }
      
      #-----------------------------------------------------------------------------------------------------------------------------------
      ### IMPORTANT:
      #-----------
      ## Attribution of behaviour according to vertical sinuosity -- Remind that the sinuosity threshold used here was determined according
      ## to the histogram/density plot of vertical sinuosity for every BS segments of every dive
      ## so, before setting your threshold at 0.9, check if it suits your dataset (i.e after running the BS on all your dive)
      
      dbs2$foraging <- 2 ## 2 stands for "hunting" mode
      dbs2$foraging[dbs2$sinuosity >= 0.9 & dbs2$sinuosity <= 1] <- 1 ## 1 stands for "transit" mode
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
    ## allows to keep somewhere the data for which the fit of the Gompertz model didn't work 
  }
  ## end of if loop for dur>15 & max.d>4
  if (d == NROW(num.list)) {
    dbs <- dbs[-1,]
    #save(dbs, file = "17_VDB_2011W_0990468_A160_BSP.Rda")
  }
}
 
setwd(save_dbs)
save(dbs, file = paste(fs_list[1],"b"))
