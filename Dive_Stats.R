##############################################################################################################################
##################### Sean_fseal_VARS.R: Is a file containg code to process fur seal diving data. ############################
#####################             A vertical area restricted search method is used.               ############################
##############################################################################################################################

##############################################################################################################################
############################################# Post-processing of dives #######################################################
#############################################      By Sean Evans       #######################################################
##############################################################################################################################


# Load packages -----------------------------------------------------------
library(purrr)
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

rm(list = ls())
# Which seal?
sealtag <- "A146" # used for accessing and loading file 
# (If error = "invalid description argument" then check that seal doesn't have two files associated with it)
sealID <- 24 # Used for saving file 



# Saving/reading files in MSc
save_bsm_seg_df = "bsm_seg_df"
save_df_init_tmp2 = "df_init_tmp2"
save_divestats = "divestats"
save_loc1 = "loc1"
save_dbs = "dbs"
save_ndbs = "ndbs"


bsm111 <- read_rds(file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")))
                   
                   
                   
                   
                   
# Basic Stats per seal -------------------------------------------------------------

############################################################################################################################## 
######################## Description: Some basic stats for diving parameters for each seal.  #################################
##############################################################################################################################


## Checking for sampling interval difference between and within dives.
# boxplot(data.frame(diff(df_init_tmp2$Time)) %>% filter(diff.df_init_tmp2.Time. >10,diff.df_init_tmp2.Time.<30))# where is there missing data? and how much is missing?
# df_init_tmp2[which(diff(df_init_tmp2$Time)>1 & df_init_tmp2$max.d<4),]$num %>% unique() %>% length() # Plenty of dives with interval > 1
n_int_err <- df_init_tmp2[which(diff(df_init_tmp2$Time)>1 & df_init_tmp2$max.d>4),] # In this seal only x points in dives with interval > 1


# n_all = All dives 
n_all <- df_init_tmp2 %.>% unique(.$num) %>% length()

# n_dives = number of dives used in analysis for foraging (sinuosity using BSM - optimal vs low res methods)
n_dives <- all_dives %.>% unique(.$num) %>% length()

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


# Data Analysis -----------------------------------------------------------


dbs %>%  filter(dur == max(dbs$dur))
dbs %>%  filter(num == 6994) 
dbs %>% filter(foraging == 2,mean_depth>20) %.>% hist(.$mean_depth)
dbs %>% filter(foraging == 1,mean_depth>20) %.>% hist(.$mean_depth)
dbs$mean_depth %>% boxplot()
dbs %>% filter(foraging == 2,mean_depth>20) %.>% var(.$all.dur)
var.test((dbs %>% filter(foraging == 2,mean_depth>20) %>% select(mean_depth)),(dbs %>% filter(foraging == 1,mean_depth>20))["mean_depth"])
t.test((dbs %>% filter(foraging == 2,mean_depth>20) %>% select(mean_depth)),(dbs %>% filter(foraging == 1,mean_depth>20) %>% select(mean_depth)),var.equal = FALSE)



#Shallow dives vs deep dives
filtered_divestats$max.d %>% plot()
filtered_divestats$max.d %>% hist().




plot(divestats %>% filter(pdsi<300,pdsi>0,max.d>20) %>% ungroup() %>% select(pdsi,max.d))
cor(divestats %>% filter(pdsi<300,pdsi>0,max.d>20) %>% ungroup() %>% select(pdsi,max.d))



# Simple t-tests and GLM --------------------------------------------------


########################################################################################################################
################################################ Simple stats on dives  ################################################
####################### Are there signiifcant differences between hunting vs transit dives  ############################
####################### shallow vs deep dives, species, age, mass(kg),....                  ############################
########################################################################################################################

#Shallow dives vs deep dives
filtered_divestats$max.d %>% plot()
filtered_divestats$max.d %>% hist()


# ##Do the behaviours differ in transit vs foraging modes
# #remember, mode 2 = transit and mode 1 =ARS
t.test(loc1$bottom[loc1$mode==1], loc1$bottom[loc1$mode==2])

par(mfrow=c(1,1))
##plot the results
t <- tapply(loc1$bottom, loc1$mode, mean, na.rm=T)
s <- tapply(loc1$bottom, loc1$mode, sd, na.rm=T)
n <- tapply(loc1$bottom, loc1$mode, length)
se <- s/sqrt(n)*2
mp <- barplot(t, beside = TRUE,
              col = c("red", "grey"),
              legend = rownames(t), ylim= c(0,max(t+se, na.rm=T)),
              main = "Bottom Time", font.main = 4,
              cex.names = 1.5)

segments(mp, t+se, mp, t-se,  col = "black", lwd = 3)

t.test(loc1$depth[loc1$mode==1], loc1$depth[loc1$mode==2])
t.test(loc1$duration[loc1$mode==1], loc1$duration[loc1$mode==2])
t.test(loc1$travel[loc1$mode==1], loc1$travel[loc1$mode==2])
t.test(loc1$pdsi[loc1$mode==1], loc1$pdsi[loc1$mode==2])
t.test(loc1$wiggles[loc1$mode==1], loc1$wiggles[loc1$mode==2])

##...but it is best to not do multiple t tests. Better to incorporate everything in one model
## in this case the data are binomial so it needs a GLM
mod.null <- glm(mode-1~+1, family="binomial", data=na.omit(loc1))
mod1 <- glm(mode-1~depth+duration+bottom+travel+log(pdsi)+wiggles, family="binomial", data=na.omit(loc1))
summary(mod1)
AIC(mod.null)-AIC(mod1) ##this value needs to be more than 2 to be confident the models differ
##plot up the results - be sure to include only the terms that were indicated as important

plot(effect("depth", mod1))
plot(effect("duration", mod1))
plot(effect("bottom", mod1))
plot(effect("wiggles", mod1))
plot(effect("log(pdsi)", mod1))

##############################################################################
## Step 5: Map the distribution of behaviours
par(mfrow=c(2,1))
##first the modes
#define the plot limits
theXlim <- c((min(loc1$lon)-1), (max(loc1$lon)+1))
theYlim <- c((min(loc1$lat)-1), (max(loc1$lat)+1))

plot(loc1$lon,loc1$lat,col=loc1$mode, pch=19, xlim=theXlim, ylim=theYlim, main="Behavioural Mode")
map(resolution=0, add=T)
legend(theXlim[1]+1, theYlim[2]-1,c("Transit","Search"),fill=c(2,1),cex=1) ##you might need to change the position

##then the behaviours: this example is for bottom time
colfun <- colorRampPalette(c("dark blue", "yellow", "dark red"))
fmr.brks<-seq(min(loc1$bottom, na.rm=T), max(loc1$bottom, na.rm=T), length=101)
plot(loc1$lon,loc1$lat,
     xlim=theXlim,
     ylim=theYlim,
     main="Bottom Time (secs)",
     col=colfun(100)[findInterval(loc1$bottom, fmr.brks)],
     pch=19,
     cex=1.5)
map(resolution=0, add=T)
legend(theXlim[1]+1, theYlim[2]-1,legend=floor(fmr.brks[c(1, 25, 50, 75, 100)]), fill=colfun(100)[c(1, 25, 50, 75, 100)],cex=1)


#save the worksheet - this is important!
out.f <- paste(seal, "_divesummary.Rdata", sep="") #sets up the output file name
out.f
# head(loc1)
save.image(out.f)

write.csv(out,file.path(resDir,paste(seal,"_divesummary.csv")))
head(dout)
write.csv(loc1,file.path(resDir,paste(seal,"_LOCATIONdivesummary.csv")))




# moveVis gifs and videos for presentations -------------------------------

###########################################################################################################################
######################################### MoveVis gifs and videos for presentations #######################################

# use df2move to convert the data.frame into a moveStack
l <- df2move(loc1,
             proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0,0,0,0,0", #+init=epsg:4326 
             x = "lon", y = "lat", time = "gmt", track_id = "id")


# ggplot()+geom_point(data=loc1, aes(x=lon, y=lat), color=colfun(100)[findInterval(loc1$bottom, fmr.brks)])
ggplot()+geom_point(data=loc1, aes(x=lon, y=lat))+
  scale_fill_identity(loc1$hunting_time)


l<-align_move(l)
frames <- frames_spatial(l, path_colours = c("red"),path_legend_title = "seal")
length(frames) # number of frames
frames[[10]] # display one of the frames
animate_frames(frames, out_file = "example_1.gif")











# Testing loop ------------------------------------------------------------



# Model initialization -------------------------------------------------------------------

############################################################################################################################## 
######################## Description: Model used to access first 100 dives. Find summary of BSpoints. ########################
##############################################################################################################################


num.list<-unique(all_dives$num)
### Finding the optimal number of Broken points for each dive
#length(unique(df[df$dur >30 & df$max.d <15 & df$max.d >4,]$num)) # = 367 dives for sealID == 1

########
## dataframe in which the dives for which the fit doesn't work will be stored.
# ncdv = data.frame("num" = 0, "Time" = 0, "Time" = 0, "Depth" = 0, "Temperature" = 0, "External.Temp" = 0,
#                   "Light.Level" = 0, "sealID" = 0, "species" = 0, "cor.depth" = 0, "beach" = 0, "Temp_int" = 0,
#                   "Season" = 0, "seal_tag" = 0, "TDR/SPLASH" = 0, "gmt" = 0, "cor.depth" = 0, "dur" = 0,
#                   "max.d" = 0, "bottom_depth" = 0, "bottom_time" = 0) 

# ncdv <- data.frame(matrix(integer(), ncol = 20, nrow = 0), stringsAsFactors = FALSE) %>% 
#   setNames(nm = c(colnames(df_init_tmp2)))


dbs <- data.frame("num" = rep(0, 1), "all.dur" = 0, "start" = 0, "end" = 0, "depth_start" = 0, "depth_end" = 0, "seg" = 0, "npoints" = 0,
                  "dur" = 0, "dur.per" = 0, "coef" = 0, "mean_depth" = 0, "max.depth" = 0, "wiggle" = 0, "sinuosity" = 0, "mean_err" = 0, "foraging" = 0, "velocity" = 0) ## Broken stick dataframe

# df[df$cor.depth == max(df$cor.depth),]
########


# foreach () # install package doparallel
system.time({
  num_seq <- seq(1,100) #first 100 dives    #length(num.list))   ## Make it 'for (d in 100){' to just play with dive num 100
  
  for (d in 1:length(num_seq)) {  ## Make it 'for (d in 100){' to just play with dive num 100
    print(paste(d,"Out of",length(num_seq),sep=" "))
    dive <- all_dives[all_dives$num == num.list[d],]
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
    } #else { ncdv <- rbind(ncdv, dive) }
    
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
  dbs$foraging <- as.factor(dbs$foraging)
  ## According to Heerah, Hindell, Guinet, and Charrassin (2015) & Heerah (2014)
  
})

#summary of first 100 dives' optimal no of BSPs to use in entire seal dataset
dbs <- dbs %>% group_by(num,foraging)

dbs %>% slice(1) %>% select(npoints) %.>% hist(.$npoints) 

bsm_seg_df %>% filter(max.d>20) %>% ungroup() %>% do(tidy(t.test(.$mean_depth ~ .$foraging)))

bsm_seg_df %>% ungroup() %>% do(tidy(t.test(.$mean_depth ~ .$foraging)))
bsm_seg_df %>% filter(max.d>20) %>% ungroup() %>% do(tidy(t.test(.$mean_depth ~ .$foraging)))

levels(bsm_seg_df$foraging)

abline(lm(dout$duration~dout$depth),col="red")

# Quite r session ---------------------------------------------------------

q("yes")
