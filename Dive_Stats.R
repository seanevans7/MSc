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
# Which seal?
#sealtag <- "A146" # used for accessing and loading file 
# (If error = "invalid description argument" then check that seal doesn't have two files associated with it)
sealID <- 2 # Used for saving file 



# Saving/reading files in MSc
save_bsm_seg_df = "bsm_seg_df"
save_df_init_tmp2 = "df_init_tmp2"
save_divestats = "divestats"
save_loc1 = "loc1"
save_dbs = "dbs"
save_ndbs = "ndbs"
save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")


bsm_seg_df <- read_rds(file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")))
df_init_tmp2 <- read_rds(file.path(save_df_init_tmp2,paste(sealID,"_df_init_tmp2.rds",sep = "")))
divestats <- read_rds(file.path(save_divestats,paste(sealID,"_divestats.rds",sep = "")))
#filtered_divestats <- read_rds(file.path(save_filtered_divestats,paste(sealID,"_filtered_divestats.rds",sep = "")))
loc1 <- read_rds(file.path(save_loc1,paste(sealID,"_loc1.rds",sep = "")))


divessummary <- read_csv(file.path(save_divessummary,"divessummary.csv"))
Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')

#################### Recalibrating divestats times #################
#divessummary
divessummary$local_time = force_tz(divessummary$local_time, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$sunrise = force_tz(divessummary$sunrise, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$sunset = force_tz(divessummary$sunset, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$dawn = force_tz(divessummary$dawn, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$dusk = force_tz(divessummary$dusk, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600


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


############# Mean vs media pdsi are very different (Data are skewed heavily to the right)
# Therefore probably better to use median pdsi
mean(divestats["pdsi"][divestats['pdsi']>0 & divestats['pdsi']<3600])
mean(divestats["pdsi"][divestats['pdsi']>0 & divestats['pdsi']>0])
median(divestats["pdsi"][divestats['pdsi']>0 & divestats['pdsi']<3600])
median(divestats["pdsi"][divestats['pdsi']>0 & divestats['pdsi']>0])
hist(divestats["pdsi"][divestats['pdsi']>0 & divestats['pdsi']<200])


divestats %>% filter(diel_phase=='Night') %>% summarise('Night' = mean(max.d))

# Data Analysis -----------------------------------------------------------


bsm_seg_df %>%  filter(dur == max(bsm_seg_df$dur))
bsm_seg_df %>%  filter(num == 6994) 
bsm_seg_df %>% filter(foraging == 'hunting',mean_depth>20) %.>% hist(.$mean_depth)
bsm_seg_df %>% filter(foraging == 'transit',mean_depth>20) %.>% hist(.$mean_depth)
bsm_seg_df$mean_depth %>% boxplot()
bsm_seg_df %>% filter(foraging == 'hunting',mean_depth>20) %.>% var(.$all.dur)
#var.test((bsm_seg_df %>% filter(foraging == 'hunting',mean_depth>20) %>% dplyr::select(mean_depth)),(bsm_seg_df)["mean_depth"])
t.test((bsm_seg_df %>% filter(foraging == 'hunting',mean_depth>20) %>% dplyr::select(mean_depth)),(bsm_seg_df %>% filter(foraging == 'transit',mean_depth>20) %>% dplyr::select(mean_depth)),var.equal = FALSE)


#Shallow dives vs deep dives
filtered_divestats$max.d %>% plot()
filtered_divestats$max.d %>% hist().




plot(divestats %>% filter(pdsi<300,pdsi>0,max.d>20) %>% ungroup() %>% select(pdsi,max.d))
cor(divestats %>% filter(pdsi<300,pdsi>0,max.d>20) %>% ungroup() %>% select(pdsi,max.d))

bsm_seg_df %>% group_by(diel_phase,foraging) %>% filter(vdist<5,dur>3) %>%  summarise('hd' = mean(mean_depth), 'hd_s' = sd(mean_depth),
                                                                                      'ht' = mean(all.dur), 'ht_s' = sd(all.dur),
                                                                                      'ss' = mean(swim_speed), 'ss_s' = sd(swim_speed))

divestats %>% group_by(diel_phase) %>% filter(hunting_time>0) %>% summarise('n' = NROW(hunting_time),
                                                                      'ht' = mean(hunting_time),
                                                                      'ht_s' = sd(hunting_time),
                                                                      'ht.q1' = summary(hunting_time)[2],
                                                                      'ht.q3' = summary(hunting_time)[5],
                                                                      'ht_rat' = mean(ht_rat),
                                                                      'hd' = median(Mdepth_hunting),
                                                                      'de' = median(dive_efficiency),
                                                                      'Surf_T' = median(External.Temp),
                                                                      'Surf_T_s' = sd(External.Temp),
                                                                      'trans_t' = median(transit_time),
                                                                      'trans_t_s' = sd(transit_time))

qqplot(bsm_seg_df %>% filter(foraging == 'hunting') %>% select(vdist))
# Where X is a lat lon ID pair:
hist((divestats %>% group_by(X) %>% summarise(meanht = mean(hunting_time)))$meanht)
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

#### hunting vs transit

mod.null <- glm(mode-1~+1, family="binomial", data=na.omit(filtered_divestats))
mod1 <- glm(mode-1~strat+delta+bottom+travel+log(pdsi)+wiggles, family="binomial", data=na.omit(loc1))
summary(mod1)
AIC(mod.null)-AIC(mod1)
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









#summary of first 100 dives' optimal no of BSPs to use in entire seal dataset
dbs <- dbs %>% group_by(num,foraging)

dbs %>% slice(1) %>% select(npoints) %.>% hist(.$npoints) 

bsm_seg_df %>% filter(max.d>20) %>% ungroup() %>% do(tidy(t.test(.$mean_depth ~ .$foraging),alternative="greater"))

bsm_seg_df %>% ungroup() %>% do(tidy(t.test(.$mean_depth ~ .$foraging)))
bsm_seg_df %>% filter(max.d>20) %>% ungroup() %>% do(tidy(t.test(.$mean_depth ~ .$foraging)))

levels(bsm_seg_df$foraging)

abline(lm(dout$duration~dout$depth),col="red")

# Quite r session ---------------------------------------------------------

#q("yes")






#######################


# Simple t-tests and GLMs -------------------------------------------------

# ##Do the behaviours differ in transit vs foraging modes
# #remember, mode 2 = transit and mode 1 =ARS, while vARS is based on hunting time or 'hunting' vs 'transit'


#%% bottom_time
p_value = tidy(t.test(loc1$bottom_time[loc1['b.5']==1], loc1$bottom_time[loc1['b.5']==2]))['p.value']

par(mfrow=c(1,1))
##plot the results
t <- tapply(loc1$bottom_time, loc1['b.5'], mean, na.rm=T)
s <- tapply(loc1$bottom_time, loc1['b.5'], sd, na.rm=T)
n <- tapply(loc1$bottom_time, loc1['b.5'], length)
se <- s/sqrt(n)*2
mp <- barplot(t, beside = TRUE,
              col = c("red", "grey"),
              legend = rownames(t), ylim= c(0,max(t+se, na.rm=T)),
              main = "Bottom Time", font.main = 4,
              cex.names = 1.5)

segments(mp, t+se, mp, t-se,  col = "black", lwd = 3)

#%% hunting time
p_value = tidy(t.test(loc1$hunting_time[loc1['b.5']==1], loc1$hunting_time[loc1['b.5']==2]))['p.value']

par(mfrow=c(1,1))
##plot the results
t <- tapply(loc1$hunting_time, loc1['b.5'], mean, na.rm=T)
s <- tapply(loc1$hunting_time, loc1['b.5'], sd, na.rm=T)
n <- tapply(loc1$hunting_time, loc1['b.5'], length)
se <- s/sqrt(n)*2
mp <- barplot(t, beside = TRUE,
              col = c("red", "grey"),
              legend = rownames(t), ylim= c(0,max(t+se, na.rm=T)),
              main = "Bottom Time", font.main = 4,
              cex.names = 1.5)

segments(mp, t+se, mp, t-se,  col = "black", lwd = 3)







# Plotting ----------------------------------------------------------------



################## Plotting ARS where thermocline is present ###########
sf_Marion <- data.frame("lon" = 37.746368, "lat" = -46.893361) %>% 
  st_as_sf(coords = c("lon", "lat"),crs=4326,remove = FALSE)

ggplot() + 
  geom_point(aes(x = lon , y = lat), color='green', size=5,pch = 19,data = (divessummary %>% filter(Thermocline=='present'))[,c(10,30,31)]) +
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='red', data = sf_Marion) +
  ggtitle(paste('Seal',sealID,sep = " "))

## Map the distribution of a single seals behaviours
ggplot() + 
  geom_point(aes(x = lon , y = lat), color='green', size=8,pch = 19,data = (divessummary %>% filter(Thermocline=='present'))[,c(10,30,31)]) +
  geom_point(aes(x = lon , y = lat, color = hunting_time, size = heffort), pch = 19,data = loc1) +
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='red', data = sf_Marion) +
  geom_point(aes(x = lon , y = lat),pch = 1,size=max(loc1$heffort)+1, color='red', data = loc1[loc1$'b.5'==2,]) +
  ggtitle(paste('Seal',sealID,sep = " "))



# 

######################  Plotting per hour stats
x <- divestats$local_time
y <- divestats$max.d # change to the variable of interest
hour <- format(x, "%H")

# ##this plots the variable as a function of time of day
# group <- floor(y/.10)*10 #consider changing the scaling factor
# mat <- tapply(x, list(hour, group*-1), length)
# mat[is.na(mat)] <- 0
# title.1 <- paste('1', "depth,  n= ", max(divestats$num))
# 
# par(mfrow=c(2,2))
# hist(y[y>6], breaks=50)  ##plot a frequency distribution for a particular variable
# plot(x,y*-1,  type="l") # Plots two variable against each other
# image.plot(mat, xlab="time of Day", ylab="depth") # does the time of day image plot
# plot(mat)


group <- floor(y/.10)*10 #consider changing the scaling factor
mat <- tapply(x, hour, length)
mat[is.na(mat)] <- 0
title.1 <- paste('1', "depth,  n= ", max(divestats$num))

par(mfrow=c(2,2))
hist(y[y>6], breaks=50)  ##plot a frequency distribution for a particular variable
plot(x,y*-1,  type="l") # Plots two variable against each other
image.plot(mat, xlab="time of Day", ylab="depth") # does the time of day image plot

splom(divestats %>% ungroup() %>% filter(all.dur>60) %>% select(c(1,16,17,18,19,20,22,24)), 
      panel = function(x, y, pch=19,cex.lab=0.001,cex.axis=0.001,cex.sub=0.001,ps=0.001,...) {
        panel.xyplot(x, y, pch=19,cex.lab=0.001,cex.axis=0.001,cex.sub=0.001,ps=0.001,aspect = "xy",...)
        panel.lmline(x,y,pch=19,cex.lab=0.001,cex.axis=0.001,cex.sub=0.001,ps=0.001,...)
      }
)




# adding foraging trip to divestats ---------------------------------------



sealID <- 1
save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")
divessummary <- read_csv(file.path(save_divessummary,"divessummary.csv"))
Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')

#################### Recalibrating divestats times #################
#divessummary
divessummary$local_time = force_tz(divessummary$local_time, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$sunrise = force_tz(divessummary$sunrise, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$sunset = force_tz(divessummary$sunset, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$dawn = force_tz(divessummary$dawn, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600 
divessummary$dusk = force_tz(divessummary$dusk, tzone = "Africa/Addis_Ababa", roll = FALSE)  + 3*3600

#############
Fts_summaries_init = Fts_summaries %>% filter(id == sealID) %>% dplyr::select(ft.start,ft.end,meandir,sddirDEG,ani_n)

Fts_summaries_init$ft_ht <- NA # sum of hunting time for all dives within this foraging trip

##run a loop over the foraging trip data, grabbing the corresponding dive data
system.time({
  for(i in 1: (nrow(Fts_summaries_init))) {
    print(paste(i,nrow(Fts_summaries_init)-1),sep=" ")
    tmin <- Fts_summaries_init$'ft.start'[i]
    tmax <- Fts_summaries_init$'ft.end'[i]
    mvar <- divessummary[divessummary$start >= tmin & divessummary$start <= tmax,]
    Fts_summaries_init$ft_ht[i] <- mvar %>% filter(hunting_time>0) %>% dplyr::select(hunting_time) %>% sum()
  }
})

divessummary$ft <- NA # Adding ft column to divestats for later stats
##run a loop over the divessummary data, grabbing the corresponding foraging trip id
system.time({
  for(i in 1: (nrow(Fts_summaries_init))) {
    print(paste(i,nrow(Fts_summaries_init)-1),sep=" ")
    tmin <- as.POSIXct(Fts_summaries_init$'ft.start'[i])
    tmax <- as.POSIXct(Fts_summaries_init$'ft.end'[i])+24*3600
    divessummary$ft[divessummary$start>=tmin & divessummary$start<=tmax] = i
  }
})
unique(divessummary$ft)

# % of hunting time to total dive time of dives within the mixed layer
divessummary %>% filter(Thermocline=='absent') %>% summarise(mean(hunting_time)/(mean(transit_time)+mean(hunting_time))*100)
# % of hunting time to total dive time of dives the mixed layer
divessummary %>% filter(Thermocline=='present') %>% summarise(mean(hunting_time)/(mean(transit_time)+mean(hunting_time))*100)


### Why is there no value for Tmld????????????
divessummary %>% filter(Tmld>0)

# Add column for bouts
time_diff <- difftime(divessummary$start, lag(divessummary$start, default = divessummary$start[1]), units = "sec")
divessummary$bout <- cumsum(ifelse(time_diff<3600*6,0,1))
