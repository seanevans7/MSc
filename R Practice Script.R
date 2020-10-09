
# Practice ----------------------------------------------------------------



##############################################################################################################################
####################### Reading in seal tracks produced by Mia. This will be useful for horizontal   ###############
#######################         dives and averaging the dives that are > 60sec long.                           ###############
##############################################################################################################################
## Import tracks analysed by Mia
tracks <- read.csv("Marion_FStracks_SSMresults_2009W_2015S.csv",sep = ";")
# str(tracks)

loc1 <-droplevels(subset(tracks,tracks$id == sealID)) %>% filter(gmt > first(df_init_tmp2$gmt) & gmt < last(df_init_tmp2$gmt))
loc1$gmt <- as.POSIXct(loc1$gmt,format = "%Y-%m-%d %H:%M", tz = "GMT")
# strptime()
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

divestats <- bsm_seg_df %>% 
  group_by(num) %>%
  filter(foraging == "transit") %>%
  summarize("transit_time" = sum(dur)) %>% 
  full_join(bsm_seg_df %>% 
              group_by(num) %>%
              filter(foraging == "hunting") %>% #calculate the sum time of all hunting segments
              summarize("hunting_time" = sum(dur)), by="num") %>% 
  arrange(num) %>% 
  #post dive surface interval between possible foraging dives (i.e. between dives >60s long & 4m deep)
  full_join(tibble(df_init_tmp2 %>% 
                     filter(max.d > 4, all.dur > 60) %>% 
                     select(num) %>% 
                     unique(),
                   "pdsi" = (((df_init_tmp2 %>% filter(max.d > 4, all.dur > 60) %>% slice(1))$Time)[-1] - ((df_init_tmp2 %>% filter(max.d > 4, all.dur > 60) %>% slice(n()))$Time)))) %>% 
  replace_na(list(hunting_time = 0, transit_time = 0, pdsi = NaN)) %>% 
  arrange(num) %>% 
  right_join(df_init_tmp2 %>% 
               filter(row_number()==1) %>% 
               select(-Time,-Depth,-Temperature,-External.Temp,-Light.Level) %>% 
               mutate("start" = as.POSIXct(strptime(gmt,format = "%Y-%m-%d %H:%M", tz = "GMT"))) %>% 
               select(-gmt,-cor.depth)
             ,by = "num") %>% 
  mutate(hunting_time = replace_na(hunting_time, 0), transit_time = ifelse(is.na(transit_time),dur,transit_time))
divestats$end <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + (df_init_tmp2 %>% slice(n()))$Time


## Set up dataframes  to add columns lat, lon, X
divestats <- divestats[-1,]
loc1$X <- as.factor(loc1$X)
z <- seq(length(tmin))
z <- z[1:(length(z)-1)]
z <- cut(divestats$start,breaks = tmin, labels = z)
divestats <- divestats %>% 
  mutate("X" = z) %>% 
  left_join(loc1 %>% select(X,lon,lat),by = "X")


# Check if any weird data - divestats %>% filter(transit_time==0,hunting_time>0)  

## filtered dive summaries that may have foraging
filtered_divestats <-  divestats %>% filter(max.d > 4,all.dur>60) #Probably foraging dives according to Heerah (2014), Heerah (2015), 
#SES - (15m,60sec); Weddels - (4m,60sec)()

# Frequency dist of dive depths bi-modal? Two groups of dive depth separated at how many meters?

# Can we exclude dives < than a certain depth, from further analysis (% of dives longer than 60 sec?)
# as they may indicate non-foraging activities?

# According to Arthur (2016) - Only excursions to >6 m were analysed (Staniland & Robinson 2008) as we 
# found the broken stick algorithm typically did not fit dives shallower than this (see results).
# We found the BSt algorithm typically did not fit short and shallow dives, as the model could not detect 
# an inflection point (see Results). Consequently, only dives >40 s of duration were included in the analysis.

## Adding whether dive is a hunting dive or not according to ht_rat (ratio of hunting time to dive time)
filtered_divestats$ht_rat <- filtered_divestats$hunting_time/filtered_divestats$all.dur  #hunting to transit ratio
#visually decide on threshold h below
boxplot(filtered_divestats$ht_rat[filtered_divestats$ht_rat<4 & filtered_divestats$ht_rat>0])


h <- 0.25 #threshold for "ht_rat' above which seal is hunting in the dive (Spends half the dive hunting) 
# p = % of dives where hunting time is considerably small large enough that the dive can be seen as a foraging dive
p <- length(filtered_divestats$ht_rat[filtered_divestats$ht_rat>h])/length(filtered_divestats$ht_rat)

# Considering the calculation below, i'd say that bottom time is not a good indicator of foraging in deep dives.
length(filtered_divestats$'%bt/dt'[filtered_divestats$'%bt/dt'>0.4 & filtered_divestats$max.d >100])/length(filtered_divestats$'%bt/dt'[filtered_divestats$max.d >100])
#### Adding "hunt_dive" column to divestats and filtered_divestats
divestats <- filtered_divestats %>%  # More than 25% of dive time is foraging time
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




# filtering tracks  -------------------------------------------------------
# Raw location data (Before SSM)
######### Filter the tracks using Erroneous GPS locations were filtered using a maximum transit rate of 2.5 m s-1 (Tremblay et al. 2006; Kuhn et al. 2009b). 
######### Tracks were then interpolated using a hermite curve to associate each dive with a location (Tremblay et al. 2006).

install.packages("argosfilter")
library(argosfilter)
raw_tracks<-read.csv("MarionIsland_FurSeal_RawTracks_2009_2015.csv",sep=";")
id = 1
sealdf <- raw_tracks[raw_tracks$id==1,]
sealdf$gmt <- as.POSIXct(paste(as.Date(sealdf$date), sealdf$time))
lat<-sealdf$lat
lon<-sealdf$lon
dtime<-sealdf$gmt
lc<-sealdf$lc

# plot unfiltered data
plot(lon,lat,col="lightgrey",type="l",xlab="Longitude",ylab="Latitude")

# filter by speed only
mfilter<-vmask(lat,lon,dtime,2.5)
mfilter[1:10]
lines(lon[which(mfilter=="not")],lat[which(mfilter=="not")],col="red")

# filter data using sdafilter
cfilter<-sdafilter(lat, lon, dtime, lc)
cfilter[1:20]
lines(lon[which(cfilter=="not")],lat[which(cfilter=="not")],col="blue")
lines(lon[which(cfilter=="removed")],lat[which(cfilter=="removed")],col="red")
# check number of locations (by location class) removed by each filter
table(lc,mfilter)
table(lc,cfilter)

# Tracks were then interpolated using a hermite curve to associate each dive with a location (Tremblay et al. 2006).
spline()
pchip()








##############################################################################
## Map the distribution of behaviours
ggplot() + 
  geom_point(aes(x = lon, color = b.5, y = lat, size = ht_rat), data = loc1)

# par(mfrow=c(1,1))
# ##first the modes
# #define the plot limits
# theXlim <- c((min(loc1$lon)-1), (max(loc1$lon)+1))
# theYlim <- c((min(loc1$lat)-1), (max(loc1$lat)+1))
# # ft <- loc1 %>% filter(ft==3)
# # plot(ft$lon,ft$lat, pch=19, xlim=theXlim, ylim=theYlim, main="Behavioural Mode")
# plot(loc1$lon,loc1$lat, pch=19, xlim=theXlim, ylim=theYlim, main="Behavioural Mode")
# plot(loc1$lon,loc1$lat, pch=19, xlim=theXlim, ylim=theYlim, main="Behavioural Mode")
# # Map(resolution=0, add=T)
# legend(theXlim[1]+1, theYlim[2]-1,c("Transit","Search"),fill=c(2,1),cex=1) ##you might need to change the position



# fsle plotting - rather do this in python -----------------------------------------------------------

#fsle <- brick("C:/Users/Sean Evans/Documents/2020/MSc/Data/AVISO eddy tracking/seal1_VDB_2009W_0890438_GW522/dataset-duacs-dt-global-allsat-madt-fsle_1596196018265.nc")
#plot(fsle[[1]])
#e<-extent(min(seal$lon),max(seal$lon),min(seal$lat),max(seal$lat))
#r.crop <- crop(fsle,e)
#plot(r.crop[[10]]);par(new=TRUE)
#plot(seal$lon,seal$lat, pch=19 ,add=TRUE,useRaster=TRUE)
# extract()


#Reading netcdf file and extracting lat, lon and variable
fsle <- nc_open("C:/Users/Sean Evans/Documents/2020/MSc/Data/AVISO eddy tracking/seal1_VDB_2009W_0890438_GW522/dataset-duacs-dt-global-allsat-madt-fsle_1596196018265.nc")
lon <- ncvar_get(fsle, varid = "lon")
lat <- ncvar_get(fsle, varid = "lat")
time <- ncvar_get(fsle, varid = "time")
tunits <- ncatt_get(fsle, "time", "units")
fsle_max <- ncvar_get(fsle, "fsle_max")
fsle_max_units <- ncatt_get(fsle, "fsle_max", "units")
#nc_close(fsle)
mean1 <- apply(fsle_max,1,mean)
plot(mean1)
fsle_max[fsle_max<c(-0.2)]
## seal track
sealID = 1
tracks <- read.csv("Marion_FStracks_SSMresults_2009W_2015S.csv",sep = ";")
tracks$gmt <- as.POSIXct(tracks$gmt,format = "%Y-%m-%d %H:%M", tz = "GMT")
seal <-droplevels(subset(tracks,tracks$id == sealID)) %>% filter(gmt > first(df_init_tmp2$gmt) & gmt < last(df_init_tmp2$gmt))
## lat & lon domain
e<-extent(min(seal$lon),max(seal$lon),min(seal$lat),max(seal$lat))

## One day fsle.max
fsle_max.slice <- fsle_max[, , 1]
dim(fsle_max.slice)

# r <- raster(t(fsle_max.slice), crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0,0,0,0,0"))
r <- raster(fsle_max.slice, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0,0,0,0,0"))
extent(r)<-e
# r <- flip(r, direction='y')
theXlim <- c((min(seal$lon)-1), (max(seal$lon)+1))
theYlim <- c((min(seal$lat)-1), (max(seal$lat)+1))

ggplot(seal,aes(lon,lat)) + 
  geom_point() +
  geom_point(fsle_max,aes())

plot(r,useRaster=TRUE,xlim=theXlim, ylim=theYlim);par(new=TRUE)
plot(loc1$lon,loc1$lat, pch=19, size=loc1$ht_rat, xlim=theXlim, ylim=theYlim ,add=TRUE)

# add seal 
#add countries!!!!

library(maptools)
data(wrld_simpl)
plot(wrld_simpl, add = TRUE)


