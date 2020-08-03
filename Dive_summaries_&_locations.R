##############################################################################################################################
##################### Sean_fseal_VARS.R: Is a file containg code to process fur seal diving data. ############################
#####################             A vertical area restricted search method is used.               ############################
##############################################################################################################################


# Locations ---------------------------------------------------------------


##############################################################################################################################
####################### Reading in seal tracks produced by Mia. This will be useful for horizontal   ###############
#######################         dives and averaging the dives that are > 60sec long.                           ###############
##############################################################################################################################
## Import tracks analysed by Mia
tracks <- read.csv("Marion_FStracks_SSMresults_2009W_2015S.csv",sep = ";")
# str(tracks)

loc1 <-droplevels(subset(tracks,tracks$id == sealID)) %>% filter(gmt > first(df_init_tmp2$gmt) & gmt < last(df_init_tmp2$gmt))
loc1$gmt <- strptime(loc1$gmt,format = "%Y-%m-%d %H:%M", tz = "GMT")

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
  summarize("transit_time" = sum(dur),"Mvdist_err_transit"= mean(vdist)/2,"Mdepth_transit"=mean(mean_depth)) %>% 
  # Mvdist_err_transit - mean error of the mean_depth per segment- across all transit segments of the dive
  # Mdepth_transit - mean of mean_depth of each transit segment for each dive
  full_join(bsm_seg_df %>% 
              group_by(num) %>%
              filter(foraging == "hunting") %>% #calculate the sum time of all hunting segments
              summarize("hunting_time" = sum(dur),"Mvdist_err_hunting"= mean(vdist)/2,"Mdepth_hunting"=mean(mean_depth))
            # Mvdist_err_transit - mean error of the mean_depth per segment- across all hunting segments of the dive
            # Mdepth_transit - mean of mean_depth of each hunting segment for each dive
            , by="num") %>% 
  arrange(num) %>% 
  #post dive surface interval between possible foraging dives (i.e. between dives >60s long & 4m deep)
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
  mutate(hunting_time = replace_na(hunting_time, 0), transit_time = ifelse(is.na(transit_time),all.dur,transit_time)) 

divestats$end <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT") + (df_init_tmp2 %>% slice(n()))$Time

# Check if any weird data - divestats %>% filter(transit_time==0,hunting_time>0)  

## filtered dive summaries that may have foraging
filtered_divestats <-  divestats %>% filter(max.d > 4, all.dur>60)
## Adding whether dive is a hunting dive or not according to ht_rat (ratio of hunting time to dive time)
filtered_divestats$ht_rat <- filtered_divestats$hunting_time/filtered_divestats$all.dur  #hunting to transit ratio

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



filtered_divestats$max.d[filtered_divestats$hunt_dive=="transit"] %>% hist()
filtered_divestats$max.d[filtered_divestats$hunt_dive=="transit"] %>% length()











# Practice ----------------------------------------------------------------



##############################################################################################################################
####################### Reading in seal tracks produced by Mia. This will be useful for horizontal   ###############
#######################         dives and averaging the dives that are > 60sec long.                           ###############
##############################################################################################################################
## Import tracks analysed by Mia
tracks <- read.csv("Marion_FStracks_SSMresults_2009W_2015S.csv",sep = ";")
# str(tracks)

loc1 <-droplevels(subset(tracks,tracks$id == sealID)) %>% filter(gmt > first(df_init_tmp2$gmt) & gmt < last(df_init_tmp2$gmt))
loc1$gmt <- strptime(loc1$gmt,format = "%Y-%m-%d %H:%M", tz = "GMT")

diff(loc1$gmt)
which(diff(loc1$gmt)>3) # where there is missing data





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

##########################  Group dives closest to locations and perform summary stats ##########################

## get mean values for each variables 4 hours either side of each location
## the code below show how to it for bottom time. You need to repeat this for the other variables


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


