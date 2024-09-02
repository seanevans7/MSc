library(argosfilter)
library(tidyverse)
library(viridis)
# # MIXED PICKLE (mm042):
# MP <- droplevels(subset(dives, site == "MM042"))
# levels(MP$id)
# 
# lat.mi<- -46.87 #this is the lattitude of original point or MP beach where pup is at 
# lon.mi<- 37.65  # this is the longitude of original point or MP beach where pup is
# i <- {}
# MP$bear.mi.deg <- {}
# MP$dist.mi <- {}
# MP$MI <-{}
# MP$speed <-{}
# for (i in 1:nrow(MP)){
#   MP$bear.mi.deg[i] <- bearing(lat.mi,MP[i,"latm"],lon.mi,MP[i,"lonm"]) #Column 8 is LAT values and 5 the LON values.
#   MP$dist.mi[i] <- distance(lat.mi,MP[i,"latm"],lon.mi,MP[i,"lonm"]) 
#   # MP$MI[i]<- ifelse(MP$dist.mi[i]<=5,"1","0") #I am just marking all the points that are within 10kms
#   #of Marion Island as being on the island ("Y") and all the points that are more than 10kms away as not on Marion - "N".
#   # MP$speed[i] <- ifelse(MP$MI[i]=="1",0,(MP$step.dist[i]*1000)/9000)
#   #If animal on Marion speed = o else, if not on Marion then
#   #Calculate speed in m/s by multiplying distance between current point and prev points
#   #with 1000 (to get to m) and dividing by time difference in seconds.
#   print(i)
# }
# head(MP)
# str(MP)
# # MP$MI <- as.factor(MP$MI)
# # qplot(MP$lon, MP$lat, col=MP$speed)
# # hist(MP$speed)
# # qplot(MP$dist.mi, MP$speed)
# qplot(MP$lonm, MP$latm, col=MP$dist.mi)
# 
# # Van den Boogaard (MM067)):
# vandenB <- droplevels(subset(dives, site == "MM067"))
# levels(vandenB$id)
# 
# lat.mi<- -46.87 #this is the lattitude of original point or MP beach where pup is at 
# lon.mi<- 37.86  # this is the longitude of original point or MP beach where pup is
# i <- {}
# vandenB$bear.mi.deg <- {}
# vandenB$dist.mi <- {}
# vandenB$MI <-{}
# vandenB$speed <-{}
# nrow(vandenB)
# for (i in 1:nrow(vandenB)){
#   vandenB$bear.mi.deg[i] <- bearing(lat.mi,vandenB[i,"latm"],lon.mi,vandenB[i,"lonm"]) #Column 8 is LAT values and 5 the LON values.
#   vandenB$dist.mi[i] <- distance(lat.mi,vandenB[i,"latm"],lon.mi,vandenB[i,"lonm"]) 
#   # vandenB$MI[i]<- ifelse(vandenB$dist.mi[i]<=5,"1","0") #I am just marking all the points that are within 10kms
#   #of Marion Island as being on the island ("Y") and all the points that are more than 10kms away as not on Marion - "N".
#   # vandenB$speed[i] <- ifelse(vandenB$MI[i]=="1",0,(vandenB$step.dist[i]*1000)/9000)
#   #If animal on Marion speed = o else, if not on Marion then
#   #Calculate speed in m/s by multiplying distance between current point and prev points
#   #with 1000 (to get to m) and dividing by time difference in seconds.
#   print(i)
# }



# Sean_dist_dir_fun -------------------------------------------------------

lat.VDB<- -46.87 #this is the lattitude of original point or MP beach where pup is at 
lon.VDB<- 37.86 
lat.MP<--46.87
lon.MP<-37.65

VDB <- droplevels(subset(dives, sealID %in% c(1,2,4,5,13,14,15,16,18,20,21,22,23,25,31,17,19,24,26,27,28,109,110,112)))
MP <- droplevels(subset(dives, !(sealID %in% c(1,2,4,5,13,14,15,16,18,20,21,22,23,25,31,17,19,24,26,27,28,109,110,112))))

# VDB <- dives[which(dives$sealID %in% c(1,2,4,5,13,14,15,16,18,20,21,22,23,25,31,17,19,24,26,27,28,109,110,112)),]

# (VDB %>% group_by(sealID) %>% count(season))$season %>% table()
# (MP %>% group_by(sealID) %>% count(season))$season %>% table()

## Can use a loop, but using mutate is much faster
# i <- {}
# VDB$bear.VDB.deg <- {}
# VDB$dist.VDB <- {}
# VDB$MI <-{}
# VDB$speed <-{}
# for (i in 1:nrow(VDB)){
#   VDB$bear.VDB.deg[i] <- bearing(lat.VDB,VDB[i,"lat"],lon.VDB,VDB[i,"lon"]) #Column 8 is LAT values and 5 the LON values.
#   # VDB$dist.VDB[i] <- distance(lat.VDB,VDB[i,"lat"],lon.VDB,VDB[i,"lon"]) 
#   # MP$MI[i]<- ifelse(MP$dist.mi[i]<=5,"1","0") #I am just marking all the points that are within 10kms
#   #of Marion Island as being on the island ("Y") and all the points that are more than 10kms away as not on Marion - "N".
#   # MP$speed[i] <- ifelse(MP$MI[i]=="1",0,(MP$step.dist[i]*1000)/9000)
#   #If animal on Marion speed = o else, if not on Marion then
#   #Calculate speed in m/s by multiplying distance between current point and prev points
#   #with 1000 (to get to m) and dividing by time difference in seconds.
#   print(i)
# }

## Rather use these distances ad the previous distance values were based on Marion as a whole and not a distance from the colony itself
VDB <- VDB %>% mutate('bear.deg' = bearing(lat.VDB,lat,lon.VDB,lon), 'dist' = distance(lat.VDB,lat,lon.VDB,lon), colony = 'VDB')
head(VDB)
str(VDB)

MP <- MP %>% mutate('bear.deg' = bearing(lat.MP,lat,lon.MP,lon), 'dist' = distance(lat.VDB,lat,lon.VDB,lon), colony = 'MP')
head(MP)
str(MP)

dives <- rbind(MP,VDB)
dives$colony <- as.factor(dives$colony)



# Updated (20201/01/27) Plotting of mean direction of locations per colony --------
#### Using Locations

locs_tmp <- locs
filter<-final_df %>% group_by(sealID,ft) %>% count() %>% dplyr::select(sealID,ft) %>% ungroup()
locs_tmp<-right_join(locs_tmp,filter,by=c('sealID','ft')) # includes all rows in filter
locs_tmp<-full_join(locs_tmp,Metadata %>% dplyr::select(sealID,Site))

lat.VDB<- -46.87 #this is the lattitude of original point or MP beach where pup is at 
lon.VDB<- 37.86 
lat.MP<- -46.87
lon.MP<- 37.65

locs_tmp_VDB <- locs_tmp %>% filter(Site=='MM067') %>% mutate('bear.deg' = bearing(lat.VDB,lat,lon.VDB,lon), 'dist' = distance(lat.VDB,lat,lon.VDB,lon))
locs_tmp_VDB <- locs_tmp_VDB %>% filter(dist>5)
locs_tmp_MP <- locs_tmp %>% filter(Site=='MM042') %>% mutate('bear.deg' = bearing(lat.MP,lat,lon.MP,lon), 'dist' = distance(lat.MP,lat,lon.MP,lon))
locs_tmp_MP <- locs_tmp_MP %>% filter(dist>5)
locs_tmp<-full_join(locs_tmp_VDB,locs_tmp_MP)

# Make the plot
ggplot(locs_tmp) +
  # Add the stacked bar
  geom_bar(aes(x=Site, y=bear.deg, fill=Site), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE)+
  coord_polar()

# ggplot(dives, aes(bear.deg))+
#   geom_histogram(fill='blue',color='black', bins=30)+
#   coord_polar()+
#   facet_wrap(~season)+
#   ggtitle('Direction')

ft_locs <- locs_tmp %>% group_by(sealID,Site,season,ft) %>% summarise(direction=mean(bear.deg))
ggplot(ft_locs, aes(direction, fill=season))+
  geom_histogram(color='black',bins=60)+
  coord_polar(start = 0)+
  facet_wrap(~Site)+
  ggtitle('Direction')

rm(ft_locs,locs_tmp_VDB,locs_tmp_MP,locs_tmp)

### Using Dives
