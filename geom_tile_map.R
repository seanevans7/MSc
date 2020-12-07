#### Maps #####
library(ncdf4)
library(cmocean)
library(grid)
library(ncmeta)
library(tidync)
library(tidyr)
#install.packages("ggOceanMaps", repos = c("https://mikkovihtakari.github.io/drat", "https://cloud.r-project.org"))
library(ggOceanMaps)
library(imager)
# library('rgeos')
# Add the Orsifronts
# library(orsifronts)
# myOrsi <- fortify(orsifronts)
# myOrsi <- myOrsi[myOrsi$id %in% c("stf", "saf", "pf"), ]

# Get Southern Ocean fronts from Park & Durand 2019
# https://doi.org/10.17882/59800

frnts <- nc_open("62985.nc")

STF <- data.frame(
  "lat" = ncvar_get(frnts, "LatNB"),
  "lon" = ncvar_get(frnts, "LonNB"),
  "name" = "STF"
)
SAF <- data.frame(
  "lat" = ncvar_get(frnts, "LatSAF"),
  "lon" = ncvar_get(frnts, "LonSAF"),
  "name" = "SAF"
)
PF <- data.frame(
  "lat" = ncvar_get(frnts, "LatPF"),
  "lon" = ncvar_get(frnts, "LonPF"),
  "name" = "PF"
)
# SACCF <- data.frame(
#   "lat" = ncvar_get(frnts, "LatSACCF"),
#   "lon" = ncvar_get(frnts, "LonSACCF"),
#   "name" = "SACCF"
# )
# SB <- data.frame(
#   "lat" = ncvar_get(frnts, "LatSB"),
#   "lon" = ncvar_get(frnts, "LonSB"),
#   "name" = "SB"
# )
nc_close(frnts)

frnts <- rbind(STF,SAF,PF)

frnts <- na.omit(frnts)
frnts2 <- droplevels(subset(frnts,frnts$lon >= 20 & frnts$lon <= 55))
rm(PF,SAF,STF,frnts)

# Antarctica base map of just Weddell Sea
# data("wrld_simpl", package = "maptools")    
# ant <- crop(wrld_simpl, extent(-35, 35, -90, -45)) 

# Bathymetry base map of Marion
(y_lines <- seq(-75,-45, by = 10))
(x_lines <- c(-35, -15,0,15,35))

# readtopo(topo = c("gebco_19"), polar = FALSE, lon180 = TRUE, xylim = NULL,returnfiles = FALSE)

### Marion Island
sf_Marion <- data.frame("lon" = 37.746368, "lat" = -46.893361) %>% 
  st_as_sf(coords = c("lon", "lat"),crs=4326,remove = FALSE)


### Bathymetry Gebco_20
bathy_file = tidync('gebco_2020_n-20.478515625_s-65.33789277076721_w6.372072458267214_e72.9580078125.nc')
bathy_file = tidync('GRIDONE_2D.nc')

bathy_summer <- bathy_file %>% hyper_filter(lon = between(lon, 29, 46), 
                             lat = between(lat, -52, -45)) #%>% hyper_tibble()
bathy_winter <- bathy_file %>% hyper_filter(lon = between(lon, 15, 55), 
                            lat = between(lat, -55, -39)) %>% hyper_tibble()
ggplot() +
  geom_tile(data=bathy_summer,aes(lon, lat, fill = elevation))+
  geom_point(aes(x = lon , y = lat),data = locs[locs$season=='summer',])

ggplot() +
  geom_raster(data=bathy_winter,aes(lon, lat, fill = elevation))+
  geom_point(aes(x = lon , y = lat),data = locs[locs$season=='winter',])

bathy_file = raster('gebco_2020_n-20.478515625_s-65.33789277076721_w6.372072458267214_e72.9580078125.nc')
bathy_file = raster('GRIDONE_2D.nc')
############ Summer ###########
####### locs ########
PF<-frnts2 %>% filter((name=='PF') & (lon>32) & (lon<46) & (lat>-52) & (lat<-45))
SAF<-frnts2 %>% filter((name=='SAF') & (lon>32) & (lon<46) & (lat>-52) & (lat<-45))

ggplot() +
  # geom_raster(data=bathy,aes(lon, lat, fill = elevation))+
  # geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
  # coord_map("azequidistant", orientation = c(-90, 0, 0)) +
  geom_tile(data = locs[locs$season=='summer' & locs$year==2010,], aes(lon, lat, z = strat_prop),binwidth = 0.08, stat = "summary_2d", fun = mean,na.rm = TRUE)+
  scale_fill_gradientn(name = "% Stratification",colours = c("grey60", "blue", "orange"), values = c(0,0.1,1))+
  # geom_point(data = frnts2 %>% filter(name=='PF' | name=='SAF'), aes(x = lon, y = lat, colour = name),size=0.9)+ #, linetype = 1, name=='STF' | 
  geom_point(data = SAF, aes(x = lon, y = lat),size=0.9)+
  geom_point(data = PF, aes(x = lon, y = lat),size=0.9)+
  # geom_text(data = frnts2 %>% filter(name=='PF' | name=='SAF'), aes(label = name, colour = name, x = Inf, y=lat))+
  geom_text(data = SAF[which.max(SAF[,"lon"]),], aes(label = name, x = Inf, y=lat),colour='black', hjust = +2, vjust = -1.5, size=6)+
  geom_text(data = PF[which.max(PF[,"lon"]),], aes(label = name, x = Inf, y=lat),colour='black', hjust = +2, vjust = -1.5, size=6)+
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='green', data = sf_Marion)+
  theme_bw()   +
  #facet_grid(diel_phase~season)+
  scale_x_continuous(name="lon", limits=c(32, 46)) + #summer - name="lon", limits=c(30, 50)
  scale_y_continuous(name="lat", limits=c(-52, -45)) + #summer - name="lat", limits=c(-55, -40)
  xlab("") + 
  ylab("")  +
  # labs(fill = "Mean diving \nduration (min)")+
  # scale_fill_viridis_b()+
  ggtitle('Thermocline presence') +
  theme(plot.title = element_text(size=22,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        legend.title = element_text(face="bold",size=18),legend.text = element_text(size=14),legend.key.size = unit(1.0, "cm"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

####### Dives strat_prop ######
summ = c(2011,2012,2013,2014,2015)
wint = c(2009,2011,2012,2013,2014)
for (i in 1:length(summ)) {
y=summ[i]
n = dives_pdsi %>% filter(season=='summer' & year==y) %>% NROW()
nT = dives_pdsi %>% filter(season=='summer' & year==y & Thermocline=='present') %>% NROW()
nw = dives_pdsi %>% filter(season=='summer' & year==y & Thermocline=='present' & lon<37.746368) %>% NROW()
nwll = tibble('lon'=34.5,'lat'=-45,'n'=paste0('nWesT = ',as.character(nw),' (',as.character(round(nw/(ne+nw),digits=4)*100),'%)'))
ne = dives_pdsi %>% filter(season=='summer' & year==y & Thermocline=='present' & lon>=37.746368) %>% NROW()
nell = tibble('lon'=42.5,'lat'=-45,'n'=paste0('nEast = ',as.character(ne),' (',as.character(round(ne/(ne+nw),digits=4)*100),'%)'))


ggplot() +
  # geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
  # coord_map("azequidistant", orientation = c(-90, 0, 0)) +
  geom_tile(data = locs[locs$season=='summer' & locs$year==y,], aes(lon, lat, z = strat_prop),binwidth = 0.1, stat = "summary_2d", fun = mean,na.rm = TRUE)+
  geom_tile(data = dives_pdsi %>% filter(season=='summer' & year==y), aes(lon, lat, z = therm_depth),binwidth = 0.1, stat = "summary_2d", fun = mean)+
  #geom_raster(data=bathy_winter,aes(lon, lat, fill = elevation))+
  
  #geom_point(aes(x = lon , y = lat),data = locs[locs$season=='summer',])+
  scale_fill_gradientn(name = "T dep [m]",colours = c("grey60", "blue", "orange"), values = c('NA',0.1,1), limits = c(1,150))+
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='green', data = sf_Marion)+
  geom_point(data = SAF, aes(x = lon, y = lat),size=0.9)+
  geom_point(data = PF, aes(x = lon, y = lat),size=0.9)+
  geom_text(data = SAF[which.max(SAF[,"lon"]),], aes(label = name, x = Inf, y=lat),colour='black', hjust = +1, vjust = -1, size=6)+
  geom_text(data = PF[which.max(PF[,"lon"]),], aes(label = name, x = Inf, y=lat),colour='black', hjust = +1, vjust = -1, size=6)+
  geom_text(data = nwll, aes(label = n, x = lon, y=lat),colour='black', size=4)+
  geom_text(data = nell, aes(label = n, x = lon, y=lat),colour='black', size=4)+
  theme_bw()   +
  #facet_grid(diel_phase~season)+
  scale_x_continuous(name="lon", limits=c(32, 46)) + #summer - name="lon", limits=c(30, 50)
  scale_y_continuous(name="lat", limits=c(-52, -45)) + #summer - name="lat", limits=c(-55, -40)
  xlab("") + 
  ylab("")  +
  # labs(fill = "Mean diving \nduration (min)")+
  ggtitle(paste0('Thermocline depth [m]  n=',as.character(nT),'/',as.character(n),'(',as.character(round(nT/(n+nT),digits=4)*100),'%)',' dives (',as.character(y),')')) +
  theme(plot.title = element_text(size=22,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        legend.title = element_text(face="bold",size=18),legend.text = element_text(size=14),legend.key.size = unit(1.0, "cm"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

ggsave(paste0('C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc/Plots & Dive Tables/All seals/Exploratory analysis/Tile mapping/Summer/New therm threshold (0.25)/Therm_dep_',as.character(y),'.png'), width = 24, height = 15, units = "cm")
}

####### Dives max.d ######

ggplot() +
  # geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
  # coord_map("azequidistant", orientation = c(-90, 0, 0)) +
  geom_tile(data = summer_dives, aes(lon, lat, z = max.d),binwidth = 0.1, stat = "summary_2d", fun = mean,na.rm = TRUE)+
  scale_fill_gradientn(name = "max depth [m]",colours = c("grey60", "blue", "orange"), values = c(0,0.1,1))+
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='green', data = sf_Marion)+
  geom_point(data = SAF, aes(x = lon, y = lat),size=0.9)+
  geom_point(data = PF, aes(x = lon, y = lat),size=0.9)+
  geom_text(data = SAF[which.max(SAF[,"lon"]),], aes(label = name, x = Inf, y=lat),colour='black', hjust = +1, vjust = -1, size=6)+
  geom_text(data = PF[which.max(PF[,"lon"]),], aes(label = name, x = Inf, y=lat),colour='black', hjust = +1, vjust = -1, size=6)+
  theme_bw()   +
  #facet_grid(diel_phase~season)+
  scale_x_continuous(name="lon", limits=c(32, 46)) + #summer - name="lon", limits=c(30, 50)
  scale_y_continuous(name="lat", limits=c(-52, -45)) + #summer - name="lat", limits=c(-55, -40)
  xlab("") + 
  ylab("")  +
  # labs(fill = "Mean diving \nduration (min)")+
  ggtitle('Maximum depth [m]') +
  theme(plot.title = element_text(size=22,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        legend.title = element_text(face="bold",size=18),legend.text = element_text(size=14),legend.key.size = unit(1.0, "cm"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

####### Dives divetype ######

ggplot() +
  # geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
  # coord_map("azequidistant", orientation = c(-90, 0, 0)) +
  geom_tile(data = summer_dives, aes(lon, lat, fill = divetype),binwidth = 0.1)+
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='green', data = sf_Marion)+
  geom_point(data = SAF, aes(x = lon, y = lat),size=0.9)+
  geom_point(data = PF, aes(x = lon, y = lat),size=0.9)+
  geom_text(data = SAF[which.max(SAF[,"lon"]),], aes(label = name, x = Inf, y=lat),colour='black', hjust = +1, vjust = -1, size=6)+
  geom_text(data = PF[which.max(PF[,"lon"]),], aes(label = name, x = Inf, y=lat),colour='black', hjust = +1, vjust = -1, size=6)+
  theme_bw()   +
  #facet_grid(diel_phase~season)+
  scale_x_discrete(name="lon", limits=c(32, 46)) + #summer - name="lon", limits=c(30, 50)
  scale_y_discrete(name="lat", limits=c(-52, -45)) + #summer - name="lat", limits=c(-55, -40)
  xlab("") + 
  ylab("")  +
  # labs(fill = "Mean diving \nduration (min)")+
  ggtitle('Maximum depth [m]') +
  theme(plot.title = element_text(size=22,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        legend.title = element_text(face="bold",size=18),legend.text = element_text(size=14),legend.key.size = unit(1.0, "cm"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))





######## Looking at dives West and East of the Island in summer
# East Thermocline depths
summer_dives[summer_dives$lon>37.746368 & !is.na(summer_dives$therm_depth),]$therm_depth %>% summary() 
summer_dives[summer_dives$lon>37.746368 & !is.na(summer_dives$therm_depth),]$therm_depth %>% sd()
summer_dives[summer_dives$lon>37.746368 & !is.na(summer_dives$therm_depth),]$therm_depth %>% NROW()
summer_dives[summer_dives$lon>37.746368,] %>% group_by(Thermocline) %>% count()
# East max.d for Thermocline dives
summer_dives[summer_dives$lon>37.746368 & !is.na(summer_dives$therm_depth),]$max.d %>% summary() 
summer_dives[summer_dives$lon>37.746368 & !is.na(summer_dives$therm_depth),]$max.d %>% sd()
summer_dives[summer_dives$lon>37.746368 & !is.na(summer_dives$therm_depth),]$max.d %>% NROW()
# East Thermocline absent dives 
summer_dives[summer_dives$lon>37.746368 & is.na(summer_dives$therm_depth),]$max.d %>% summary() 
summer_dives[summer_dives$lon>37.746368 & is.na(summer_dives$therm_depth),]$max.d %>% sd()
summer_dives[summer_dives$lon>37.746368 & is.na(summer_dives$therm_depth),]$max.d %>% NROW()
# East all dives
summer_dives[summer_dives$lon>37.746368,]$max.d %>% summary() 
summer_dives[summer_dives$lon>37.746368,]$max.d %>% sd()
summer_dives[summer_dives$lon>37.746368,]$max.d %>% NROW()

# West Thermocline depths
summer_dives[summer_dives$lon<37.746368 & !is.na(summer_dives$therm_depth),]$therm_depth %>% summary() 
summer_dives[summer_dives$lon<37.746368 & !is.na(summer_dives$therm_depth),]$therm_depth %>% sd()
summer_dives[summer_dives$lon<37.746368 & !is.na(summer_dives$therm_depth),]$therm_depth %>% NROW()
summer_dives[summer_dives$lon<37.746368,] %>% group_by(Thermocline) %>% count()
# West max.d for Thermocline dives
summer_dives[summer_dives$lon<37.746368 & !is.na(summer_dives$therm_depth),]$max.d %>% summary() 
summer_dives[summer_dives$lon<37.746368 & !is.na(summer_dives$therm_depth),]$max.d %>% sd()
summer_dives[summer_dives$lon<37.746368 & !is.na(summer_dives$therm_depth),]$max.d %>% NROW()
# West max.d absent dives 
summer_dives[summer_dives$lon<37.746368 & is.na(summer_dives$therm_depth),]$max.d %>% summary() 
summer_dives[summer_dives$lon<37.746368 & is.na(summer_dives$therm_depth),]$max.d %>% sd()
summer_dives[summer_dives$lon<37.746368 & is.na(summer_dives$therm_depth),]$max.d %>% NROW()
# West all dives
summer_dives[summer_dives$lon<37.746368,]$max.d %>% summary() 
summer_dives[summer_dives$lon<37.746368,]$max.d %>% sd()
summer_dives[summer_dives$lon<37.746368,]$max.d %>% NROW()
 

summer_dives[summer_dives$lon>37.746368,] %>% group_by(sealID) %>% count()
summer_dives[summer_dives$lon<37.746368,] %>% group_by(sealID) %>% count()

West_summer = c(52,53,57,58,63,64,65,66,67,109,113)
East_summer = c(13,14,15,16,20,21,22,23,110,112)

(summer_dives %>% filter(sealID %in% West_summer & !is.na(summer_dives$therm_depth)))$therm_depth %>% summary()
(summer_dives %>% filter(sealID %in% West_summer & !is.na(summer_dives$therm_depth)))$therm_depth %>% sd()
(summer_dives %>% filter(sealID %in% West_summer & !is.na(summer_dives$therm_depth)))$therm_depth %>% NROW()
(summer_dives %>% filter(sealID %in% West_summer & !is.na(summer_dives$therm_depth)))$therm_depth %>% hist()

(summer_dives %>% filter(sealID %in% West_summer))$max.d %>% summary()
(summer_dives %>% filter(sealID %in% West_summer))$max.d %>% sd()
(summer_dives %>% filter(sealID %in% West_summer))$max.d%>% hist()

(summer_dives %>% filter(sealID %in% West_summer & !is.na(summer_dives$therm_depth)))$therm_depth %>% NROW()
(summer_dives %>% filter(sealID %in% West_summer & !is.na(summer_dives$therm_depth)))$therm_depth %>% hist()



(summer_dives %>% filter(sealID %in% East_summer & !is.na(summer_dives$therm_depth)))$therm_depth %>% summary()
(summer_dives %>% filter(sealID %in% East_summer & !is.na(summer_dives$therm_depth)))$therm_depth %>% sd()
(summer_dives %>% filter(sealID %in% East_summer & !is.na(summer_dives$therm_depth)))$therm_depth %>% NROW()
(summer_dives %>% filter(sealID %in% East_summer & !is.na(summer_dives$therm_depth)))$therm_depth %>% hist()

(summer_dives %>% filter(sealID %in% East_summer & !is.na(summer_dives$therm_depth)))$max.d %>% summary()
(summer_dives %>% filter(sealID %in% East_summer & !is.na(summer_dives$therm_depth)))$max.d %>% sd()
(summer_dives %>% filter(sealID %in% East_summer & !is.na(summer_dives$therm_depth)))$max.d%>% hist()

(summer_dives %>% filter(sealID %in% East_summer))$max.d %>% NROW()


# Adds labels
  # geom_text(aes(x = 30, y = seq(-52, -42.5, by = 2), hjust = -0.2,
              # label = rev(paste0(seq(42.5, 52, by = 2), "°S")))) #+
  # geom_text(aes(x = x_lines, y = -42, label = c("35°W","15°W", "0°", "15°E","35°E"))) #+
# # Adds Y axes
# # geom_segment(aes(y = y_lines, yend = y_lines, x = -35, xend = 35),
#              linetype = "dashed", colour = "lightgrey") +
# # # Add X axes
# # geom_segment(aes(y = -45 , yend = -80, x = x_lines, xend = x_lines),
#              linetype = "dashed", colour = "lightgrey") +
# # theme(panel.background = element_blank(),
#       panel.grid.major = element_blank(),
#       axis.ticks=element_blank(),
#       # legend.title = element_text("Mean diving \nduration (min)"),
#       legend.text = element_text(size=10),
#       legend.position = c(.1,.25)))
# 

############ Winter ###########

####### locs ########

ggplot() +
  # geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
  # coord_map("azequidistant", orientation = c(-90, 0, 0)) +
  geom_tile(data = locs[locs$season=='winter' & locs$strat_prop>0,], aes(lon, lat, z = strat_prop),binwidth = 0.1, stat = "summary_2d", fun = mean,na.rm = TRUE)+
  geom_point(data = frnts2 %>% filter(name=='PF' | name=='SAF'), aes(x = lon, y = lat),size=0.5, colour = "grey60")+ #, linetype = 1, name=='STF' | 
  geom_point(aes(x = lon , y = lat),pch = 17,size=2.5, color='red', data = sf_Marion)+
  theme_bw()   +
  #facet_grid(diel_phase~season)+
  scale_x_continuous(name="lon", limits=c(20, 50)) + #summer - name="lon", limits=c(30, 50)
  scale_y_continuous(name="lat", limits=c(-52, -40)) + #summer - name="lat", limits=c(-55, -40)
  xlab("") + 
  ylab("")  +
  # labs(fill = "Mean diving \nduration (min)")+
  scale_fill_viridis_b()+
  ggtitle('Thermocline presence')

####### winter dives ########
ggplot() +
  # geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
  # coord_map("azequidistant", orientation = c(-90, 0, 0)) +
  geom_tile(data = winter_dives, aes(lon, lat, z = mean_Temp),binwidth = 0.1, stat = "summary_2d", fun = mean,na.rm = TRUE)+
  geom_point(data = frnts2 %>% filter(name=='PF' | name=='SAF'), aes(x = lon, y = lat),size=0.5, colour = "grey60")+ #, linetype = 1, name=='STF' | 
  geom_point(aes(x = lon , y = lat),pch = 17,size=2.5, color='red', data = sf_Marion)+
  theme_bw()   +
  #facet_grid(diel_phase~season)+
  scale_x_continuous(name="lon", limits=c(20, 50)) + #summer - name="lon", limits=c(30, 50)
  scale_y_continuous(name="lat", limits=c(-52, -40)) + #summer - name="lat", limits=c(-55, -40)
  xlab("") + 
  ylab("")  +
  # labs(fill = "Mean diving \nduration (min)")+
  scale_fill_viridis_b()+
  ggtitle('mean_Temp')











# Practice plot using ggOceanMaps -----------------------------------------

dt <- data.frame(lon = c(32, 46), lat = c(-52,-45))


basemap(data=dt,bathymetry = TRUE) +
  geom_tile(data = locs[locs$season=='summer',], aes(lon, lat, z = strat_prop),binwidth = 0.08, stat = "summary_2d", fun = mean,na.rm = TRUE)+
  geom_tile(data = dives %>% filter(season=='summer'), aes(lon, lat, z = therm_depth),binwidth = 0.1, stat = "summary_2d", fun = mean)+
  #geom_point(aes(x = lon , y = lat),data = locs[locs$season=='summer',])+
  scale_fill_gradientn(name = "T dep [m]",colours = c("grey60", "blue", "orange"), values = c('NA',0.1,1))+
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='green', data = sf_Marion)+
  geom_point(data = SAF, aes(x = lon, y = lat),size=0.9)+
  geom_point(data = PF, aes(x = lon, y = lat),size=0.9)+
  geom_text(data = SAF[which.max(SAF[,"lon"]),], aes(label = name, x = Inf, y=lat),colour='black', hjust = +1, vjust = -1, size=6)+
  geom_text(data = PF[which.max(PF[,"lon"]),], aes(label = name, x = Inf, y=lat),colour='black', hjust = +1, vjust = -1, size=6)+
  theme_bw()   +
  #facet_grid(diel_phase~season)+
  #scale_x_continuous(name="lon", limits=c(32, 46)) + #summer - name="lon", limits=c(30, 50)
  #scale_y_continuous(name="lat", limits=c(-52, -45)) + #summer - name="lat", limits=c(-55, -40)
  xlab("") + 
  ylab("")  +
  # labs(fill = "Mean diving \nduration (min)")+
  ggtitle('Thermocline depth [m]') +
  theme(plot.title = element_text(size=22,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        legend.title = element_text(face="bold",size=18),legend.text = element_text(size=14),legend.key.size = unit(1.0, "cm"),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))


basemap(data=dt,bathymetry = TRUE)+
  geom_point(aes(x = lon , y = lat),data = locs[locs$season=='summer',], shape=1)
