#### Maps #####

library(ncdf4)
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
frnts2 <- droplevels(subset(frnts,frnts$lon >= 25 & frnts$lon <= 55))
rm(PF,SAF,STF,frnts)

# Antarctica base map of just Weddell Sea
data("wrld_simpl", package = "maptools")    
ant <- crop(wrld_simpl, extent(-35, 35, -90, -45)) 

(y_lines <- seq(-75,-45, by = 10))
(x_lines <- c(-35, -15,0,15,35))

sf_Marion <- data.frame("lon" = 37.746368, "lat" = -46.893361) %>% 
  st_as_sf(coords = c("lon", "lat"),crs=4326,remove = FALSE)

ggplot() +
  # geom_polygon(data = ant, aes(x=long, y = lat, group = group),fill="grey", alpha = 0.8)+
  # coord_map("azequidistant", orientation = c(-90, 0, 0)) +
  geom_tile(data = dives_sr, aes(lon, lat, z = ht_rat),binwidth = 0.25, stat = "summary_2d", fun = mean,na.rm = TRUE)+
  geom_point(data = frnts2 %>% filter(name=='STF' | name=='PF'), aes(x = lon, y = lat),size=1, colour = "grey60")+ #, linetype = 1
  geom_point(aes(x = lon , y = lat),pch = 17,size=2, color='red', data = sf_Marion)+
  theme_bw()   +
  facet_grid(diel_phase~season)+
  scale_x_continuous(breaks = NULL) +
  xlab("") + 
  ylab("")  +
  # labs(fill = "Mean diving \nduration (min)")+
  scale_fill_viridis_b()+
  ggtitle('ht_rat')

# Adds labels
# # geom_text(aes(x = -40, y = seq(-75, -55, by = 10), hjust = -0.2,
#               label = rev(paste0(seq(55, 75, by = 10), "°S")))) +
# # geom_text(aes(x = x_lines, y = -42, label = c("35°W","15°W", "0°", "15°E","35°E"))) +
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


