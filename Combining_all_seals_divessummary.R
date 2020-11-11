##################### Sean_fseal_VARS.R: Is a file containg code to process fur seal diving data. ############################

############################################# Creating large dataframe #######################################################
#############################################      By Sean Evans       #######################################################


# Load packages -----------------------------------------------------------
library(purrr)
library(broom)
library(move)
#library(moveVis)
library(argosfilter)
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
library(ggstatsplot)
# library(ncdf4)
library(raster)
library(magrittr)
library(dplyr)
library(rgl)
# library(lme4)
# library(lmerTest)

#require(GGally)
#require(reshape2)
#require(compiler)
#require(parallel)
#require(boot)
rm(list = ls())

#SealIDS = c(1,2,4,5,seq(13,18,1),seq(20,25,1),31,49,50,51,52,57,63,65,68,69,110,111,112,113)
SealIDS = c(1,2,4,5,seq(13,25,1),31,47,seq(49,53,1),57,58,seq(62,70,1),109,110,112,113)

for (i in 1:length(SealIDS)) {
  print(i)
  sealID = SealIDS[i]
  save_filtered_divestats = paste("Plots & Dive Tables/Seal",sealID, sep = "")
  filtered_divestats <- read_rds(file.path(save_filtered_divestats,paste(sealID,"_filtered_divestats.rds",sep = '')))
  
  if (i==1) {
    dives = filtered_divestats
  }
  
  else{
    dives = rbind(dives,filtered_divestats)
  }
}


for (i in 1:length(SealIDS)) {
  print(i)
  sealID = SealIDS[i]
  save_loc = paste("Plots & Dive Tables/Seal",sealID, sep = "")
  loc1 <- read_rds(file.path(save_loc,paste(sealID,"_loc.rds",sep = '')))
  
  if (i==1) {
    locs = loc1
  }
  
  else{
    locs = rbind(locs,loc1)
  }
}

###### Fixing Thermocline column
# str(dives$Thermocline)
dives$Thermocline <- as.factor(dives$Thermocline)
dives[(which(dives$Thermocline == "")),'Thermocline'] <- as.factor('absent')
dives$Thermocline <- droplevels(dives$Thermocline)
table(dives$Thermocline,dives$diel_phase)

# Saving data
save_all = 'Plots & Dive Tables/'
saveRDS(dives,file.path(save_all,"dives.rds"))
saveRDS(dives_sr,file.path(save_all,"dives_sr.rds"))
write.csv(dives_sr,paste0(save_all,"dives_sr.csv"))
saveRDS(locs,file.path(save_all,"locs.rds"))

# Reading data
dives = readRDS(file.path(save_all,"dives.rds"))
locs = readRDS(file.path(save_all,"locs.rds"))

################# Reduce dives to those without nan values for ft and bout   #################
dives <- dives[!is.na(dives$ft) | !is.na(dives$bout),] # or dives <- dives[which(!is.na(dives$ft) | !is.na(dives$bout))]
# Omit dives that do not fit into within 24hours of the foraging trip interval time limits

###### add season column to dives using JDay ############
dives$season = NaN
dives[which(dives$JDay>92&dives$JDay<275),]$season = 'winter' # winter (Apr-Sep)
dives[which(dives$JDay<=92|dives$JDay>=275),]$season = 'summer' # summer (Oct-Mar)

# Dive  residuals ----------------------------------------

### Dive residuals ### Essentially the measure of diving effort
# Shorter or longer dive durations for a given depth
# Specify the error distribution and link function based on the distribution of the response variable (not normal - Binary/Count data) and if the errors are skewed
# Does my data fit a Poisson or normal distribution?
# mean(dives$all.dur) > sqrt(var(dives$all.dur)) # Therefore the data is not overdispersed (If it is, use quasi_ dist)

#Using glm because the data (all.dur) is not normally distributed, it may fit a Poisson distribution better
Dive_residual.null = glm(all.dur ~ max.d + (1|sealID) + (1|ft) + (1|bout), family = poisson(link = "log"), data=dives) # 8448
summary(Dive_residual.null)
# The summary will say: 'Dispersion parameter for poisson family taken to be 1'

# Test for overdispersion
Dive_residual.null = glm(all.dur ~ max.d + (1|sealID) + (1|ft) + (1|bout), family = quasipoisson(link = "log"), data=dives) # 
summary(Dive_residual.null) # Dispersion parameter = 6.233905

## Reduced or null models ## - REML FALSE when comparing models with different fixed effects. Make TRUE for final model.
##  Models do not have comparable likelihoods when fitted using REML.
Dive_residual.m1 = lmer(all.dur ~ max.d + (1|sealID/ft/bout), data=dives, REML = FALSE)
Dive_residual.m1 = lmer(all.dur ~ max.d + (1|sealID:ft:bout), data=dives, REML = FALSE) # What is the difference between ':' and '/'
plot(resid(Dive_residual.m1),ylim = c(-200,300))
Dive_residual.m2 = lmer(all.dur ~ max.d + (1|sealID) + (1|ft) + (1|bout), data=dives, REML = FALSE) 
Dive_residual.m3 = lmer(all.dur ~ max.d + (1|sealID) + (1|ft), data=dives, REML = FALSE) 
Dive_residual.m4 = lmer(all.dur ~ max.d + (1|sealID) + (1|bout), data=dives, REML = FALSE) 
Dive_residual.m5 = lmer(all.dur ~ max.d + (1|ft) + (1|bout), data=dives, REML = FALSE) 
Dive_residual.m6 = lmer(all.dur ~ max.d + (1|ft/bout), data=dives, REML = FALSE)
Dive_residual.m7 = lmer(all.dur ~ max.d + (1|sealID), data=dives, REML = FALSE) 
Dive_residual.m8 = lmer(all.dur ~ max.d + (1|ft), data=dives, REML = FALSE) 
Dive_residual.m9 = lmer(all.dur ~ max.d + (1|bout), data=dives, REML = FALSE) 
Dive_residual.m10 = lmer(all.dur ~ max.d + (1|sealID/bout), data=dives, REML = FALSE)
Dive_residual.m11 = lmer(all.dur ~ max.d + (1|sealID/ft), data=dives, REML = FALSE)
Dive_residual.m12 = lmer(all.dur ~ max.d + (1|sealID/bout) + (1|ft), data=dives, REML = FALSE)
Dive_residual.m13 = lmer(all.dur ~ max.d + (1|sealID/ft) + (1|bout), data=dives, REML = FALSE)
Dive_residual.m13 = lmer(all.dur ~ max.d + (1|ft/bout) + (1|sealID), data=dives, REML = FALSE)

# (0+fixed effect|random effect) vary with slope
# (1+fixed effect|random effect) vary with slope and intercept
# (1|random effect) vary with intercept only
# (fixed effect||random effect) 

Dive_residual.m14 = lmer(all.dur ~ max.d + (max.d||sealID), data=dives, REML = FALSE) # Random slope and intercept per individual
Dive_residual.m15 = lmer(all.dur ~ max.d + (max.d||sealID/ft/bout), data=dives, REML = FALSE) # Random slope and intercept per individual

Dive_residual.m0 = lmer(all.dur ~ 1 + (1|sealID/ft/bout), data=dives, REML = FALSE) #effect of max.d

AICs = AIC(Dive_residual.m1,Dive_residual.m2,Dive_residual.m3,Dive_residual.m4,Dive_residual.m5,
    Dive_residual.m6,Dive_residual.m7,Dive_residual.m8)
delta_AIC <- AICs$AIC - min(AICs$AIC) 

summary(Dive_residual.m1)
AIC_Dive_residual.null = AIC(Dive_residual.null) # = 4130.57
logLik(Dive_residual.null)
coef(Dive_residual.null)
length(resid(Dive_residual.null))

plot(effect('max.d', Dive_residual.null))
plot(effect('sealID', Dive_residual.null))

length(predict(Dive_residual.null))
hist(resid(Dive_residual.m0))

length(resid(Dive_residual.null))


dives$dive_res = resid(Dive_residual.m1)
# ggplot(dives, aes(x=dive_res,y=seq(1,NROW(dives),1)))+
#   geom_point()
ggplot(dives, aes(x=dive_res))+
  geom_histogram(bins=30)
#Summer (Oct-Mar)
ggplot(dives %>% filter(JDay<=92|JDay>=275), aes(x=diel_phase, y=dive_res, fill=diel_phase))+
  geom_boxplot(outlier.shape=NA)+
  ylim(-100,150)+
  guides(fill=F)
#Winter (Apr-Sep)
ggplot(dives %>% filter(JDay>92&JDay<275), aes(x=diel_phase, y=dive_res, fill=diel_phase))+
  geom_boxplot(outlier.shape=NA)+
  ylim(-100,150)+
  guides(fill=F)

# Final dive res model ------------------------------------------------------------

## Full models ## - REML TRUE in linear mixed effects model
# We know there is a difference between all.dur of seals in winter vs summer. Definite seasonal differences in variance.
# Therefore allow for different variances per season by weighting
# Dive_residual.model = lmer(all.dur ~ max.d + (max.d|sealID), weights = season, data=dives, REML = FALSE)
Dive_residual.model = lmer(all.dur ~ max.d + (max.d|sealID), data=dives, REML = TRUE)
# Weights needs to be a numeric vector not 'summer' or 'winter'
summary(Dive_residual.model)
AIC(Dive_residual.model)

## Plotting ##
plot(resid(Dive_residual.model),ylim = c(-200,300))
# Get effects and then plot using ggplot
# residuals_ <- effect(('max.d', Dive_residual.model))
# ggplot(residuals_)

#OR
plot(effect('max.d', Dive_residual.model))

## testing whether random effects should be included or not
ranef(Dive_residual.model) # Gives posterior modes/distributions for each distinct group (levels of random effect)
str(rr1 <- ranef(Dive_residual.m7)) # Assessing whether there is variation between intercept and slope across seals
#Note: dotplot() requires 'lattice' package
dotplot(rr1) # There needs to be sufficient variation to make sealID a random effect i.e. Am I happy with the variation to keep sealID in model
qqmath(rr1) #quantile-quantile plot of sample and distribution


confint(Dive_residual.m7) # confidence intervals

# Assess fixed variable interactions
# You can update model to drop certain effects and even interactions
# e.g. Model1.Drop1 <-update(M8, .~.-max.d)
# anova(Model1, Model1.Drop1)

# Surface residuals -------------------------------------------------------

###### Surface residuals ##### # Essentially the physiological measure of cost
#shorter or longer pdsi for a given dive duration

dives_sr <- dives[which(dives$pdsi<600),]
ggplot(dives_sr, aes(x=pdsi))+
  geom_histogram(bins=30)

Surface_residual.m1 = lmer(pdsi ~ all.dur + (1|sealID/ft/bout), data=dives_sr, REML = FALSE)
Surface_residual.m2 = lmer(pdsi ~ all.dur + (0+all.dur|sealID) + (1|sealID), data=dives_sr, REML = FALSE)
Surface_residual.m3 = lmer(pdsi ~ all.dur + (all.dur||sealID), data=dives_sr, REML = FALSE)

AICs = AIC(Surface_residual.m1,Surface_residual.m2,Surface_residual.m3)
delta_AIC <- AICs$AIC - min(AICs$AIC)

# Final surface res model ------------------------------------------------------------
Surface_residual.m1 = lmer(pdsi ~ all.dur + (all.dur|sealID), data=dives_sr, REML = FALSE)
dives_sr$surf_res = resid(Surface_residual.m1)

# ggplot(dives, aes(x=dive_res,y=seq(1,NROW(dives),1)))+
#   geom_point()
ggplot(dives_sr, aes(x=surf_res))+
  geom_histogram(bins=30)
ggplot(dives_sr %>% filter(JDay<=92|JDay>=275), aes(x=diel_phase, y=surf_res, fill=diel_phase))+ # What is the sample size for each box?
  geom_boxplot(outlier.shape=NA)+
  ylim(-100,150)+
  guides(fill=F)
#Winter (Apr-Sep)
ggplot(dives_sr %>% filter(JDay>92&JDay<275), aes(x=diel_phase, y=surf_res, fill=diel_phase))+
  geom_boxplot(outlier.shape=NA)+
  ylim(-100,150)+
  guides(fill=F)

ggplot(dives_sr, aes(x=all.dur, y=pdsi))+
  geom_point()
abline(Surface_residual.m1)
# pdsi general trend. Is the data normal? Ofcourse not because there are seal bouts where seals prefer to forage at night. Why? Is it because of stratification?

# Time between bouts (pdsi). Seasonal differences?
dives %>% filter(pdsi<80000 & pdsi>20000) %.>% hist(.$pdsi/3600,100)
# pdsi within bouts
ggplot() + # distances from Marion. Shorter foraging trips in summer and shorter bouts.
  geom_point(aes(pdsi, distances..m., color = season), binwidth=40, data=(dives %>% filter(pdsi<80000 & pdsi>20000)))

ggplot() + # bout
  geom_point(aes(pdsi, bout, color = season), binwidth=40, data=(dives %>% filter(pdsi<80000 & pdsi>20000)))

ggplot() + # bout
  geom_boxplot(aes(ht_rat, diel_phase, color=season), binwidth=40, data=dives)

Surface_residual.null = lmer(pdsi ~ all.dur + (1|sealID), data=na.omit(dives), REML = TRUE) #+ hour + JDay
summary(Dive_residual.null)
AIC_Dive_residual.null = AIC(Dive_residual.null) # = 4130.57
logLik(Dive_residual.null)
coef(Dive_residual.null)
length(residuals(Dive_residual.null))

plot(effect('max.d', Dive_residual.null))
plot(effect('sealID', Dive_residual.null))

# % dives with ht_rat over 0.65 in winter vs summer
# winter
(dives %>% filter((JDay<100 | JDay>300) & ht_rat>0.5) %>% NROW())/(dives %>% filter(JDay<100 | JDay>300) %>% NROW())
# summer 
(dives %>% filter((JDay>100 | JDay<300) & ht_rat>0.5) %>% NROW())/(dives %>% filter(JDay>100 | JDay<300) %>% NROW())




# Data Exploration --------------------------------------------------------

###### Simple dive stats
# Bouts per seal
dives %>% group_by(sealID,bout) %>% count() %>% summarise() %>% count() %>% ungroup() %>% dplyr::select(n) %>% summary()

# Dives per bout 
summary((dives %>% group_by(sealID,bout) %>% count() %>% ungroup() %>% dplyr::select(n))$n)
sd((dives %>% group_by(sealID,bout) %>% count() %>% ungroup() %>% dplyr::select(n))$n)

# Dives per diel phase
dives %>% group_by(diel_phase) %>% count()

# max depth per seal
dives$sealID <- factor(dives$sealID)
dives$diel_phase <- factor(dives$diel_phase)
dives$bout <- factor(dives$bout)
# seasonal differences
ggplot(dives, aes(x=sealID, y=max.d, fill=season))+
  geom_boxplot()
# diel phase differences
ggplot(dives, aes(x=sealID, y=max.d, fill=diel_phase))+
  geom_boxplot()

# dive efficiency per seal
# seasonal differences
ggplot(dives, aes(x=sealID, y=dive_efficiency, fill=season))+
  geom_boxplot()
# diel phase differences
ggplot(dives, aes(x=sealID, y=dive_efficiency, fill=diel_phase))+
  geom_boxplot()

# ht_rat per seal
# seasonal differences
ggplot(dives, aes(x=sealID, y=ht_rat, fill=season))+
  geom_boxplot()
# diel phase differences
ggplot(dives, aes(x=sealID, y=ht_rat, fill=diel_phase))+
  geom_boxplot()

# Nt vs season
ggplot(dives %>% filter(Nt>0 | Nt<0), aes(x=sealID, y=Nt, fill=season))+
  geom_boxplot()


# distance travelled per sealID with seasonal differences
ggplot(dives, aes(x=sealID, y=distances..m., fill=season))+
  geom_boxplot()
#stats
(dives %>% group_by(sealID) %>% summarise(maxdist = max(distances..m.), seas=min(season)) %>% data.frame()) 
  %>% group_by(seas) %>% summarise(mindist = min(maxdist), maximdist = max(maxdist), meandist = mean(maxdist),mediandist = median(maxdist), stddist = sd(maxdist))

boxplot(mean_Temp~sealID, data=dives, xlab="sealID",ylab="mean_Temp")
boxplot(deltaT~sealID, data=dives, xlab="bout",ylab="deltaT")

boxplot(dive_res~diel_phase, data=dives, xlab="dive residuals",ylab="diel phase")
ggplot(dives, aes(x=diel_phase, y=dive_res, fill=diel_phase))+
  geom_boxplot(outlier.shape=NA)+
  ylim(0,100)+
  guides(fill=F)

###### Hunting
# % of dives without hunting
dives %>% filter(ht_rat==0) %>% NROW()/dives %>% NROW()*100

# max.d to hunting depth
plot(dives$max.d[!is.na(dives$Mdepth_hunting)],dives$Mdepth_hunting[!is.na(dives$Mdepth_hunting)])
plot(dives$Mdepth_hunting[!is.na(dives$Mdepth_hunting)]/dives$max.d[!is.na(dives$Mdepth_hunting)]*100)

hist(dives$max.d,xlim=c(0,200),ylim=c(0,20000),breaks=100)
hist(dives$Mdepth_hunting,xlim=c(0,200),ylim=c(0,20000),breaks=100)

# Plotting all dive locations ---------------------------------------------

sf_Marion <- data.frame("lon" = 37.746368, "lat" = -46.893361) %>% 
  st_as_sf(coords = c("lon", "lat"),crs=4326,remove = FALSE)

ggplot() + 
  geom_point(aes(x = lon , y = lat, color = sealID),data = dives) + #, pch = 19
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='green', data = sf_Marion) +
  ggtitle(paste('All SAFS trips   (',abs(round(min(loc1$lat),2)),'S,',
                round(min(loc1$lon),2),'E',' - ',abs(round(max(loc1$lat),2)),'S,',round(max(loc1$lon),2),'E)',sep = ''))


#' Calculate the successive distance between points
#' 
#' @param x The geometry column of an `sf` object
#' @return The distance between the `x[i]` and `x[i + 1]` elements of `x`
#' 
# distance_between_points <- function(x) {
#   null_point <- st_sf(x = st_sfc(st_point(x = c(0, 0))))
#   
#   x0 <- rbind(st_sf(x), null_point)
#   x1 <- rbind(null_point, st_sf(x))
#   
#   d <- st_distance(x0, x1, by_element = TRUE) %>%
#     .[-1]
#   
#   
#   d[length(d)] <- NA
#   d
#   
# }
# 
# distance_between_points(df$geometry)

sf_locs <- locs %>% group_by(id,ft) %>% st_as_sf(coords = c("lon", "lat"),crs=4326,remove = FALSE)
dist_between <- st_distance(sf_locs[-1,],sf_locs[-nrow(sf_locs),],by_element=TRUE)/1000


dist_between <- st_distance(sf_locs %>% filter(row_number()!=n()) %>% .[-1,],sf_locs[-nrow(sf_locs),],by_element=TRUE)/1000
sf_locs %>% filter(row_number()!=n()) %>% .[-1,] ## slice(c(1, n()))




ggplot(dives_sr, aes(x=all.dur, y=ht_rat))+
  geom_point()
ggplot(dives_sr, aes(x=max.d, y=ht_rat))+
  geom_point()



###### Changes made to dives 
dives$ft[(which(dives$sealID == 22 & is.na(dives$ft)))] <- 5



###### Plot Nt spatially
qplot(lat,Nt, data = dives)+theme_bw()+facet_wrap(~season)
qplot(lon,Nt, data = dives)+theme_bw()+facet_wrap(~season)

qplot(lat,Nt, data = dives)+theme_bw()+facet_grid() #plot for two variables

table(dives$Thermocline,dives$season)
