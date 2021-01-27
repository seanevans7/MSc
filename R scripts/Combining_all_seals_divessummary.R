##################### Sean_fseal_VARS.R: Is a file containg code to process fur seal diving data. ############################

# Creating large dataframe
# By Sean Evans


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
library(lme4)
# install.packages('nlme')
library(nlme)
# library(lmerTest)
library('mgcv')
library(car)
library(ggpubr)
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

# Changing thermocline depths and presence
for (i in 1:length(SealIDS)) {
  print(i)
  sealID = SealIDS[i]
  save_loc = paste("Plots & Dive Tables/Seal",sealID, sep = "")
  Thermoclines1 <- read_csv(file.path(save_loc,paste("Thermoclines1.csv",sep = '')))
  Thermoclines1$sealID <- NaN
  Thermoclines1$sealID <- as.numeric(sealID)

  if (i==1) {
    Thermoclines = Thermoclines1
  }

  else{
    Thermoclines = rbind(Thermoclines,Thermoclines1)
  }
}

Thermoclines$X1 <- seq(1,348750,1)
Thermoclines<-anti_join(Thermoclines,Thermoclines %>% group_by(sealID) %>% filter(row_number()==1),by='X1')
Thermoclines$X1 <- seq(1,348711,1)
Thermoclines$sealID = NULL
dives$therm_depth = NULL
dives$Tmld = NULL
dives$Therm_dep = NULL
dives$X1 <- seq(1,348711,1)
dives<-full_join(dives,Thermoclines,by='X1')
dives$X1 = NULL

#######################################################
### Add max_diff_Therm & hunt_diff_Therm ###
#######################################################

dives <- dives %>%
  mutate(therm_depth = ifelse(!is.na(Therm_dep) & !is.na(Tmld) & abs(Therm_dep-Tmld)<=10 & Therm_dep-Tmld>=0, Tmld,
                              ifelse(!is.na(Therm_dep) & !is.na(Tmld) & abs(Therm_dep-Tmld)<=10 & Tmld-Therm_dep>0, Therm_dep,
                                     ifelse(!is.na(Therm_dep) & is.na(Tmld), NA, # was Therm_dep before NA
                                            ifelse(!is.na(Tmld) & is.na(Therm_dep), Tmld,
                                                   ifelse(!is.na(Therm_dep) & !is.na(Tmld) & abs(Therm_dep-Tmld)>10 & Therm_dep-Tmld>=0, Tmld,
                                                          ifelse(!is.na(Therm_dep) & !is.na(Tmld) & abs(Therm_dep-Tmld)>10 & Tmld-Therm_dep>0, Therm_dep,NA)))))),
         diff_therm = ifelse(!is.na(Therm_dep) & !is.na(Tmld), (Therm_dep-Tmld),NA), #error estimate
         hunt_diff_Therm = Mdepth_hunting-therm_depth,
         max_diff_Therm = max.d-therm_depth)

#######################################################
### Re-doing Thermocline presence based on above ###
#######################################################

dives$Thermocline <- NULL
dives <- dives %>%
  mutate(Thermocline = ifelse(!is.na(therm_depth), 'present','absent'))

# # Checking dives (divessummary and Thernmocline1)
# for (i in 1:length(SealIDS)) {
#   print(i)
#   sealID = SealIDS[i]
#   save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")
#   divessummary1 <- read.csv(file.path(save_divessummary,"divessummary.csv"),sep=',')
#   
#   if (i==1) {
#     divessummary = divessummary1[,c('sealID','max.d')]
#   }
#   
#   else{
#     divessummary = rbind(divessummary,divessummary1[,c('sealID','max.d')])
#   }
# }



summary(dives)

###### Fixing Thermocline column
# str(dives$Thermocline)
dives$Thermocline <- as.factor(dives$Thermocline)
# dives[(which(dives$Thermocline == "")),'Thermocline'] <- as.factor('absent')
# dives$Thermocline <- droplevels(dives$Thermocline)
# table(dives$Thermocline,dives$diel_phase)

###### Fixing ft,pdsi and mean_Temp columns by removing Na
dives <- dives[which(!is.na(dives$ft)),] # 
dives <- dives[which(!is.na(dives$mean_Temp)),]
dives <- dives[which(!is.na(dives$pdsi)),]

################# Reduce dives to those without nan values for ft and bout   #################
dives <- dives[!is.na(dives$ft) | !is.na(dives$bout),] # or dives <- dives[which(!is.na(dives$ft) | !is.na(dives$bout))]
# Omit dives that do not fit into within 24hours of the foraging trip interval time limits

###### add season column to dives using JDay ############
# Use 92+15 and 275+15 to avoid winter 'dives' in 'summer.' - refer to plot Seasonal_distribution_of_seal_data in 'Plots & Dive Tables\All seals\Exploratory analysis'
dives$season = NaN
dives[which(dives$JDay>107&dives$JDay<290),]$season = 'winter' # winter (Apr-Sep)
dives[which(dives$JDay<=107|dives$JDay>=290),]$season = 'summer' # summer (Oct-Mar)
dives$season <- as.factor(dives$season)


# This is from database in C:\Users\Sean Evans\Documents\2020\MSc\Computing\MSc
Wyear = tibble(year=c(2009,2009,2009,2009,2011,2011,2011,2012,2012,2013,2011,2011,2011,2011,2012,2013,2013,2013,2014),
       season=rep('winter',19),
       dives_pdsi %>% group_by(season,sealID) %>% count() %>% filter(season=='winter') %>% select(sealID) %>% ungroup() %>% select(sealID))
       
Syear = tibble(year=c(2011,2011,2011,2011,2012,2012,2012,2012,2012,2012,2012,2012,2013,2013,2013,2013,2013,2015,2015,2014),
               season=rep('summer',20),
               dives_pdsi %>% group_by(season,sealID) %>% count() %>% filter(season=='summer') %>% select(sealID) %>% ungroup() %>% select(sealID))

year_metadata = rbind(Wyear,Syear)
year_metadata$season = NULL
#dives
dives<-full_join(dives,year_metadata,by='sealID')
dives$year <- as.factor(dives$year)
#dives_pdsi
#dives_pdsi<-full_join(dives_pdsi,year_metadata,by='sealID')
#dives_pdsi$year <- as.factor(dives_pdsi$year)
#locs
names(locs)[1] <- "sealID"
locs$sealID <- as.factor(locs$sealID)
locs<-full_join(locs,year_metadata,by='sealID')
locs$year <- as.factor(locs$year)

##### diel phase to factor
dives$diel_phase<-as.factor(dives$diel_phase)

##### sealID to factor
dives$sealID <- as.factor(dives$sealID)


###### add season column to locs using JDay ############
# Use 92+15 and 275+15 to avoid winter 'dives' in 'summer.' - refer to plot Seasonal_distribution_of_seal_data in 'Plots & Dive Tables\All seals\Exploratory analysis'
locs$JDay = as.numeric(locs$JDay)
locs$season = NaN
locs[which(locs$JDay>107&locs$JDay<290),]$season = 'winter' # winter (Apr-Sep)
locs[which(locs$JDay<=107|locs$JDay>=290),]$season = 'summer' # summer (Oct-Mar)
locs$season <- as.factor(locs$season)

# Dive  residuals ----------------------------------------

##### Model types
# glm() - discription of error distribution. Stats package. No random effects!!
# glmer() - Distribution of response variable is known, fixed and random effects.
# lmer() - allows nested random effects and fits via REML or ML. lme4 package
# lme() - allows nested random effects within-group errors are allowed to be correlated and/or have unequal variances (i.e. weights). nlme package.

### Dive residuals ### Essentially the measure of diving effort
# Shorter or longer dive durations for a given depth
# Specify the error distribution and link function based on the distribution of the response variable (not normal - Binary/Count data) and if the errors are skewed
# Does my data fit a Poisson or normal distribution?
# mean(dives$all.dur) > sqrt(var(dives$all.dur)) # Therefore the data is not overdispersed (If it is, use quasi_ dist)

# #Using glm because the data (all.dur) is not normally distributed, it may fit a Poisson distribution better
# Dive_residual.null = glm(all.dur ~ max.d + (1|sealID) + (1|ft) + (1|bout), family = gaussian(link = "cloglog"), data=dives) # 8448
# summary(Dive_residual.null)
# The summary will say: 'Dispersion parameter for poisson family taken to be 1'

# #Poisson has to be integers! Wont work with my data
# # Test for overdispersion
# Dive_residual.null = glm(all.dur ~ max.d + (1|sealID) + (1|ft) + (1|bout), family = quasipoisson(link = "log"), data=dives) # 
# 
# ######## gmler - fixed & random effects, cloglog distribution
# ## gmler with max.d as fixed and random effect and seal id as random effect
# M1 = glmer(all.dur ~ max.d + (1|sealID) + (1|max.d), family = gaussian(link = "cloglog"), data=dives) # 
# mean((dives$all.dur - predict(lme, newdata = dives, type = "response"))^2)
# hist(residuals(M1))
# plot(M1)
# plot(residuals(M1))
# qqnorm(residuals(M1))
# qqline(residuals(M1))
# 
# summary(M1) # Dispersion parameter = ??
# 
# ######## gmler - fixed & random effects, inverse.gaussian distribution
# ## gmler with max.d as fixed and random effect and seal id as random effect
# M1 = glmer(all.dur ~ max.d + (1|sealID) + (1|max.d), family = inverse.gaussian(link = "inverse"), data=dives) # 
# mean((dives$all.dur - predict(lme, newdata = dives, type = "response"))^2)
# hist(residuals(M1))
# plot(M1)
# plot(residuals(M1))
# qqnorm(residuals(M1))
# qqline(residuals(M1))
# 
# M1 = glmer(all.dur ~ max.d + (1|sealID) + (1|max.d), family=Gamma(link=log), data=dives) # 
# mean((dives$all.dur - predict(lme, newdata = dives, type = "response"))^2)
# hist(residuals(M1))
# plot(M1)
# plot(residuals(M1))
# qqnorm(residuals(M1))
# qqline(residuals(M1))
# 
# ######## gmler - fixed & random effects, cloglog distribution
# ## gmler with max.d as fixed and random effect and seal id as random effect
# M1 = glmer(all.dur ~ max.d + (1|sealID) + (1|max.d), family = Gamma("inverse"), data=dives) # 
# mean((dives$all.dur - predict(lme, newdata = dives, type = "response"))^2)
# hist(residuals(M1))
# plot(M1)
# plot(residuals(M1))
# qqnorm(residuals(M1))
# qqline(residuals(M1))

# Final dive res model used ------------------------------------------------------------

######## lme - fixed & random effects, weighting with season
# lme assumes Gaussian, which my data is not
M1<-lme(all.dur~max.d, random=list(~1|sealID, ~1|max.d),
        weights=varIdent(form=~1|season), data = dives)
# mean((dives$all.dur - predict(lme, newdata = dives, type = "response"))^2)
hist(residuals(M1))
plot(M1)
plot(residuals(M1))
qqnorm(residuals(M1))
qqline(residuals(M1))
# 
# ## Reduced or null models ## - REML FALSE when comparing models with different fixed effects. Make TRUE for final model.
# ##  Models do not have comparable likelihoods when fitted using REML.
# Dive_residual.m1 = lmer(all.dur ~ max.d + (1|sealID/ft/bout), data=dives, REML = FALSE)
# Dive_residual.m1 = lmer(all.dur ~ max.d + (1|sealID:ft:bout), data=dives, REML = FALSE) # What is the difference between ':' and '/'
# plot(resid(Dive_residual.m1),ylim = c(-200,300))
# Dive_residual.m2 = lmer(all.dur ~ max.d + (1|sealID) + (1|ft) + (1|bout), data=dives, REML = FALSE) 
# Dive_residual.m3 = lmer(all.dur ~ max.d + (1|sealID) + (1|ft), data=dives, REML = FALSE) 
# Dive_residual.m4 = lmer(all.dur ~ max.d + (1|sealID) + (1|bout), data=dives, REML = FALSE) 
# Dive_residual.m5 = lmer(all.dur ~ max.d + (1|ft) + (1|bout), data=dives, REML = FALSE) 
# Dive_residual.m6 = lmer(all.dur ~ max.d + (1|ft/bout), data=dives, REML = FALSE)
# Dive_residual.m7 = lmer(all.dur ~ max.d + (1|sealID), data=dives, REML = FALSE) 
# Dive_residual.m8 = lmer(all.dur ~ max.d + (1|ft), data=dives, REML = FALSE) 
# Dive_residual.m9 = lmer(all.dur ~ max.d + (1|bout), data=dives, REML = FALSE) 
# Dive_residual.m10 = lmer(all.dur ~ max.d + (1|sealID/bout), data=dives, REML = FALSE)
# Dive_residual.m11 = lmer(all.dur ~ max.d + (1|sealID/ft), data=dives, REML = FALSE)
# Dive_residual.m12 = lmer(all.dur ~ max.d + (1|sealID/bout) + (1|ft), data=dives, REML = FALSE)
# Dive_residual.m13 = lmer(all.dur ~ max.d + (1|sealID/ft) + (1|bout), data=dives, REML = FALSE)
# Dive_residual.m13 = lmer(all.dur ~ max.d + (1|ft/bout) + (1|sealID), data=dives, REML = FALSE)
# 
# # (0+fixed effect|random effect) vary with slope
# # (1+fixed effect|random effect) vary with slope and intercept
# # (1|random effect) vary with intercept only
# # (fixed effect||random effect) 
# 
# Dive_residual.m14 = lmer(all.dur ~ max.d + (max.d||sealID), data=dives, REML = FALSE) # Random slope and intercept per individual
# Dive_residual.m15 = lmer(all.dur ~ max.d + (max.d||sealID/ft/bout), data=dives, REML = FALSE) # Random slope and intercept per individual
# 
# Dive_residual.m0 = lmer(all.dur ~ 1 + (1|sealID/ft/bout), data=dives, REML = FALSE) #effect of max.d
# 
# AICs = AIC(Dive_residual.m1,Dive_residual.m2,Dive_residual.m3,Dive_residual.m4,Dive_residual.m5,
#     Dive_residual.m6,Dive_residual.m7,Dive_residual.m8)
# delta_AIC <- AICs$AIC - min(AICs$AIC) 
# 
# summary(Dive_residual.m1)
# AIC_Dive_residual.null = AIC(Dive_residual.null) # = 4130.57
# logLik(Dive_residual.null)
# coef(Dive_residual.null)
# length(resid(Dive_residual.null))
# 
# plot(effect('max.d', Dive_residual.null))
# plot(effect('sealID', Dive_residual.null))
# 
# length(predict(Dive_residual.null))
# hist(resid(Dive_residual.m0))
# 
# length(resid(Dive_residual.null))
# 

# Add dive_res column and basic plot exploration --------------------------
res <- residuals(M1)
dives$dive_res <- res

# ggplot(dives, aes(x=dive_res,y=seq(1,NROW(dives),1)))+
#   geom_point()
ggplot(dives, aes(x=dive_res))+
  geom_histogram(bins=30)
#Summer (Oct15-Apr15)
ggplot(dives %>% filter(JDay<=107|JDay>=290), aes(x=diel_phase, y=dive_res, fill=diel_phase))+
  geom_boxplot(outlier.shape=NA)+
  ylim(-4,6)+
  guides(fill=F)+
  ggtitle('Summer dive_res')
#Winter (Apr15-Oct15)
ggplot(dives %>% filter(JDay>107&JDay<290), aes(x=diel_phase, y=dive_res, fill=diel_phase))+
  geom_boxplot(outlier.shape=NA)+
  ylim(-4,6)+
  guides(fill=F)+
  ggtitle('Winter dive_res')

# ## More models ## - REML TRUE in linear mixed effects model
# # We know there is a difference between all.dur of seals in winter vs summer. Definite seasonal differences in variance.
# # Therefore allow for different variances per season by weighting
# # Dive_residual.model = lmer(all.dur ~ max.d + (max.d|sealID), weights = season, data=dives, REML = FALSE)
# Dive_residual.model = lmer(all.dur ~ max.d + (1|sealID) + (1|max.d), weights = season, data=dives, REML = TRUE)
# 
# ####### Log data
# hist(log(dives$all.dur))
# hist(log(dives$max.d))
# Dive_residual.model = lmer(log(all.dur) ~ log(max.d) + (log(max.d)|sealID), data=dives, REML = TRUE)
# 
# ####### scale data
# hist(scale(dives$all.dur))
# hist(scale(dives$max.d))
# Dive_residual.model = lmer(scale(all.dur) ~ scale(max.d) + (scale(max.d)|sealID), data=dives, REML = TRUE)
# 
# # Weights needs to be a numeric vector not 'summer' or 'winter'
# summary(Dive_residual.model)
# AIC(Dive_residual.model)
# 
# ## Plotting ##
# plot(resid(Dive_residual.model))
# # Get effects and then plot using ggplot
# # residuals_ <- effect(('max.d', Dive_residual.model))
# # ggplot(residuals_)
# 
# #OR
# plot(effect('max.d', Dive_residual.model))
# 
# ## testing whether random effects should be included or not
# ranef(M1) # Gives posterior modes/distributions for each distinct group (levels of random effect)
# str(rr1 <- ranef(Dive_residual.m7)) # Assessing whether there is variation between intercept and slope across seals
# #Note: dotplot() requires 'lattice' package
# dotplot(rr1) # There needs to be sufficient variation to make sealID a random effect i.e. Am I happy with the variation to keep sealID in model
# qqmath(rr1) #quantile-quantile plot of sample and distribution
# 
# 
# confint(Dive_residual.m7) # confidence intervals
# 
# # Add dive_res to dataframe
# dives$dive_res <- resid(Dive_residual.model)
#   
# # Assess fixed variable interactions
# # You can update model to drop certain effects and even interactions
# # e.g. Model1.Drop1 <-update(M8, .~.-max.d)
# # anova(Model1, Model1.Drop1)


# Saving dives (Not including surface residuals) --------------------------
# Excluding sealID==57 West dives - too far West to be an East diving seal (Only 30 dives in one night)

# Saving data
dives_pdsi <- dives[dives$pdsi<900,]
save_all = 'Plots & Dive Tables/'
saveRDS(dives,file.path(save_all,"dives.rds"))
## PDSI>=900 removed
dives_pdsi$WE <- NaN
dives_pdsi$WE[dives_pdsi$lon>=37.746368] <- 'East'
dives_pdsi$WE[dives_pdsi$lon<37.746368] <- 'West'
dives_pdsi$WE <- as.factor(dives_pdsi$WE)
saveRDS(dives_pdsi,file.path(save_all,"dives_pdsi.rds"))

excludes <- dives_pdsi %>% filter(sealID==57 & lon>=37.746368)
diving<-anti_join(dives_pdsi,excludes)
saveRDS(diving,file.path(save_all,"diving.rds"))
diving<-readRDS(file.path(save_all,'diving.rds'))

saveRDS(dives_sr,file.path(save_all,"dives_sr.rds"))
write.csv(dives_sr,paste0(save_all,"dives_sr.csv"))
saveRDS(locs,file.path(save_all,"locs.rds"))

# Reading data
save_all = 'Plots & Dive Tables/'
dives = readRDS(file.path(save_all,"dives.rds"))
dives_pdsi = readRDS(file.path(save_all,"dives_pdsi.rds"))
locs = readRDS(file.path(save_all,"locs.rds"))


# Adding divetype to locs as a proportion ---------------------------------

SealIDS = c(1,2,4,5,seq(13,25,1),31,47,seq(49,53,1),57,58,seq(62,70,1),109,110,112,113)
locs$divetype_prop <- NA # No. of dives that are 

for (i in 1:length(SealIDS)) {
  print(i)
  sealID = SealIDS[i]
  
  system.time({
    for(j in 1: (nrow(loc1))) {
      loc1 <- locs %>% filter(id==sealID)
      mvar <- summer_dives %>% filter(sealID==sealID,lat==loc1$lat,lon==loc1$lon)
      loc1$divetype_prop[j] <- NROW(new_df)/NROW(mvar)
    }
    else {
      loc1$strat_prop[j] <- 0
    }
  }
})

# Surface residuals -------------------------------------------------------

###### Surface residuals ##### # Essentially the physiological measure of cost
#shorter or longer pdsi for a given dive duration

dives_sr <- dives[which(dives$pdsi<120),] # 2 minutes
NROW(dives_sr)/NROW(dives)*100

# Only looking at dives that are within 2 minutes of each other (within bouts) accounts for 88% of the data
ggplot(dives_sr, aes(x=season, y=pdsi, fill=season))+
  geom_boxplot()+
  ggtitle('pdsi by season')

# 
# 
# Surface_residual.m1 = lmer(pdsi ~ all.dur + (1|sealID/ft/bout), data=dives_sr, REML = FALSE)
# Surface_residual.m2 = lmer(pdsi ~ all.dur + (0+all.dur|sealID) + (1|sealID), data=dives_sr, REML = FALSE)
# Surface_residual.m3 = lmer(pdsi ~ all.dur + (all.dur||sealID), data=dives_sr, REML = FALSE)
# 
# AICs = AIC(Surface_residual.m1,Surface_residual.m2,Surface_residual.m3)
# delta_AIC <- AICs$AIC - min(AICs$AIC)

# Final surface res model ------------------------------------------------------------

# What does the distribution look like
ggplot(dives_sr,aes(x=all.dur,y=pdsi, colour = season))+
  geom_point()
qplot(dives_sr$pdsi)
# lme assumes Gaussian, which my data is not
M2<-lme(pdsi~all.dur, random=list(~1|sealID, ~1|all.dur),
        weights=varIdent(form=~1|season), data = dives_sr)
# mean((dives$all.dur - predict(lme, newdata = dives, type = "response"))^2)
hist(residuals(M2))
plot(M2)
plot(residuals(M2))
qqnorm(residuals(M2))
qqline(residuals(M2))

dives_sr$surf_res <- residuals(M2) 
#        
# Surface_residual.m1 = lmer(pdsi ~ all.dur + )all.dur|sealID), data=dives_sr, REML = FALSE)
# plot(resid(Surface_residual.m1))
# dives_sr$surf_res = resid(Surface_residual.m1)

# ggplot(dives, aes(x=dive_res,y=seq(1,NROW(dives),1)))+
# #   geom_point()
# ggplot(dives_sr, aes(x=surf_res))+
#   geom_histogram(bins=30)
ggplot(dives_sr %>% filter(JDay<=92|JDay>=275), aes(x=diel_phase, y=surf_res, fill=diel_phase))+ # What is the sample size for each box?
  geom_boxplot(outlier.shape=NA)+
  ylim(-100,150)+
  guides(fill=F)
#Winter (Apr-Sep)
ggplot(dives_sr %>% filter(JDay>92&JDay<275), aes(x=diel_phase, y=surf_res, fill=diel_phase))+
  geom_boxplot(outlier.shape=NA)+
  ylim(-100,150)+
  guides(fill=F)

# ggplot(dives_sr, aes(x=all.dur, y=pdsi))+
#   geom_point()
# abline(Surface_residual.m1)
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
# https://www.r-graph-gallery.com/
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

# max.d to hunting depth
plot(dives$max.d[!is.na(dives$Mdepth_hunting)],dives$Mdepth_hunting[!is.na(dives$Mdepth_hunting)])
plot(dives$Mdepth_hunting[!is.na(dives$Mdepth_hunting)]/dives$max.d[!is.na(dives$Mdepth_hunting)]*100)

hist(dives$max.d,xlim=c(0,200),ylim=c(0,20000),breaks=100) # For all max.d
hist(dives$max.d[!is.na(dives$Mdepth_hunting)],xlim=c(0,200),ylim=c(0,20000),breaks=100) # For only max.d where Mdepth_hunting != Na
hist(dives$Mdepth_hunting,xlim=c(0,200),ylim=c(0,20000),breaks=100)

###### Thermocline
# Depth difference between thermocline and max.d
ggplot()+
  geom_histogram(aes(max_diff_Therm),data = dives[dives$Thermocline=='present' & !is.na(dives$max_diff_Therm),], bins = 50)+
  ggtitle('Depth difference between thermocline and max.d')
# Depth difference between thermocline and Mdepth_hunting
ggplot()+
  geom_histogram(aes(hunt_diff_Therm),data = dives[dives$Thermocline=='present' & !is.na(dives$hunt_diff_Therm),], bins = 50)+
  ggtitle('Depth difference between thermocline and Mdepth_hunting')
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



###### Changes made to dives with na because ft was taken from ft_summaries.csv which had some errors
dives$ft[(which(dives$sealID == 22 & is.na(dives$ft)))] <- 4



###### Plot Nt spatially
qplot(lat,Nt, data = dives)+theme_bw()+facet_wrap(~season)
qplot(lon,Nt, data = dives)+theme_bw()+facet_wrap(~season)



qplot(lat,Nt, data = dives)+theme_bw()+facet_grid() #plot for two variables

table(dives$Thermocline,dives$season)

##### Max depth with time
xyplot(0-max.d ~  start | season , dives,
       grid = TRUE,
       panel = panel.smoothScatter)

xyplot(0-max.d ~  diel_phase | season , dives,
       grid = TRUE,
       panel = panel.smoothScatter)

xyplot(bottom_time ~  lat | season , dives,
       grid = TRUE,  layout = c(1,2),
       panel = panel.smoothScatter)
# lat,lon - diel_phase
xyplot(lat ~ lon | diel_phase , dives,
       grid = TRUE,  layout = c(4,1),
       panel = panel.smoothScatter)

# bottom time - with diel_phase [lat,lon]
xyplot(lat ~ bottom_time | diel_phase , dives,
       grid = TRUE,  layout = c(4,1),
       panel = panel.smoothScatter)

xyplot(bottom_time ~  lon | diel_phase , dives,
       grid = TRUE,  layout = c(1,4),
       panel = panel.smoothScatter)

xyplot(bottom_time ~  lat | diel_phase , dives[which(dives$ht_rat!=0),],
       grid = TRUE, index.cond=list(c(2,3,4,1)),
       panel = panel.smoothScatter)

xyplot(bottom_time ~  lat | diel_phase , dives[which(dives$ht_rat==0),],
       grid = TRUE, index.cond=list(c(2,3,4,1)),
       panel = panel.smoothScatter)

xyplot(diel_phase ~  bottom_time | season , dives[which(dives$ht_rat!=0),],
       grid = TRUE,
       panel = panel.smoothScatter)

xyplot(Thermocline ~  bottom_time | season*diel_phase , dives,
       grid = TRUE,
       panel = panel.smoothScatter)

xyplot(lat ~  mean_Temp | season*diel_phase , dives,
       grid = TRUE, groups = sealID,
       panel = panel.smoothScatter)

xyplot(max.d ~  Thermocline | season*diel_phase , dives,
       grid = TRUE,
       panel = panel.smoothScatter)


## In order to use max.d in the cluster analysis for dive type which is then a response variable for a model whose fixed variable is Thermocline presence, the Thermocline presence must not be correlated to max.d 
ggplot(dives, aes(x=Thermocline, y=max.d, fill=Thermocline))+
  geom_boxplot()+
  facet_wrap(~season)
  # ylim(-4,6)+
  # guides(fill=F)+
  ggtitle('Is Thermocline dependent on max.d')

# ggplot(dives, aes(bear.deg))+
#   geom_histogram(fill='blue',color='black', bins=30)+
#   coord_polar()+
#   facet_wrap(~season)+
#   ggtitle('Direction')

ggplot(dives, aes(bear.deg, fill=season))+
  geom_histogram(color='black',bins=60)+
  coord_polar(start = pi)+
  facet_wrap(~colony)+
  ggtitle('Direction')

M3 = glmer(Thermocline ~ scale(max.d) + (1|sealID), family = binomial(link = "logit"), data=dives)
summary(M3)

# Assessing bouts ---------------------------------------------------------

dives %>% group_by(sealID,bout) %>% count() %>% ungroup() %>%  dplyr::select(n) %>% summarise(meann = mean(n),min=min(n),max=max(n),stdev=sd(n))

n<-dives %>% group_by(sealID,bout) %>% count() %>% dplyr::select(n) %>% ungroup() %>% dplyr::select(n)
n <- cbind(unlist(n,use.names = FALSE) %>% data_frame(),dives %>% group_by(sealID,bout) %>% summarise(seasons = season[1]))
colnames(n) <- c('bout_dives','sealID','bout','seasons')
n$bout_dives %>% qplot(bins=100,xlab = paste0('Bout dives n=',n %>% NROW()),ylab = 'Count',fill=n$seasons)


######Add season column to locs using JDAY 
locs$season = NaN
locs$JDay<-as.numeric(locs$JDay)
locs[which(locs$JDay>92&locs$JDay<275),]$season = 'winter' # winter (Apr-Sep)
locs[which(locs$JDay<=92|locs$JDay>=275),]$season = 'summer' # summer (Oct-Mar)
locs$season<-as.factor(locs$season)

# Plot Loc dives
locs$no_dives[locs$no_dives>0] %>% qplot(bins=40,xlab = paste0('Loc dives n=',locs$no_dives[locs$no_dives>0] %>% NROW()),ylab = 'Count',fill=locs$season[locs$no_dives>0])
locs$no_dives %>% qplot(bins=40,xlab = paste0('Loc dives n=',locs$no_dives %>% NROW()),ylab = 'Count',fill=locs$season)



dty<-dives %>% group_by(divetype) %>% dplyr::select(ht_rat)
dty <- unlist(dty,use.names = FALSE) %>% data_frame()



dives$divetype<-as.factor(dives$divetype)
ggplot(dives,aes(bottom_time,fill = divetype))+geom_boxplot()

# count of thermocline presence in divetype by cluster analysis
dives %>% group_by(divetype,Thermocline) %>% count()



# ht_rat differences in dives ---------------------------------------------
# % of dives without hunting
dives %>% filter(ht_rat==0) %>% NROW()/dives %>% NROW()*100



# Effect of physical water properties on dive types -----------------------

ggplot(winter_dives,aes(x=divetype,y=Nt,fill = divetype))+geom_boxplot()+ylab('Nt')

my_gam_model <- gamm(divetype~Nt+mean_Temp+Thermocline+lat+lon, random=list(sealID=~1), data=winter_dives, family = binomial)
 



# West-East analysis of physical oceanography -----------------------------
dives = dives[dives$pdsi<900,]
# Summer
east <- dives %>% filter(lon>=37.746368 & season=='summer') %>% mutate(zonal_dir='East')
west <- dives %>% filter(lon<37.746368 & season=='summer') %>% mutate(zonal_dir='West')
west$sealID %>% NROW()
west %>% group_by(sealID) %>% count()
east %>% group_by(sealID) %>% count()
we_dives <- rbind(west,east)
rm(west,east)
we_dives$zonal_dir <- as.factor(we_dives$zonal_dir)
ggplot(we_dives, aes(therm_depth))+
  geom_histogram(color='black',bins=60,na.rm=TRUE)+
  facet_wrap(zonal_dir~.)+
  ggtitle('Therm_depth')

# winter
east <- dives %>% filter(lon>=37.746368 & season=='winter') %>% mutate(zonal_dir='East')
west <- dives %>% filter(lon<37.746368 & season=='winter') %>% mutate(zonal_dir='West')
west %>% group_by(sealID) %>% count()
east %>% group_by(sealID) %>% count()
we_dives <- rbind(west,east)
rm(west,east)
we_dives$zonal_dir <- as.factor(we_dives$zonal_dir)
ggplot(we_dives, aes(therm_depth))+
  geom_histogram(color='black',bins=60,na.rm=TRUE)+
  facet_wrap(zonal_dir~.)+
  ggtitle('Therm_depth')

# Seasonality and inter-annual data structure

# What is the seasonal distributoion of data. From this we can see if it would be worth it to split data into e.g. late summer and early summer to see if there would be a greater difference in mean temperature profile. 
# Create a text
grob1 <- grobTree(textGrob("Summer", x=0.1,  y=0.95, hjust=0, vjust=0.2,
                          gp=gpar(col="red", fontsize=12, fontface="italic")))
grob2 <- grobTree(textGrob("Summer", x=0.8,  y=0.95, hjust=0, vjust=0.2,
                           gp=gpar(col="red", fontsize=12, fontface="italic")))
grob3 <- grobTree(textGrob("Winter", x=0.52,  y=0.95, hjust=0, vjust=0.2,
                           gp=gpar(col="blue", fontsize=12, fontface="italic")))
grob4 <- grobTree(textGrob("15 April", x=0.30,  y=0.90, hjust=1, vjust=0, rot = 90,
                           gp=gpar(col="black", fontsize=12, fontface="italic")))
grob5 <- grobTree(textGrob("15 October", x=0.755,  y=0.90, hjust=1, vjust=0, rot = 90,
                           gp=gpar(col="black", fontsize=12, fontface="italic")))
ggplot(dives)+
  geom_histogram(aes(JDay,fill=year),bins = 365)+
  geom_vline(aes(xintercept = 107),size=1, col='blue')+
  geom_vline(aes(xintercept = 290),size=1, col='red')+
  annotation_custom(grob1)+
  annotation_custom(grob2)+
  annotation_custom(grob3)+
  annotation_custom(grob4)+
  annotation_custom(grob5)+
  theme_bw()+
  ggtitle('Seasonal_distribution_of_seal_data')
  theme(plot.title = element_text(size=22,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        legend.title = element_text(face="bold",size=18),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))
rm(grob1,grob2,grob3,grob4,grob5)



## Percentage of locations within the area over which geostrophic velocities have been averaged (between -42 and -50 lat)
(locs %>% filter(lat<=-42 & lat>=-50) %>% NROW())/(locs %>% NROW())

## Table of summer vs winter gernal seal movements
diving %>% filter(season=='winter') %>% summary()
(diving %>% filter(season=='summer'))$dive_res %>%  sd(na.rm = TRUE)
(diving %>% group_by(season,sealID,ft) %>% summarise(sum_ht = sum(hunting_time)) %>% filter(season=='summmer'))$sum_ht %>% summary()
diving %>% group_by(season) %>% summarise()

#### Thermocline absent vs present and hunting dive or not (based on ht-rat) - table
no.of.dives <- dives %>% group_by(Thermocline) %>% count() %>% t()

dives$hunting <- NaN
dives[which(dives$ht_rat==0),]$hunting = 'transit' 
dives[which(dives$ht_rat>0),]$hunting = 'hunting' 


Therm_table<- diving %>% group_by(Thermocline) %>% 
  summarise('Dive_duration' = mean(all.dur),
            'Dive_depth' = mean(max.d),
            'Bottom_time' = mean(bottom_time),
            'vARS' = mean(ht_rat),
            'Dive_residual' = mean(dive_res),
            'Dive_efficiency' = mean(dive_efficiency),
            'Nt' = mean(Nt),
            'Temp' = mean(mean_Temp),
            'dist_to_thermocline' = mean(hunt_diff_Therm),
            'Mdepth_hunting' = mean(Mdepth_hunting),
            'hunt_diff_therm' = mean(na.omit(hunt_diff_Therm)),
            'max_diff_therm' = mean(na.omit(max_diff_Therm))) %>% t() 
colnames(Therm_table) <- Therm_table[1,]
Therm_table<-Therm_table[-1,]
Therm_table

# Groupby season & Thermocline
Table<- diving %>% group_by(season,WE,Thermocline) %>% 
  summarise('dives' = n(),
            'Dive_duration' = paste0(as.character(round(mean(all.dur),2)),' (',as.character(round(sd(all.dur),2)),')'),
            'Dive_depth' = paste0(as.character(round(mean(max.d),2)),' (',as.character(round(sd(max.d),2)),')'),
            'Bottom_time' = paste0(as.character(round(mean(bottom_time),2)),' (',as.character(round(sd(bottom_time),2)),')'),
            'vARS' = paste0(as.character(round(mean(ht_rat),2)),' (',as.character(round(sd(ht_rat),2)),')'),
            'Dive_residual' = paste0(as.character(round(mean(dive_res),2)),' (',as.character(round(sd(dive_res),2)),')'),
            'Dive_efficiency' = paste0(as.character(round(mean(dive_efficiency),2)),' (',as.character(round(sd(dive_efficiency),2)),')'),
            'Nt' = paste0(as.character(round(mean(Nt),2)),' (',as.character(round(sd(Nt),2)),')'),
            'Temp' = paste0(as.character(round(mean(mean_Temp),2)),' (',as.character(round(sd(mean_Temp),2)),')'),
            'dist_to_thermocline' = paste0(as.character(round(mean(na.omit(hunt_diff_Therm)),2)),' (',as.character(round(sd(na.omit(hunt_diff_Therm)),2)),')'),
            'Mdepth_hunting' = paste0(as.character(round(mean(na.omit(Mdepth_hunting)),2)),' (',as.character(round(sd(na.omit(Mdepth_hunting)),2)),')'),
            'max_diff_therm' = paste0(as.character(round(mean(na.omit(max_diff_Therm)),2)),' (',as.character(round(sd(na.omit(max_diff_Therm)),2)),')')) %>% t() 


### T-tests 
multi.tests <- function(fun = t.test, df, vars, group.var, ...) {
  sapply(simplify = FALSE,                                    # sapply(simplify=T) better, elements named
         vars,                                                # loop on vector of outcome variable names
         function(var) {
           formula <- as.formula(paste(var, "~", group.var))# create a formula with outcome and grouping var.
           fun(data = df, formula, ...)                     # perform test with a given fun, default t.test
         }
  )
}


# One-Way Anova -----------------------------------------------------------

# One-way Anova assumes normality and equal variance!

diving %>% group_by(season) %>%
  summarise(
    count = n(),
    mean = mean(max.d, na.rm = TRUE),
    sd = sd(max.d, na.rm = TRUE)
  )
M1<-aov(max.d~season,diving)
summary(M1)
TukeyHSD(M1)
plot(M1,1) #Checking homogeneity of variances
leveneTest(max.d ~season, diving) # Testing for equal variances 
# There is evidence to suggest that the variances across groups is statistically significantly different. Therefore cannot assume homogeneity of variances in the different treatment groups 
## Anova test with no assumption of equal variances (Welch one-way test)
oneway.test(max.d ~season, diving)
pairwise.t.test(diving$max.d, diving$season,p.adjust.method = "BH", pool.sd = FALSE)
## Testing for normality
plot(M1,2) #dots should follow line - way off means data is not normal
## Test further
# Extract the residuals
aov_residuals <- residuals(object = M1)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals[0:5000]) #H0 = normality

res.multi.t.tests <- multi.tests(fun = t.test,df = diving,vars = c("max.d","all.dur"),group.var = "season",var.equal = TRUE)
res.multi.t.tests
## p-values can be extracted from the result object
data.frame(p.value = sapply(res.multi.t.tests, getElement, name = "p.value"))
#Multiple ANOVA: Make sure the grouping variable is a factor (categorical variable in R)
res.multi.anova <-  multi.tests(fun = oneway.test,df = diving,vars = c("max.d","all.dur"),group.var = "season",var.equal = TRUE)
res.multi.anova

# Kruskal-Wallis test -----------------------------------------------------

# When Anova assumptions are not met use a non-parametric alternative to a one-way anova called Kruskal-Wallis rank sum test
kruskal.test(max.d ~season, data = diving)
#p-value less than significance level 0.05 suggests significant difference between treatment groups
pairwise.wilcox.test(diving$max.d, diving$season,p.adjust.method = "BH")#, pool.sd = FALSE)

#Multiple Kruskal-Wallis tests
res.multi.kruskal.tests <-multi.tests(fun = kruskal.test,df = diving,vars = c("max.d","all.dur"),group.var = "WE")
res.multi.kruskal.tests
#Multiple plotting
plots <- multi.tests(fun = plot,df = diving,vars = c("max.d","all.dur"),group.var = "WE")
statistics <- multi.tests(fun = summary,df = diving,vars = c("max.d","all.dur"),group.var = "WE")


diving %>% filter(max.d<10) %>% view()

diving$hr <- as.factor(strftime(diving$start-3600*2, format="%H"))
diving$hr <- factor(diving$hr , levels=c("12","13","14","15","16","17","18", "19", "20", "21", "22", "23", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11"))
qplot(hr,max.d, data = diving)+theme_bw()+facet_wrap(~season)

### Plotting max depth per hour boxplot with sample size shown
diving$hr <- as.numeric(strftime(diving$start-3600*2, format="%H"))
hr_labels <- diving %>% group_by(season,hr) %>% count()
hr_labels <- (hr_labels %>% filter(season=='summer' & (hr>13 | hr<6))) %>% ungroup() %>%  dplyr::select(hr,n)
hr_labels$n <- as.character(hr_labels$n)
colnames(hr_labels)[2] <- 'hr_n'
sdiving <- diving %>% filter(season=='summer' & (hr>13 | hr<6))
sdive <- full_join(sdiving,hr_labels,by='hr')
hr_labels <- diving %>% group_by(season,hr) %>% count()
hr_labels <- (hr_labels %>% filter(season=='winter' & (hr>13 | hr<6))) %>% ungroup() %>%  dplyr::select(hr,n)
hr_labels$n <- as.character(hr_labels$n)
colnames(hr_labels)[2] <- 'hr_n'
wdiving = diving %>% filter(season=='winter' & (hr>13 | hr<6))
wdive = full_join(wdiving,hr_labels,by='hr')

divings <- rbind(sdive,wdive)
rm(hr_labels,sdive,wdive)

# ggplot(divings)+
#   geom_boxplot(aes(hr,max.d,fill=hr_n))+
#   #geom_vline(aes(xintercept = 107),size=1, col='blue')+
#   #geom_vline(aes(xintercept = 290),size=1, col='red')+
#   facet_wrap(~season)

library(RColorBrewer)
colourCount = length(unique(divings$hr))
divings$hr = as.factor(divings$hr)
divings$hr <- factor(divings$hr , levels=c("14","15","16","17","18", "19", "20", "21", "22", "23", "0", "1", "2", "3", "4", "5"))
divings$hr_n = as.factor(divings$hr_n)

divings$hr_n <- factor(divings$hr_n , labels = c("14","15","16","17","18", "19", "20", "21", "22", "23", "0", "1", "2", "3", "4", "5"))

getPalette = colorRampPalette(brewer.pal(9, "Set1"))
mycol = list("#874F6F" ,"#E41A1C" ,"#3881B0", "#449B75","#999999", "#56A255" ,"#7E6E85", "#CB8CAD" ,"#AC5782", "#E3712B", "#FFA10D", "#FFE528", "#E1C62F","#B16C29", "#C66764" ,"#F17EB4" )

ggplot()+
  geom_boxplot(data = divings %>% filter(season=='summer'),aes(hr,max.d,color=hr_n),outlier.shape = NaN,lwd=0.725)+
  #scale_fill_brewer(palette="Dark2")+
  #scale_fill_manual(values = getPalette(colourCount))+
  scale_fill_manual(values = mycol)+
  theme_bw()+
  ggtitle("Summer")
  #geom_point(data = hr_labels,aes(hr,n))+
  #scale_y_continuous(name = 'max.d',sec.axis=sec_axis(~.*n,name = 'n'))+
  #theme_ipsum()

ggplot(divings %>% filter(season=='winter'),aes(hr,max.d,color=hr_n))+
  geom_boxplot(outlier.shape=NA,lwd=0.725)+
  #scale_fill_brewer(palette="BuPu")+
  #scale_fill_manual(values = getPalette(colourCount))+
  scale_fill_manual(values = mycol)+
  theme_bw()+
  ggtitle("Winter")





ggplot()+
  geom_boxplot(data = divings %>% filter(season=='winter'),aes(hr,max.d,color=hr),outlier.shape = NaN,lwd=0.725)+
  #scale_fill_brewer(palette="Dark2")+
  #scale_fill_manual(values = getPalette(colourCount))+
  scale_fill_manual(values = mycol)+
  theme_bw()+
  ggtitle("Winter")
hr_labels <- diving %>% group_by(season,hr) %>% count()
(hr_labels %>% filter(season=='winter' & (hr>13 | hr<6))) %>% ungroup() %>%  dplyr::select(hr,n) %>% view()


ggplot(data = locs,aes(x=lat,color=season,fill=season))+
  geom_density(alpha=0.4)+
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=100,alpha=0)+
  geom_vline(aes(xintercept = -46.893361),size=1, col='blue',linetype="dashed")+
  annotate(geom="text", x=-50, y=1, label="South",color="black")+
  annotate(geom="text", x=-43.5, y=1, label="North",color="black")+
  theme(legend.position="right")+
  theme_classic()+ #theme_minimal()
  coord_flip()
  


ggplot(data = locs,aes(x=lon,color=season,fill=season))+
  geom_density(alpha=0.4)+
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=50,alpha=0)+
  geom_vline(aes(xintercept = 37.746368),size=1, col='blue',linetype="dashed")+
  annotate(geom="text", x=30, y=0.17, label="West",color="black")+
  annotate(geom="text", x=46, y=0.17, label="East",color="black")+
  theme(legend.position="right")+
  theme_classic() #theme_minimal()

    
