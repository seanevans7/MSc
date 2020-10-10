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
library(ggstatsplot)
# library(ncdf4)
library(raster)
library(magrittr)
library(dplyr)
library(rgl)
library(lme4)
#require(GGally)
#require(reshape2)
#require(compiler)
#require(parallel)
#require(boot)

rm(list = ls())
# Which seal?
#sealtag <- "A146" # used for accessing and loading file 
# (If error = "invalid description argument" then check that seal doesn't have two files associated with it)
sealID <- 25 # Used for saving file 



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


divessummary <- read.csv(file.path(save_divessummary,"divessummary.csv"),sep = ',')
Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')

filtered_divestats = divessummary %>% filter(max.d >4, all.dur>60)

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
filtered_divestats$max.d %>% hist()




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
                                                                      'trans_t_s' = sd(transit_time),
                                                                      'max_depth'= mean(max.d),
                                                                      'std_max_depth' = sd(max.d))

qqplot(bsm_seg_df %>% filter(foraging == 'hunting') %>% select(vdist))
# Where X is a lat lon ID pair:
hist((divestats %>% group_by(X) %>% summarise(meanht = mean(hunting_time)))$meanht)
# Simple t-tests and GLM --------------------------------------------------


########################################################################################################################
################################################ Simple stats on dives  ################################################
####################### Are there signiifcant differences between hunting vs transit dives  ############################
####################### shallow vs deep dives, species, age, mass(kg),....                  ############################
########################################################################################################################

t.test(loc1$for_effort[loc1$for_effort>0 & loc1$diel_phase=='Dusk'], loc1$for_effort[loc1$for_effort>0 & loc1$diel_phase=='Dawn'])
t.test(loc1$for_effort[loc1$for_effort>0 & loc1$diel_phase=='Night'], loc1$for_effort[loc1$for_effort>0 & loc1$diel_phase=='Dawn'])
t.test(loc1$for_effort[loc1$for_effort>0 & loc1$diel_phase=='Night'], loc1$for_effort[loc1$for_effort>0 & loc1$diel_phase=='Dusk'])
plot3d(loc1$ht_rat,loc1$no_dives,loc1$for_effort)

#Shallow dives vs deep dives
filtered_divestats$max.d %>% plot()
filtered_divestats$max.d %>% hist()


# ##Do the behaviours differ in transit vs foraging modes
# #remember, mode 2 = transit and mode 1 =ARS
t.test(loc1$bottom[loc1$mode==1], loc1$bottom[loc1$mode==2])

boxplot(filtered_divestats$ht_rat ~ filtered_divestats$hunt_dive*filtered_divestats$Thermocline,
        col=c("white","lightgray"),filtered_divestats)
boxplot(filtered_divestats$dive_efficiency ~ filtered_divestats$hunt_dive*filtered_divestats$Thermocline,
        col=c("white","lightgray"),filtered_divestats)
boxplot(filtered_divestats$dive_efficiency ~ filtered_divestats$hunt_dive*filtered_divestats$diel_phase,
        col=c("white","lightgray"),filtered_divestats)
boxplot(-filtered_divestats$Mdepth_hunting ~ filtered_divestats$hunt_dive*filtered_divestats$diel_phase,
        col=c("white","lightgray"),filtered_divestats)
boxplot(-filtered_divestats$Mdepth_transit ~ filtered_divestats$hunt_dive*filtered_divestats$diel_phase,
        col=c("white","lightgray"),filtered_divestats)

Mdepth_hunting.model = lmer(Mdepth_hunting ~ hunt_dive  +
                              Thermocline + (1|diel_phase), data=filtered_divestats)
summary(Mdepth_hunting.model)

par(mfrow=c(1,1))
##plot the results
t <- tapply(filtered_divestats$dive_efficiency, filtered_divestats$Thermocline, mean, na.rm=T)
s <- tapply(filtered_divestats$dive_efficiency, filtered_divestats$Thermocline, sd, na.rm=T)
n <- tapply(filtered_divestats$dive_efficiency, filtered_divestats$Thermocline, length)
se <- s/sqrt(n)*2
mp <- barplot(t, beside = TRUE,
              col = c("red", "grey"),
              legend = rownames(t), ylim= c(0,max(t+se, na.rm=T)),
              main = "Dive efficiency ~ Thermocline present or absent", font.main = 4,
              cex.names = 1.5)

segments(mp, t+se, mp, t-se,  col = "black", lwd = 3)

t.test(filtered_divestats$'max.d'[filtered_divestats$Thermocline=='present'], filtered_divestats$'max.d'[filtered_divestats$Thermocline=='absent'])
t.test(filtered_divestats$hunting_time[filtered_divestats$Thermocline=='present'], filtered_divestats$hunting_time[filtered_divestats$Thermocline=='absent'])
t.test(filtered_divestats$'all.dur'[filtered_divestats$Thermocline=='present'], filtered_divestats$'all.dur'[filtered_divestats$Thermocline=='absent'])
t.test(filtered_divestats$ht_rat[filtered_divestats$Thermocline=='present'], filtered_divestats$ht_rat[filtered_divestats$Thermocline=='absent'])
t.test(filtered_divestats$'%bt/dt'[filtered_divestats$Thermocline=='present'], filtered_divestats$'%bt/dt'[filtered_divestats$Thermocline=='absent'])
t.test(filtered_divestats$mean_Temp[filtered_divestats$Thermocline=='present'], filtered_divestats$mean_Temp[filtered_divestats$Thermocline=='absent'])

##...but it is best to not do multiple t tests. Better to incorporate everything in one model
## in this case the data are binomial so it needs a GLM
mod.null <- glm(b.5-1~+1, family="binomial", data=na.omit(loc1))
mod1 <- glm(b.5~mean_max.d+all.dur+%bt/dt+hour+ht_rat+heffort, family="binomial", data=na.omit(loc1))

# GLM for location data ---------------------------------------------------

# But we do have random variables i.e. foraging trips and seal ids, Therefore need to use gmler instead of gml !!!!

##############################
### for_effort ~ mean_Temp ###
##############################

## Null model for effect of mean_Temp on for_effort
for_effort.null = lmer(for_effort ~ dive_efficiency + mean_max.d + diel_phase + b.5 + strat_prop + (1|ft), data=na.omit(loc1), REML = FALSE) #+ hour + JDay
summary(for_effort.null)
AIC_for_effort.null = AIC(for_effort.null) # = 4130.57
logLik(for_effort.null) # = -2055.285 (df=10)

## Run model for effect of mean_Temp on for_effort
for_effort.model = lmer(for_effort ~ mean_Temp + dive_efficiency + mean_max.d + diel_phase + b.5 + strat_prop + (1|ft), data=na.omit(loc1), REML = FALSE) #+ hour + JDay
summary(for_effort.model)
#residuals(mod.null)
# plot(effect("dive_efficiency", for_effort.model))
# plot(effect("mean_Temp", for_effort.model))
# plot(effect("mean_max.d", for_effort.model))
# plot(effect("diel_phase", for_effort.model))
# plot(effect("b.5", for_effort.model))
# plot(effect("strat_prop", for_effort.model))
AIC_for_effort.model = AIC(for_effort.model) # = 4042.495 
logLik(for_effort.model) # = -2010.248 (df=11)

#Did mean_Temp effect for_effort? (by how much and how?)
AIC_mod = AIC_for_effort.null - AIC_for_effort.model # Therefore the fixed effect mean_Temp is significant
#or
# Likelihood ratio test:
anova(for_effort.null,for_effort.model)

# End result e.g.: mean_Temp not affecting for_effort (χ2(1)=1e-04,	 p=0.9943),	 - if it were to be effecting it you would state for example: lowering	 it	 by	 about	19.7 Hz (Estimate)	± 5.6	(standard	errors) 



# Make diel phase a random variable
# for_effort ~dive_efficiency + mean_Temp + mean_max.d + diel_phase + b.5 + strat_prop + (1|ft)
mod.null = lmer(for_effort ~ dive_efficiency + mean_Temp + mean_max.d + (1|diel_phase) + b.5 + strat_prop + (1|ft), data=loc1) #+ hour + JDay
summary(mod.null)
#residuals(mod.null)
plot(effect("dive_efficiency", mod.null))
plot(effect("mean_Temp", mod.null))
plot(effect("mean_max.d", mod.null))
#plot(effect("diel_phase", mod.null))
plot(effect("b.5", mod.null))
plot(effect("strat_prop", mod.null))
AIC(mod.null) # = 4058.01 Worse
logLik(mod.null)

# for_effort ~ mean_Temp + mean_max.d + diel_phase + b.5 + strat_prop + (1|ft)
mod.null = lmer(for_effort ~ mean_Temp + mean_max.d + diel_phase + b.5 + strat_prop + (1|ft), data=loc1) #+ hour + JDay
summary(mod.null)
#residuals(mod.null)
# plot(effect("dive_efficiency", mod.null))
# plot(effect("mean_Temp", mod.null))
# plot(effect("mean_max.d", mod.null))
# plot(effect("diel_phase", mod.null))
# plot(effect("b.5", mod.null))
# plot(effect("strat_prop", mod.null))
AIC(mod.null) # = 4254.404
logLik(mod.null)

# for_effort ~ mean_max.d + diel_phase + b.5 + strat_prop
mod.null = glm(for_effort ~ mean_max.d + diel_phase + b.5 + strat_prop, data=loc1) #+ hour + JDay
summary(mod.null)
#residuals(mod.null)
plot(effect("mean_max.d", mod.null))
plot(effect("diel_phase", mod.null))
plot(effect("b.5", mod.null))
plot(effect("strat_prop", mod.null))
AIC(mod.null) # = 6856.25

# for_effort ~ mean_max.d + diel_phase + b.5
mod.null = glm(for_effort ~ mean_max.d + diel_phase + b.5, data=loc1) 
summary(mod.null)
#residuals(mod.null)
plot(effect("mean_max.d", mod.null))
plot(effect("diel_phase", mod.null))
plot(effect("b.5", mod.null))
AIC(mod.null) # = 6856.302 worst

# for_effort ~ diel_phase + b.5
mod.null = glm(for_effort ~ diel_phase + b.5, data=loc1)
summary(mod.null)
#residuals(mod.null)
plot(effect("diel_phase", mod.null))
plot(effect("b.5", mod.null))
AIC(mod.null) # 6854.439 best



# GLM for divessummary ----------------------------------------------------


# We do have random variables e.g. foraging trips, bouts and seal ids, Therefore need to use gmler instead of gml !!!!

##############################
### ht_rat ~ mean_Temp ###
##############################

## Null model for effect of mean_Temp on for_effort
ht_rat.null = lmer(ht_rat ~ dive_efficiency + max.d + all.dur + lon + lat + Thermocline + diel_phase + distances..m. + (1|ft) + (1|bout), data=divessummary, REML = FALSE) #+ hour + JDay
summary(ht_rat.null)
AIC_ht_rat.null = AIC(ht_rat.null) # = 4130.57 
logLik(ht_rat.null) # = -2055.285 (df=10)

## Run model for effect of mean_Temp on for_effort
ht_rat.model = lmer(ht_rat ~ mean_Temp + dive_efficiency + max.d + all.dur + lon + lat + Thermocline + diel_phase + distances..m. + (1|ft) + (1|bout), data=divessummary, REML = FALSE) #+ hour + JDay
summary(ht_rat.model)
#residuals(mod.null)
plot(effect("mean_Temp", for_effort.model))
plot(effect("dive_efficiency", for_effort.model))
plot(effect("max.d", for_effort.model))
plot(effect("all.dur", for_effort.model))
plot(effect("lon", for_effort.model))
plot(effect("lat", for_effort.model))
plot(effect("Thermocline", for_effort.model))
plot(effect("diel_phase", for_effort.model))
plot(effect("distances..m.", for_effort.model))
AIC_ht_rat.model = AIC(ht_rat.model) # = 4042.495 
logLik(ht_rat.model) # = -2010.248 (df=11)

#Did mean_Temp effect for_effort? (by how much and how?)
AIC_mod = AIC_ht_rat.null - AIC_ht_rat.model # Therefore the fixed effect mean_Temp is significant
#or
# Likelihood ratio test:
anova(ht_rat.null,ht_rat.model)

# End result e.g.: mean_Temp	 affected	 for_effort	 (χ2(1)=11.62,	 p=0.00065),	 lowering	 it	 by	 about	19.7 Hz (Estimate)	± 5.6	(standard	errors) 


# Lets try something a bit less ambitious

##############################
### vARS ~ hARS ###
##############################
hist(filtered_divestats$ht_rat*100)
ht_rat.null = lmer(ht_rat ~ diel_phase + (1|ft) + (1|bout), data=filtered_divestats, REML = FALSE, family='poisson') #+ hour + JDay


##############################
### ht_rat ~ mean_Temp ###
##############################

# Random Intercept Model
## Null model for effect of mean_Temp on for_effort
ht_rat.null = lmer(ht_rat ~ diel_phase + (1|ft) + (1|bout), data=filtered_divestats, REML = FALSE) #+ hour + JDay
summary(ht_rat.null)
AIC_ht_rat.null = AIC(ht_rat.null) 
logLik(ht_rat.null)

## Run model for effect of mean_Temp on for_effort
ht_rat.model_rim = lmer(ht_rat ~ mean_Temp + diel_phase + (1|ft) + (1|bout), data=filtered_divestats, REML = FALSE) #+ hour + JDay
summary(ht_rat.model_rim)
#residuals(mod.null)
plot(effect("mean_Temp", ht_rat.model_rim))
plot(effect("diel_phase", ht_rat.model_rim))
AIC_ht_rat.model = AIC(ht_rat.model_rim) 
logLik(ht_rat.model_rim)

#Did mean_Temp effect for_effort? (by how much and how?)
AIC_mod = AIC_ht_rat.null - ht_rat.model_rim # Therefore the fixed effect mean_Temp is significant
#or
# Likelihood ratio test:
anova(ht_rat.null,ht_rat.model_rim)

# End result:  mean_Temp	 affected	 ht_rat	 (χ2(1)=74.378  ,	 p=2.2e-16 ***), increasing it by about 0.040973 (Estimate)	± 0.004746 (standard	errors) 

##############################
### Interactions ????????? ###
##############################
# Test for inter-dependence between mean_Temp and diel_phase
ht_rat.model_inter = lmer(ht_rat ~ mean_Temp*diel_phase + (1|ft) + (1|bout), data=filtered_divestats, REML = FALSE) #+ hour + JDay
AIC_ht_rat.model_inter = AIC(ht_rat.model_inter)
AIC_mod = AIC_ht_rat.model_inter - AIC_ht_rat.model
# Likelihood ratio test:
anova(ht_rat.model_inter,ht_rat.model)

# End result:  interaction between fixed terms in the model is significant (χ2(1)=30.46  ,	 p=1.104e-06 ***)

coef(ht_rat.model_inter)


# Random Slope Model
## Tells model: Expect differing baseline-levels of ht_rat (the intercept, represented by 1) as well as differing responses to the main factor in question, which is “mean_Temp” in this case. 
ht_rat.model_rsm = lmer(ht_rat ~ mean_Temp*diel_phase + (1+mean_Temp|ft) + (1|bout), data=filtered_divestats, REML = FALSE) #+ hour + JDay
coef(ht_rat.model_rsm)
# Therefore, with individual variation in foraging trip, there is no consistency in how ht_rat is effected by mean_Temp 	

# Now compare with null model to test significance of this
ht_rat.model_rsm.null = lmer(ht_rat ~ diel_phase + (1+mean_Temp|ft) + (1|bout), data=filtered_divestats, REML = FALSE) #+ hour + JDay
# Likelihood ratio test:
anova(ht_rat.model_rsm.null,ht_rat.model_rsm)

# End result:  interaction between fixed terms and random terms in the model is significant (χ2(1)=36.171  ,	 p=2.668e-07 ***)

# Other glm ---------------------------------------------------------------



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
              main = "Hunting Time", font.main = 4,
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
## Map the distribution of a single seals behaviours
ggplot() + 
  geom_point(aes(x = lon , y = lat), color='green', size=8,pch = 19,data = (divessummary %>% filter(Thermocline=='present'))[,c(10,30,31)]) +
  geom_point(aes(x = lon , y = lat, color = hunting_time, size = for_effort), pch = 19,data = loc1) +
  geom_point(aes(x = lon , y = lat),pch = 17,size=5, color='red', data = sf_Marion) +
  geom_point(aes(x = lon , y = lat),pch = 1,size=1, color='red', data = loc1[loc1$'b.5'==2,]) +
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



sealID <- 25
save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")
divessummary <- read.csv(file.path(save_divessummary,"divessummary.csv"),sep=',')
Fts_summaries <- read.csv('Fts_summaries.csv',sep = ';')

#################### Recalibrating divestats times #################
#divessummary
divessummary$start =  force_tz(as.POSIXct(divessummary$start), tzone='GMT')
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

# Add column for bouts
time_diff <- difftime(divessummary$start, lag(divessummary$start, default = divessummary$start[1]), units = "sec")
divessummary$bout <- cumsum(ifelse(time_diff<3600*6,0,1))



# Adding hARS_mode to divessummary ----------------------------------------

# Adding hARS_mode column to divestats for later stats
tmin <- c(loc1$gmt - 1.25*3600,force_tz(as.POSIXct(last(loc1$gmt) + 1.25*3600,tz='GMT'), tzone = "GMT", roll = FALSE))
z <- seq(length(tmin)-1) ## Have to redefine z for divestats
#z <- z[1:(length(z)-1)]
divessummary$start <- force_tz(divessummary$start, tzone = "GMT", roll = FALSE)
z <- cut(divessummary$start,breaks = tmin, labels = z)
# divestats <- divestats %>% 
#   mutate("X" = as.integer(shift(z,-1))) %>% 
#   left_join(loc1 %>% select(X,lon,lat),by = "X")
divessummary <- divessummary %>% 
  mutate("X" = as.integer(z)) %>% 
  left_join(loc1 %>% select(X,b.5),by = "X")
colnames(divessummary)[which(names(divessummary) == "b.5")] <- "hARS_mode"



# Add columns to loc1 -----------------------------------------------------

##############################
### Add strat_prop to loc1 ###
##############################
loc1$strat_prop <- NA # No. of dives that are in stratified water masses

##run a loop over the location data, grabbing the corresponding dive data
##2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
system.time({
  for(i in 1: (nrow(loc1))) {
    print(paste(i,nrow(loc1)-1),sep=" ")
    tmin <- loc1$gmt[i] - (3600*1.25)
    tmax <- loc1$gmt[i] + (3600*1.25)
    mvar <- filtered_divestats[filtered_divestats$start >= tmin & filtered_divestats$start <= tmax,]
    if (NROW(mvar %>% filter(Thermocline=='present'))>0) {
      new_df <- mvar %>% filter(Thermocline=='present')
      loc1$strat_prop[i] <- NROW(new_df)
    }
    else {
      loc1$strat_prop[i] <- 0
    }
  }
})

##############################
### Add strat_prop to loc1 ###
##############################

loc1$mean_Temp <- NA
loc1$dive_efficiency <- NA

##run a loop over the location data, grabbing the corresponding dive data
##2.5 hr interval between locations, therefore chose start of dives that fell into 1.25 hrs either side of each location
system.time({
  for(i in 1: (nrow(loc1))) {
    print(paste(i,nrow(loc1)-1),sep=" ")
    tmin <- loc1$gmt[i] - (3600*1.25)
    tmax <- loc1$gmt[i] + (3600*1.25)
    mvar <- divessummary[divessummary$start >= tmin & divessummary$start <= tmax,]
    loc1$mean_Temp[i] <- mvar$mean_Temp %>% mean(na.rm=T)
    loc1$dive_efficiency[i] <- mvar$dive_efficiency %>% mean(na.rm=T)
  }
})
