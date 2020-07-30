
##############################################################################################################################
############################################# Post-processing of dives #######################################################
#############################################      By Sean Evans       #######################################################
##############################################################################################################################


# Load packages -----------------------------------------------------------

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

write.csv(dout,file.path(resDir,paste(seal,"_divesummary.csv")))
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








# Quite r session ---------------------------------------------------------

q("yes")
