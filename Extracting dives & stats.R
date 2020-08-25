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

setwd("~/2020/MSc/Computing/Fur seals/Dive data for Sean Evans")
data_path = "Extracted raw dive data - from instrument helper/Fur seal data/dt_before_TDR"

fs_list <- list.files(data_path, pattern = ".Rda")
load(paste(data_path,fs_list[1],sep = "/"))

dt$date <- dt %.>% as.POSIXct(.$Time,tz="GMT",origin="1970-01-01")

df<-dt %>% select(sealID,date,Depth,External.Temp)
names(df) <- c("id","gmt","depth","temp")
filename = "Dive Data"

tdr <- createTDR(time = df$gmt,depth = df$depth, concurrentData = data.frame(df$temp), speed = FALSE, dtime = 1, file = filename)
                 # ,dateCol = 13,depthCol = 2,speed=FALSE,concurrentCols = 3:10, sep=";")

# plotTDR(tdr, interact=TRUE)
#start detecting periods of activity (i.e individual dives)
system.time({
y<-calibrateDepth(tdr, dive.thr=4, zoc.method='offset', offset=1, dry.thr=3600, wet.thr=30, interp.wet=FALSE,smooth.par=0.1, 
                  knot.factor=20, descent.crit.q=0.01, ascent.crit.q=0.00, na.rm=T)
#now create the "usual" metrics for each dive (Activity) identified above
z<-diveStats(y,depth.deriv = FALSE)
})

plotTDR(y, concurVars=c("df.temp"), diveNo = 5, what = "phase",surface=TRUE,interact=FALSE)

#Extracting "postdive.id", "dive.id", "dive.activity" from calibrated tdr file
# getDAct(y)$dive.activity[70000:71000] #where D=Diving and W=Wet. D is used for numbering dives (with id) and W is used for identifying PDSI
getDAct()
# getDPhaseLab(y, diveNo = 1:5) #gets dives associated phases 
# plotDiveModel(y, diveNo=2600)



# Basic stats:
#Duration,maxdepth,bottom time,post dive duration (pdsi)

#Shallow dives <15m and >60sec
shallow <- z %>% filter(maxdep < 15, divetim > 60) %>% select(num)
shallow_dives <- z %>% filter(num %in% shallow_dives$num)

deep_dives <- z %>% filter(maxdep >= 15, divetim >=60) %>% select(num)

# Bouts -------------------------------------------------------------------


## Example of MLE Bouts model
postdives <- z$postdive.dur
postdives.diff <- abs(diff(postdives))
## Remove isolated dives 
postdives.diff <- postdives.diff[postdives.diff < 2000] #select dives that are less than about 35min apart
lnfreq <- boutfreqs(postdives.diff, bw=0.1, plot=FALSE) 
startval <- boutinit(lnfreq, 50) 
p <- startval[[1]]["a"] / (startval[[1]]["a"] + startval[[2]]["a"])
## Fit the reparameterized (transformed parameters) model ## Drop names by wrapping around as.vector() 
init.parms <- list(p=as.vector(logit(p)),lambda1=as.vector(log(startval[[1]]["lambda"])), lambda2=as.vector(log(startval[[2]]["lambda"])))
bout.fit1 <- bouts.mle(bouts2.LL, start=init.parms, x=postdives.diff, method="L-BFGS-B", lower=c(-2, -5, -10))
coefs <- as.vector(coef(bout.fit1))
## Un-transform and fit the original parameterization 
init.parms <- list(p=unLogit(coefs[1]), lambda1=exp(coefs[2]), lambda2=exp(coefs[3]))
bout.fit2 <- bouts.mle(bouts2.ll, x=postdives.diff, start=init.parms, method="L-BFGS-B", lower=rep(1e-08, 3),control=list(parscale=c(1, 0.1, 0.01))) 
plotBouts(bout.fit2, postdives.diff)
## Plot cumulative frequency distribution 
plotBouts2.cdf(bout.fit2, postdives.diff)
## Estimated BEC 
bec <- bec2(bout.fit2) ## Label bouts
bouts <- postdives %>%  labelBouts(rep(bec, length(postdives)), bec.method="seq.diff")
z %<>% mutate("bout" = bouts) %>% rownames_to_column(var = "num") #Creates bout and num columns



## Example of NLS Bouts model


# Working with dives ------------------------------------------------------
 
as.data.frame(extractDive(y, diveNo %in% shallow_dives$num))


as.data.frame(extractDive(y, diveNo=as.numeric(shallow_dives$num)))
as.data.frame(extractDive(y, diveNo=as.numeric(c(1,2,3,4,5,6,7,8,9,10))))  



Shortlist_dives<-as.data.frame(extractDive(y, diveNo=as.numeric(as.numeric((z %>% filter(maxdep >6, divetim > 60) %>% select(num))$num))))
divesTDR
