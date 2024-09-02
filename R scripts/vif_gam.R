# Here's the VIF code.

# select covariates based on variance inflation

# df is the data.frame of covariates you want to deflate. Each row is a cell in your raster, each column is a covariate.

# thresh is the threshold value for inflation. Any variable with a value above thresh will be excluded.

# If variable A is excluded because of VIF_A > thresh, it means that a regression of all other covariates vs A explains 90% or more of all the variance found in A

# This function returns the names of the variables to keep for your model. These are the most informative variables.
#Getting Environmental Variables for occupancy
# Load packages -----------------------------------------------------------
# May have to re-install some packages after updating R, because the latest version of Rtools might not be downloaded.
# installr::install.Rtools()
# remotes::install_cran("dplyr", force = TRUE)
library('mgcv')
library(remotes)
library(purrr)
library(move)
# library(moveVis)
library(argosfilter)
library(sf)
library(maps)
library(mapdata)
library(fields)
library(lattice)
library(effects)
library(trip)
library(Matrix)
library(diveMove)
library(pbapply)
library(wrapr)
library(stringr)
library(lubridate)
library(microbenchmark)
library(data.table)
library(ggplot2)
library(ncdf4)
library(raster)
library(magrittr)
library(rgl)
library(lme4)
library(nlme)
library(lmerTest)
library(dummies)
install.packages(itm)
#library(plyr)
theme_set(theme_bw(base_size = 16))
library(tidyverse)  # data manipulation
library(dplyr)
library(broom)
library(factoextra) # clustering algorithms & visualization
library(cluster)    # clustering algorithms
library(ggstatsplot)

# PCA and correlations using FactoMineR ----------------------------------------------------
library("FactoMineR")
library("corrplot")
library('corrr')
library('ggraph')

# Or use car::vif()
# Read in data.frame of variables-------------
# new_my_data <- dives[dives$pdsi<900,]
# new_my_data <- dives[dives$pdsi<900,] # & dives$ht_rat!=0
new_my_data <- dives
summary(new_my_data)

# summer & winter 
winter_dives <- new_my_data[new_my_data$season=='winter',]
summer_dives <- new_my_data[new_my_data$season=='summer',]
winter_dives$Nt <- abs(winter_dives$Nt)
summer_dives$Nt <- abs(summer_dives$Nt)

#Center and scale data
cor(winter_dives$Nt,winter_dives$mean_Temp)
cor(summer_dives$Nt,summer_dives$mean_Temp)
df <- as.data.frame(base::scale(winter_dives[,c('Nt','mean_Temp')], center = TRUE, scale = TRUE))
df <- as.data.frame(base::scale(summer_dives[,c('Nt','mean_Temp')], center = TRUE, scale = TRUE))

names(df)
# df$geomorphology <- as.factor(ras$geomorphology) #Add after scaling and centering because = factor
summary(df)

# install.packages("fmsb")
library("fmsb")
threshold = 10 # 5 (more conservative, but widely used) or 10 (>10 is mostly used)
#VIF Function ---- #Only for continuous variables
vif_func<-function(df,thresh=threshold){
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(df)
  for(val in var_names){
    
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = df))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  if(vif_max < thresh){
    return(var_names)
  }
  else{
    in_dat<-df
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      vif_vals<-NULL
      var_names <- names(in_dat)
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
    }
    return(names(in_dat))
  }
}

vif_result <- vif_func(df)
vif_result

names(df)[!(names(df) %in% vif_result)]
rm(df)





# Biserial correlation ----------------------------------------------------
#Computes the point-biserial correlation between a dichotomous and a continuous variable.

biserial.cor <-
  function (x, y, use = c("all.obs", "complete.obs"), level = 1) {
    if (!is.numeric(x))
      stop("'x' must be a numeric variable.\n")
    y <- as.factor(y)
    if (length(levs <- levels(y)) > 2)
      stop("'y' must be a dichotomous variable.\n")
    if (length(x) != length(y))
      stop("'x' and 'y' do not have the same length")
    use <- match.arg(use)
    if (use == "complete.obs") {
      cc.ind <- complete.cases(x, y)
      x <- x[cc.ind]
      y <- y[cc.ind]
    } 
    ind <- y == levs[level]
    diff.mu <- mean(x[ind]) - mean(x[!ind])
    prob <- mean(ind)
    sd.pop <- sd(x) * sqrt((length(x) - 1) / length(x))
    diff.mu * sqrt(prob * (1 - prob)) / sd.pop
  }

biserial.cor(summer_dives$Nt,summer_dives$Thermocline, level=2)