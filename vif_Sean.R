# Here's the VIF code.

# select covariates based on variance inflation

# df is the data.frame of covariates you want to deflate. Each row is a cell in your raster, each column is a covariate.

# thresh is the threshold value for inflation. Any variable with a value above thresh will be excluded.

# If variable A is excluded because of VIF_A > thresh, it means that a regression of all other covariates vs A explains 90% or more of all the variance found in A

# This function returns the names of the variables to keep for your model. These are the most informative variables.
#Getting Environmental Variables for occupancy
library(raster)
library(sf)

# Read in data.frame of variables-------------
#Convert to data frame for vif ---
ras <- as.data.frame(r, na.rm = TRUE)
head(ras)

#Center and scale data
df <- as.data.frame(base::scale(ras, center = TRUE, scale = TRUE))
names(df)
# df$geomorphology <- as.factor(ras$geomorphology) #Add after scaling and centering because = factor
summary(df)

# install.packages("fmsb")
library("fmsb")

#VIF Function ----
vif_func<-function(df,thresh=10){
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

vif_summer <- vif_func(df)
vif_summer

# [1] "bathy"                "dist_canyon"          "dist_shelf"          
# [4] "mldclima"             "shflux"               "shflux_sd"           
# [7] "slope"                "summer_currmag"       "summer_eke"          
# [10] "summer_ice"           "summer_ice_edge_dist" "summer_ice_sd"       
# [13] "summer_ssh.grad"      "summer_sshA"          "summer_sst.grad"     
# [16] "summer_sst"           "summer_sstA"          "summer_windmag"      
# [19] "vmix"                 "vmix_sd"

names(df)[!(names(df) %in% vif_summer)]
rm(rastlist,ras,r,df)


