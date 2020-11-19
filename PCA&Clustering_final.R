## Principal Component Analysis
#---- Works best with numerical data (not binomial)

# Sean PCA ----------------------------------------------------------------

# Load packages -----------------------------------------------------------
# May have to re-install some packages after updating R, because the latest version of Rtools might not be downloaded.
# installr::install.Rtools()
# remotes::install_cran("dplyr", force = TRUE)
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

#create a dummy data frame
# new_my_data <- dummy.data.frame(dives, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s 
new_my_data <- dives[dives$pdsi<900,]                                

summary(new_my_data)

# Checking correlations between variables ---------------------------------

# Summer & winter
winter_dives <- new_my_data[new_my_data$season=='winter',]
summer_dives <- new_my_data[new_my_data$season=='summer',]
# Simple correlation first using stats::cor()
correlations <- summer_dives %>%
  dplyr::select("ht_rat","hARS_mode","max.d","pdsi","dive_res","X.bt.dt") %>% #'bottom_time','all.dur', "dive_efficiency","hunting_time"
  cor() %>% data.frame() 
correlations
# Simple correlation first using corrr::correlate() (I get the same answer... than with stats::cor())
#plotting correlations
# new_my_data %>% 
#   dplyr::select('dive_efficiency','ht_rat','max.d','pdsi','bottom_time','dive_res') %>% 
#   correlate() %>% 
#   network_plot()
# 
tidy_cors <- summer_dives %>%
  dplyr::select('dive_efficiency','ht_rat','max.d','pdsi','dive_res','bottom_time') %>%
  correlate() %>%
  stretch() %>%
  filter(abs(r) > .3) #%>%
# graph_from_data_frame(directed = FALSE)

ggraph(tidy_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Correlations between variables")
correlations %>% rplot()

#All variables
# res.pca <- PCA(new_my_data[,c('lon','lat','diel_phaseDawn','diel_phaseDay','diel_phaseDusk','diel_phaseNight',
#                               'JDay','all.dur','dive_efficiency','ht_rat','hARS_mode',
#                               'max.d','pdsi','bottom_time','bear.deg','dist','dive_res')], 
#                graph = FALSE, scale.unit = TRUE)


# PCA - only NB variables for each season (they may still correlate) -------------------------------------------------

# Summer & winter
winter_dives <- new_my_data[new_my_data$season=='winter',]
summer_dives <- new_my_data[new_my_data$season=='summer',]

# Only important variables
res.pca <- PCA(summer_dives[,c("ht_rat","hARS_mode","max.d","pdsi","dive_res","X.bt.dt")], 
               graph = FALSE, scale.unit = TRUE)

# fviz_pca_var(res.pca, col.var = "black")

# % variance explained by PCAs
fviz_eig(res.pca, addlabels = TRUE)

#### Quality of variables!
# head(get_pca_var(res.pca)$cos2, 4)

# cos2 - The quality of representation of the variables on factor map
corrplot(get_pca_var(res.pca)$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
#### Contribution of variables!
# highlight the most contributing variables for each dimension
corrplot(get_pca_var(res.pca)$contrib, is.corr=FALSE)
# Graph contributions
# The red dashed line on the graph above indicates the expected average contribution.
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC1&2
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
# Color by contributionson the factor map
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


# Cluster analysis --------------------------------------------------------
# https://www.datanovia.com/en/lessons/clara-in-r-clustering-large-applications/
# 'all.dur' and 'bottom_time' are highly corellated with all other variables in winter (using vif function)
# 'all.dur' is highly corellated with all other variables in summer (using vif function)
# Then those variables that are correlated to other single variables in the data are excluded (using threshold r2 of 0.6)
# Finally whether the variable contributes to the variation in the data by length of vector in PCA (e.g. hARS_mode)

NB_vars_winter <- c('ht_rat','max.d','pdsi','dive_res') # dive_res and ht_rat are somewhat correlated (0.48) and ht_rat and max.d are somewhat correlated (-0.54 inverse)
NB_vars_summer <- c("ht_rat","hARS_mode","max.d","pdsi","dive_res","X.bt.dt")
winter_dives <- new_my_data[new_my_data$season=='winter',]
# winter_dives <- winter_dives[winter_dives$ht_rat!=0,]
summer_dives <- new_my_data[new_my_data$season=='summer',]
# summer_dives <- summer_dives[summer_dives$ht_rat!=0,]

### Cluster
summer_dives_clus <- summer_dives[,NB_vars_summer] %>% scale() %>% na.omit()

fviz_nbclust(summer_dives_clus, clara, method = "gap_stat")  + # Other methods: 'silhouette' or 'gap_stat')
  geom_vline(xintercept = 3, linetype = 2)
summer_dives_clus<-clara(summer_dives_clus,4, samples = 50, pamLike = TRUE) # default samples = 5, recommend to set samples an order of magnitude larger
# winter_dives_clus<-fanny(winter_dives_clus,2) #Error: cannot allocate vector of size 252.4 Gb
# winter_dives_clus<-pam(winter_dives_clus,2) #260273 observations, but not more than 65536 are allowed
# clusGap(winter_dives_clus, FUN = clara, K.max = 8, B = 60) # Error: cannot allocate vector of size 252.4 Gb


# plot(winter_dives_clus)#), ask = TRUE)
clusplot(summer_dives_clus)
summer_dives$divetype <- NA
summer_dives$divetype <- summer_dives_clus$clustering

summer_dives %>% group_by(divetype) %>% count()

summer_dives$divetype<-as.factor(summer_dives$divetype)


for (i in 1:length(NB_vars_winter)){
  print(i)
  meanss <- winter_dives %>% group_by(divetype) %>% summarise(means = mean(NB_vars_winter[i])) %>% data.frame()
  p <- ggplot(winter_dives,aes(x=divetype,y=NB_vars_winter[i],fill = divetype))+geom_boxplot()+
          ggtitle(paste0('m1 = ',round(meanss[1,2],3),', m2 = ',round(meanss[2,2],3),', m3 = ',round(meanss[3,2],3)))
  save.image(paste0('C:/Users/Sean Evans/Documents/2020/MSc/Computing/MSc/Plots & Dive Tables/All seals/Exploratory analysis/Cluster analysis/Winter (k=3)/NB_vars/',NB_vars_winter[i],'.png'))
}

fviz_cluster(summer_dives_clus, geom = "point") + ggtitle("k = 4")


# Cluster where ht_rat==0 -------------------------------------------------

ggplot(winter_dives,aes(x=divetype,y=dive_res,fill = divetype))+geom_boxplot()

#create a dummy data frame
# new_my_data <- dummy.data.frame(dives, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s 
new_my_data <- dives[dives$pdsi<900 & dives$ht_rat==0,]                            

summary(new_my_data)

# Checking correlations between variables ---------------------------------

# Summer & winter
winter_dives <- new_my_data[new_my_data$season=='winter',]
summer_dives <- new_my_data[new_my_data$season=='summer',]
# Simple correlation first using stats::cor()
correlations <- winter_dives %>%
  dplyr::select('dive_efficiency','ht_rat','max.d','pdsi','dive_res','bottom_time') %>% #'bottom_time','all.dur'
  cor() %>% data.frame() 

# Simple correlation first using corrr::correlate() (I get the same answer... than with stats::cor())
#plotting correlations
# new_my_data %>% 
#   dplyr::select('dive_efficiency','ht_rat','max.d','pdsi','bottom_time','dive_res') %>% 
#   correlate() %>% 
#   network_plot()
# 
tidy_cors <- winter_dives %>%
  dplyr::select('dive_efficiency','ht_rat','max.d','pdsi','dive_res','bottom_time') %>%
  correlate() %>%
  stretch() %>%
  filter(abs(r) > .3) #%>%
# graph_from_data_frame(directed = FALSE)

ggraph(tidy_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Correlations between variables")
correlations %>% rplot()

#All variables
# res.pca <- PCA(new_my_data[,c('lon','lat','diel_phaseDawn','diel_phaseDay','diel_phaseDusk','diel_phaseNight',
#                               'JDay','all.dur','dive_efficiency','ht_rat','hARS_mode',
#                               'max.d','pdsi','bottom_time','bear.deg','dist','dive_res')], 
#                graph = FALSE, scale.unit = TRUE)


# PCA - only NB variables for each season (they may still correlate) -------------------------------------------------

# Summer & winter
winter_dives <- new_my_data[new_my_data$season=='winter',]
summer_dives <- new_my_data[new_my_data$season=='summer',]

# Only important variables
res.pca <- PCA(winter_dives[,c('dive_efficiency','ht_rat','hARS_mode','max.d','pdsi','bottom_time','dive_res',"all.dur")], 
               graph = FALSE, scale.unit = TRUE)

# fviz_pca_var(res.pca, col.var = "black")

# % variance explained by PCAs
fviz_eig(res.pca, addlabels = TRUE)

#### Quality of variables!
# head(get_pca_var(res.pca)$cos2, 4)

# cos2 - The quality of representation of the variables on factor map
corrplot(get_pca_var(res.pca)$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)


# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#### Contribution of variables!

# highlight the most contributing variables for each dimension
corrplot(get_pca_var(res.pca)$contrib, is.corr=FALSE)

# Graph contributions
# The red dashed line on the graph above indicates the expected average contribution.
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)


# Color by contributionson the factor map
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


# Cluster analysis --------------------------------------------------------
# 'all.dur' and 'bottom_time' are highly corellated with all other variables in winter (using vif function)
# 'all.dur' is highly corellated with all other variables in summer (using vif function)
# Then those variables that are correlated to other single variables in the data are excluded (using threshold r2 of 0.6)
# Finally whether the variable contributes to the variation in the data by length of vector in PCA (e.g. hARS_mode)
NB_vars_winter <- c('ht_rat','max.d','pdsi','dive_res') # dive_res and ht_rat are somewhat correlated (0.48) and ht_rat and max.d are somewhat correlated (-0.54 inverse)
NB_vars_summer <- c('ht_rat','hARS_mode','max.d','dive_res')
winter_dives <- new_my_data[new_my_data$season=='winter',]
summer_dives <- new_my_data[new_my_data$season=='summer',]

### Cluster
winter_dives_clus <- winter_dives[,NB_vars_winter] %>% scale() %>% na.omit()

winter_dives_clus<-clara(winter_dives_clus,2)
plot(winter_dives_clus)#), ask = TRUE)

winter_dives$divetype <- winter_dives_clus$clustering

winter_dives %>% group_by(divetype) %>% count()

winter_dives$divetype<-as.factor(winter_dives$divetype)
ggplot(winter_dives,aes(ht_rat,fill = divetype))+geom_boxplot()
