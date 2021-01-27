
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

#create a dummy data frame
new_my_data <- dummy.data.frame(dives, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s 
new_my_data <- new_my_data[new_my_data$pdsi<900,]                                 
                                                     
                                                     
summary(new_my_data)
# dives.pca <- prcomp(new_my_data[,c(5,6,7,8,9,10,11,12,14,16,17,18,19,21,22,26,27,28,29,30,31,32,34)],scale. = TRUE,center = TRUE)
### Foraging and dive movement variables only::
# dives.pca <- prcomp(new_my_data[,c(12,16,17,18,19,27,28,34)],scale. = TRUE,center = TRUE)
dives.pca <- prcomp(new_my_data[,c('lon','lat','diel_phaseDawn','diel_phaseDay','diel_phaseDusk','diel_phaseNight',
                                   'JDay','all.dur','dive_efficiency','ht_rat','hARS_mode',
                                   'max.d','pdsi','bottom_time','bear.deg','dist','dive_res')],
                    scale = TRUE, center = TRUE)

summary(dives.pca)
str(dives.pca)
# The total variance explained by the components is the sum of the variances of the components:
sum((dives.pca$sdev)^2) #13
# In this case, we see that the total variance is 13, which is equal to the number of standardised variables (13                                                                                                           variables). This is because for standardised data, the variance of each standardised variable is 1. The total variance
# is equal to the sum of the variances of the individual variables, and since the variance of each standardised variable
# is 1, the total variance should be equal to the number of variables (13 here).
screeplot(dives.pca, type="lines")

round((dives.pca$sdev)^2,3)
#So using Kaiser’s criterion: that we should only retain principal components for which the variance is above 1
# (when principal component analysis was applied to standardised data): we can see we should keep PC 1 to 4.

#A third way to decide how many principal components to retain is to decide to keep the number of components
#required to explain at least some minimum amount of the total variance. For example, if it is important to explain
#at least 80% of the variance, we would retain the first SIX principal components, as we can see from the output of
#“summary(enviro.pca)” that the first five principal components explain 72.6% of the variance (while the first four
#components explain just 64.9%, so are not sufficient).
# dives.pca$rotation
# 
# dives.pca$rotation[,1]
# dives.pca$rotation[,2]
# dives.pca$rotation[,3]
# dives.pca$rotation[,4]
# dives.pca$rotation[,5]
# dives.pca$rotation[,6]

biplot(dives.pca, scale = TRUE, xlab=paste("PCA 1 (", round(summary(dives.pca)$importance[2]*100, 1), "%)", sep = ""),ylab = paste("PCA 2 (", round(summary(dives.pca)$importance[5]*100, 1), "%)", sep = ""))
# plot(dives.pca$x[,1],dives.pca$x[,2])

# plot(dives.pca$rotation[,1],dives.pca$rotation[,2])

dives.pca$x

dives.pca$rotation
# dives.cor <- cor(new_my_data[,c(5,6,7,8,9,10,11,12,14,16,17,18,19,21,22,26,27,28,29,30,31,32,34)])



# PCA and correlations using FactoMineR ----------------------------------------------------
library("FactoMineR")
library("corrplot")
library('corrr')
library('ggraph')


# Checking correlations between variables ---------------------------------

# Summer & winter
winter_dives <- new_my_data[new_my_data$seasonsummer=='0',]
summer_dives <- new_my_data[new_my_data$seasonsummer=='1',]
# Simple correlation first using stats::cor()
correlations <- summer_dives %>%
  dplyr::select('dive_efficiency','ht_rat','max.d','pdsi','dive_res','bottom_time') %>% #'bottom_time','all.dur'
  cor() %>% data.frame() 

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
winter_dives <- new_my_data[new_my_data$seasonsummer=='0',]
summer_dives <- new_my_data[new_my_data$seasonsummer=='1',]

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



# Winter PCA --------------------------------------------------------------

winter_dives <- dives[dives$season=='winter',]

new_my_data_winter <- dummy.data.frame(winter_dives, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s 
new_my_data_winter <- new_my_data_winter[new_my_data_winter$pdsi<900,]                                 
summary(new_my_data_winter)

res.pca <- PCA(new_my_data_winter[,c('lon','lat','diel_phaseDawn','diel_phaseDay','diel_phaseDusk','diel_phaseNight',
                              'JDay','all.dur','dive_efficiency','ht_rat','hARS_mode',
                              'max.d','pdsi','bottom_time','bear.deg','dist','dive_res')], 
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



# Summer PCA --------------------------------------------------------------

summer_dives <- dives[dives$season=='summer',]

new_my_data_summer <- dummy.data.frame(summer_dives, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s 
new_my_data_summer <- new_my_data_summer[new_my_data_summer$pdsi<900,]                                 
summary(new_my_data_summer)

res.pca <- PCA(new_my_data_summer[,c('lon','lat','diel_phaseDawn','diel_phaseDay','diel_phaseDusk','diel_phaseNight',
                                     'JDay','all.dur','dive_efficiency','ht_rat','hARS_mode',
                                     'max.d','pdsi','bottom_time','bear.deg','dist','dive_res')], 
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

###### create a dummy data frame for all dives
dives2 <- dives
new_my_data2 <- dummy.data.frame(dives2, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s
new_my_data2 <- new_my_data2[,c('ht_rat','hARS_mode','max.d','pdsi','bottom_time','bear.deg','dist','dive_res')] #[,c(12,16,17,19,28,34)]
new_my_data2 <- scale(new_my_data2)
new_my_data2 <- na.omit(new_my_data2)

# Euc_distance <- get_dist(new_my_data)
# fviz_dist(Euc_distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# k2 <- kmeans(df, centers = 2, nstart = 25)


# pam(new_my_data,2)

## Clara 
clus2<-clara(new_my_data2,2)
plot(clus2)#), ask = TRUE)



dives2$divetype <- clus2$clustering

dives2 %>% group_by(divetype) %>% count()

dives2$divetype<-as.factor(dives2$divetype)
ggplot(dives2,aes(dive_res,fill = divetype))+geom_boxplot()

# count of thermocline presence in divetype by cluster analysis
dives2 %>% group_by(divetype,Thermocline) %>% count()


###### create a dummy data frame for each season
winter_dives2 <- dives[dives$season=='winter',]
summer_dives2 <- dives[dives$season=='summer',]

##### Winter
winter_new_my_data2 <- dummy.data.frame(winter_dives2, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s
winter_new_my_data2 <- winter_new_my_data2[,c('dive_efficiency','ht_rat','max.d','pdsi','dive_res')]
winter_new_my_data2 <- scale(winter_new_my_data2)
winter_new_my_data2 <- na.omit(winter_new_my_data2)

# Euc_distance <- get_dist(new_my_data)
# fviz_dist(Euc_distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# k2 <- kmeans(df, centers = 2, nstart = 25)


# pam(new_my_data,2)

## Clara 
winter_clus2<-clara(winter_new_my_data2,2)
plot(winter_clus2)#), ask = TRUE)



winter_dives2$divetype <- winter_clus2$clustering

winter_dives2 %>% group_by(divetype) %>% count()

winter_dives2$divetype<-as.factor(winter_dives2$divetype)
ggplot(winter_dives2,aes(max.d,fill = divetype))+geom_boxplot()

# count of thermocline presence in divetype by cluster analysis
winter_dives2 %>% group_by(divetype,Thermocline) %>% count()

##### Summer
summer_new_my_data2 <- dummy.data.frame(summer_dives2, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s
summer_new_my_data2 <- summer_new_my_data2[,c(12,16,17,19,28,34)]
summer_new_my_data2 <- scale(summer_new_my_data2)
summer_new_my_data2 <- na.omit(summer_new_my_data2)

# Euc_distance <- get_dist(new_my_data)
# fviz_dist(Euc_distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# k2 <- kmeans(df, centers = 2, nstart = 25)


# pam(new_my_data,2)

## Clara 
summer_clus2<-clara(summer_new_my_data2,2)
plot(summer_clus2)#, ask = TRUE)



summer_dives2$divetype <- summer_clus2$clustering

summer_dives2 %>% group_by(divetype) %>% count()

summer_dives2$divetype<-as.factor(summer_dives2$divetype)
ggplot(summer_dives2,aes(max.d,fill = divetype))+geom_boxplot()

# count of thermocline presence in divetype by cluster analysis
summer_dives2 %>% group_by(divetype,Thermocline) %>% count()


###### create a dummy data frame for all dives
dives3 <- dives
new_my_data3 <- dummy.data.frame(dives3, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s
new_my_data3 <- new_my_data3[,c(12,16,17,19,28,34)]
new_my_data3 <- scale(new_my_data3)
new_my_data3 <- na.omit(new_my_data3)

# Euc_distance <- get_dist(new_my_data)
# fviz_dist(Euc_distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# k2 <- kmeans(df, centers = 2, nstart = 25)


# pam(new_my_data,2)

## Clara 
clus3<-clara(new_my_data3,3)
plot(clus3)#), ask = TRUE)



dives3$divetype <- clus3$clustering

dives3 %>% group_by(divetype) %>% count()

dives3$divetype<-as.factor(dives3$divetype)
ggplot(dives3,aes(dive_efficiency,fill = divetype))+geom_boxplot()

# count of thermocline presence in divetype by cluster analysis
dives3 %>% group_by(divetype,Thermocline) %>% count()


###### create a dummy data frame for each season
winter_dives3 <- dives[dives$season=='winter',]
summer_dives3 <- dives[dives$season=='summer',]

##### Winter
winter_new_my_data3 <- dummy.data.frame(winter_dives3, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s
winter_new_my_data3 <- winter_new_my_data3[,c(12,16,17,19,28,34)]
winter_new_my_data3 <- scale(winter_new_my_data3)
winter_new_my_data3 <- na.omit(winter_new_my_data3)

# Euc_distance <- get_dist(new_my_data)
# fviz_dist(Euc_distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# k2 <- kmeans(df, centers = 2, nstart = 25)


# pam(new_my_data,2)

## Clara 
winter_clus3<-clara(winter_new_my_data3,2)
plot(winter_clus3)#), ask = TRUE)



winter_dives3$divetype <- winter_clus3$clustering

winter_dives3 %>% group_by(divetype) %>% count()

winter_dives3$divetype<-as.factor(winter_dives3$divetype)
ggplot(winter_dives3,aes(max.d,fill = divetype))+geom_boxplot()

# count of thermocline presence in divetype by cluster analysis
winter_dives3 %>% group_by(divetype,Thermocline) %>% count()

##### Summer
summer_new_my_data3 <- dummy.data.frame(summer_dives3, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s
summer_new_my_data3 <- summer_new_my_data3[,c(12,16,17,19,28,34)]
summer_new_my_data3 <- scale(summer_new_my_data3)
summer_new_my_data3 <- na.omit(summer_new_my_data3)

# Euc_distance <- get_dist(new_my_data)
# fviz_dist(Euc_distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# k2 <- kmeans(df, centers = 2, nstart = 25)


# pam(new_my_data,2)

## Clara 
summer_clus3<-clara(summer_new_my_data3,2)
plot(summer_clus3)#, ask = TRUE)



summer_dives3$divetype <- summer_clus3$clustering

summer_dives3 %>% group_by(divetype) %>% count()

summer_dives3$divetype<-as.factor(summer_dives3$divetype)
ggplot(summer_dives3,aes(max.d,fill = divetype))+geom_boxplot()

# count of thermocline presence in divetype by cluster analysis
summer_dives3 %>% group_by(divetype,Thermocline) %>% count()




###### Plotting divetype with PCA clusters
fviz_pca_ind(dives.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = dives$divetype, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

fviz_pca_biplot(iris.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = iris$Species, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)
