######## Cluster analysis ########

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
# library(factoextra) # clustering algorithms & visualization
# install.packages('factoextra')
library('dummies')

###### create a dummy data frame for all dives
dives2 <- dives
new_my_data2 <- dummy.data.frame(dives2, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s
new_my_data2 <- new_my_data2[,c(12,17,19,34)] #[,c(12,16,17,19,28,34)]
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
summer_dives2 <- dives[dives$season=='winter',]

##### Winter
winter_new_my_data2 <- dummy.data.frame(winter_dives2, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s
winter_new_my_data2 <- winter_new_my_data2[,c(12,16,17,19,28,34)]
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
summer_dives3 <- dives[dives$season=='winter',]

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
