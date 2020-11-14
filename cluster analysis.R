######## Cluster analysis ########

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
install.packages('factoextra')
library('dummies')
#create a dummy data frame
new_my_data <- dummy.data.frame(dives, names = c("diel_phase",'season','Thermocline')) #Takes factors and creates a column for each level, which is made of 0s and 1s
new_my_data <- new_my_data[,c(12,16,17,19,28,34)]
new_my_data <- scale(new_my_data)
new_my_data <- na.omit(new_my_data)

Euc_distance <- get_dist(new_my_data)
fviz_dist(Euc_distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df, centers = 2, nstart = 25)


pam(new_my_data,2)
clus<-clara(new_my_data,2)
plot(clus, ask = TRUE)

dives$divetype <- clus$clustering

dives %>% group_by(divetype) %>% count()

#### Try with locs
