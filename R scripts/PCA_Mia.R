
## Principal Component Analysis
#---- Works best with numerical data (not binomial)
## First I need to standardise my values...
gaz <- na.omit(subset(tr, Site == "MM025"))
vdb <- na.omit(subset(tr, Site == "MM067"))
mp <- na.omit(subset(tr, Site == "MM042"))

#Choosing variables
spgaz <- as.data.frame(scale(gaz[,c(18:22,25,27:30)]))
spvdb <- as.data.frame(scale(vdb[,c(18:22,25,27:30)]))
spmp <- as.data.frame(scale(mp[,c(18:22,25,27:30)]))

spgaz <- cbind(gaz$id,spgaz)
spvdb <- cbind(vdb$id,spvdb)
spmp <- cbind(mp$id,spmp)


standardisedpredictors <-as.data.frame(scale(tr[,c(18:22,25,27:30)])) # standardise the variables
head(tr)

standardisedpredictors <- cbind(temp$id,standardisedpredictors )

sp <- standardisedpredictors
temp <- na.omit(tr)
# do a PCA
gaz.pca <- prcomp(spgaz[,2:11])
vdb.pca <- prcomp(spvdb[,2:11])
mp.pca <- prcomp(spmp[,2:11])
enviro.pca <- prcomp(sp[,2:11])
summary(mp.pca)

# This gives us the standard deviation of each component, and the proportion of variance explained by each compo-
#   nent. The standard deviation of the components is stored in a named element called “sdev” of the output variable
# made by “prcomp”:

gaz.pca$sdev

The total variance explained by the components is the sum of the variances of the components:
sum((gaz.pca$sdev)^2)

# In this case, we see that the total variance is 13, which is equal to the number of standardised variables (13                                                                                                           variables). This is because for standardised data, the variance of each standardised variable is 1. The total variance
# is equal to the sum of the variances of the individual variables, and since the variance of each standardised variable
# is 1, the total variance should be equal to the number of variables (13 here).

In order to decide how many principal components should be retained, it is common to summarise the results of a
principal components analysis by making a screenplot, which we can do in R using the “screeplot()” function:

screeplot(enviro.pca, type="lines")

(enviro.pca$sdev)^2
#So using Kaiser’s criterion: that we should only retain principal components for which the variance is above 1
# (when principal component analysis was applied to standardised data): we can see we should keep PC 1 to 4.

# A third way to decide how many principal components to retain is to decide to keep the number of components
# required to explain at least some minimum amount of the total variance. For example, if it is important to explain
# at least 80% of the variance, we would retain the first SIX principal components, as we can see from the output of
# “summary(enviro.pca)” that the first five principal components explain 80.1% of the variance (while the first four 
# components explain just 60.9%, so are not sufficient).

gaz.pca$rotation[,1]
gaz.pca$rotation[,2]
gaz.pca$rotation[,3]
gaz.pca$rotation[,4]
gaz.pca$rotation[,5]
gaz.pca$rotation[,6]

biplot(vdb.pca, scale = TRUE)
biplot(mp.pca, scale = TRUE)
biplot(gaz.pca, scale = TRUE)
biplot(enviro.pca, scale = TRUE)
plot(enviro.pca$x[,1],enviro.pca$x[,2])

# make a scatterplot
text(enviro.pca$x[1:500,1],enviro.pca$x[1:500,2], sp$id[1:500], cex = 0.7, col="red" )

#Let's make a dataframe out of first 6 PC's
pcs <- as.data.frame(cbind(enviro.pca$x[,1:6]))
head(pcs)
nrow(pcs)
nrow(data2)

forPCA <- na.omit(data2)
PCA <- cbind(forPCA,pcs) 
head(PCA)




# Sean PCA ----------------------------------------------------------------

#load library
library(dummies)
library(tidyverse)
library(broom)
theme_set(theme_bw(base_size = 16))
library(ggplot2)
library(factoextra)


# install.packages('dummies')
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



# PCA using FactoMineR ----------------------------------------------------
# install.packages('FactoMineR')
# install.packages('corrplot')
library("FactoMineR")
library("corrplot")

res.pca <- PCA(new_my_data[,c('lon','lat','diel_phaseDawn','diel_phaseDay','diel_phaseDusk','diel_phaseNight',
                              'JDay','all.dur','dive_efficiency','ht_rat','hARS_mode',
                              'max.d','pdsi','bottom_time','bear.deg','dist','dive_res')], 
               graph = FALSE, scale.unit = TRUE)
# fviz_pca_var(res.pca, col.var = "black")

# % variance explained by PCAs
fviz_eig(dives.pca, addlabels = TRUE)

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
