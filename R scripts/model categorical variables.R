library('nnet') # Neural networks - too complex and not easy to infer results
library('mgcv') # gams - good for my data where relationships are not linear 
library('lme4') # linear models - not good for my data, too simple
library('broom')
library('caret') # for predictive modelling - train(x,y,method='gam')
library('gratia')
library('gamlss')
library('voxel')
library('mgcViz')
library('gridBase')
library('grid')
library('xts')
library('vroom')
library("DHARMa")
library("AICcmodavg")
install.packages('forecats')
?smooth.terms
?family.mgcv
?gam.selection
?gam.models # hierarchical and mixed effects models (i.e. random effects)
# Saving data

rm(list = ls())

save_all = 'Plots & Dive Tables/'
saveRDS(summer_dives,file.path(save_all,"summer_dives.rds"))
saveRDS(winter_dives,file.path(save_all,"winter_dives.rds"))
saveRDS(final_df,file.path(save_all,"final_df.rds"))
final_df <- readRDS(file.path(save_all,"final_df.rds"))

# Reading data
save_all = 'Plots & Dive Tables/'
summer_dives = readRDS(file.path(save_all,"summer_dives.rds"))
winter_dives = readRDS(file.path(save_all,"winter_dives.rds"))
mydata<-summer_dives
mydata<-winter_dives


######
Perform VIF for predictor variables for gams as well i.e. 'Nt', Thermocline and mean_Temp !!!!!!!!!!!!!!!!!!!
######

# set.seed(123)
# 
# ind<-sample(2,NROW(mydata),replace = TRUE,prob = c(0.6,0.4))
# training<-mydata[ind==1,]
# testing<-mydata[ind==2,]
# 
# training$divetype<- relevel(training$divetype,ref='1')
# mymodel<- multinom(divetype~mean_Temp, data= training)
# summary(mymodel)
# residuals(mymodel)
# plot(residuals(mymodel))

# gam vs gamm models
# gam = fits generalized additive model
# gamm = fits generalized additive mixed-model. Splines. Flexibility makes it easy to overfit (fitting data to noise) data => smoothing. The linear function is replaced by a smooth function. Number of bases functions (k) is determined by REML. The more the better as then more detail in patterns is captured. Too low - prevents model from being sufficiently wiggly. Too high = more parameters than data and computationally slow.
# gamm performs poorly with binary data, since it uses PQL. It is better to use gam with s(...,bs="re") terms, or gamm4.
# gamm is not as numerically stable as gam and is usually much slower






# GAMSS Model -------------------------------------------------------------
season = 'winter'
model_name = 'GAM_NtbyTherm_meanT_Therm_k10'
file_name = 'GAM_NtbyTherm_meanT_Therm_k10.rds'
####### Final model #######
my_gam_model <- gamm(divetype~s(abs_Nt, by = Thermocline, k=5)+s(mean_Temp, k=5) + Thermocline, random = list(sealID=~1), data=final_df %>% filter(season==season), family = binomial, method = 'REML')
# Number of bases functions (k) is determined by REML
saveRDS(my_gam_model,file.path('Plots & Dive Tables/All seals/GAMS & GAMMS',season,model_name,file_name))
my_gam_model = readRDS(file.path('Plots & Dive Tables/All seals/GAMS & GAMMS',season,model_name,file_name))

# Interpreting results ----------------------------------------------------

# for GAM only
summary(my_gam_model$gam) # gam style fitted model
summary(my_gam_model$lme) # details of underlying lme fit

# Plotting output ---------------------------------------------------------
# Plot with shift. Because you've used logistic gams to get a probability between 0 and 1 you must log the data back to original using trans=plogis.
plot(my_gam_model$gam,trans=plogis, pages=1, shift=coef(my_gam_model$gam)[1],seWithMean=TRUE, rug=TRUE, shade=TRUE,shade.col='lightgrey',col='black', cex.lab=1.4) 
#shift - to incorporate model intercept #lightgreen (shifts the scale by the value of the intercept)
#seWithMean - includes the uncertainty of the model (standard errors)

plot(my_gam_model$lme) #plots residuals
##### Residuals 
# plot(my_gam_model$gam, residuals = TRUE, pch = 1)
# hist(my_gam_model$gam$residuals)
# # plot(residuals(my_gam_model$gam))
# qqnorm(my_gam_model$gam$residuals)
# qqline(my_gam_model$gam$residuals)

##### Other plots
#(p)acf plots - looking for autocorellation
acf(resid(my_gam_model$lme, type = "normalized"))
pacf(resid(my_gam_model$lme, type = "normalized"))

vis.gam(my_gam_model$gam,view = c('Thermocline','mean_Temp'),se=2,phi=20,type = 'response',plot.type = 'persp',r=0)







# GAMS --------------------------------------------------------------------
final_df$mean_Temp_s <- base::scale(final_df[,c('mean_Temp')], center = TRUE, scale = TRUE) 
# final_df$Nt_s <- base::scale(final_df[,c('abs_Nt')], center = TRUE, scale = TRUE) 

# # Scale then log
# final_df$log_Nt <- final_df$abs_Nt*1e-5
# final_df[final_df$log_Nt<1e-7,"log_Nt"] = 1e-7
# final_df$log_Nt <- base::scale(final_df[,c('log_Nt')], center = TRUE, scale = TRUE) 
# final_df[final_df$log_Nt<=0,"log_Nt"] <- final_df %>% filter(log_Nt>0) %>% dplyr::select(log_Nt) %>% min()
# final_df$log_Nt <- log(final_df$log_Nt)
# plot((final_df %>% filter(season=='summer'))$log_Nt)
# 
# # Just scale and center
# final_df$log_Nt <- base::scale(final_df[,c('abs_Nt')], center = TRUE, scale = TRUE) 
# plot((final_df %>% filter(season=='summer'))$log_Nt)

# log then scale
df <- final_df %>% filter(season=='summer')
df$log_Nt <- df$abs_Nt*1e-5
df[df$log_Nt<5e-7,"log_Nt"] = 5e-7 #(Worse AIC than 1e-7, but more interesting for interpretation )
df$log_Nt <- log(df$log_Nt)
df$log_Nt <- base::scale(df[,c('log_Nt')], center = TRUE, scale = TRUE) 
plot(df$log_Nt)

km=10
kn=10

my_gam_model <- gam(divetype~ Thermocline+s(mean_Temp_s,k=km)+s(log_Nt,by=Thermocline,k=kn)+s(sealID,bs='re'), data=df, family = binomial(link = 'logit'), method = 'REML') # binomial: ‘logit’, ‘probit’, ‘cloglog’, ‘cauchit’, ‘log’  | s(mean_Temp_s,k=km)   s(log_Nt,by=Thermocline,k=kn)
# my_gam_model1 <- gam(divetype~Thermocline+s(mean_Temp_s,k=km)+s(log_Nt,by=Thermocline,k=kn)+s(sealID,bs='re'), data=final_df %>% filter(season=='summer'), family = binomial(link = 'logit'), method = 'REML')
# by default gam uses GCV for smoothing parameter selection. GCV prone to overfitting (Wood, 2011), also problematic w. correlated covariates (Wood, 2006) => REML better
# Model checking:
summary(my_gam_model)
augment(my_gam_model)
tidy(my_gam_model)
glance(my_gam_model)
# Model comparisons of candidates
AIC(my_gam_model)
AICc(my_gam_model)
BIC(my_gam_model)
# anova(my_gam_model1)
# concurvity(my_gam_model1)

saveRDS(my_gam_model,file.path('Plots & Dive Tables/All seals/GAMS & GAMMS/my_gam_model_summer.rds'))
my_gam_model = readRDS(file.path('Plots & Dive Tables/All seals/GAMS & GAMMS/my_gam_model_summer.rds'))


gam.check(my_gam_model) # simple checking plots

# Plots 
plot(my_gam_model,trans=plogis,pages=1,shift = coef(my_gam_model)[1],seWithMean=T,rug=T,shade=T,scheme=1,shade.col='lightgrey',col='black',pch = 19,cex.lab=1.4,ylim=c(0,1)) #, select = 2, shift=coef(my_gam_model)[1], ,rug=T
plot(my_gam_model,trans=plogis,select=1,shift = coef(my_gam_model)[1],seWithMean=T,shade=T,scheme=1,shade.col='lightgrey',col='black',pch = 19,cex.lab=1.6,cex.axis=1.5,xlab=expression('sc('*'Mean Temperature ['(degree~C)*'])'), lwd=2,ylim=c(0,1))  #,trans=plogis, 
abline(h = plogis(coef(my_gam_model)[1]), lty='dashed', lwd=0.4)
abline(v = 0, lty='dashed', lwd=0.4)
p1<-plot(my_gam_model,trans=plogis,select=2,shift = coef(my_gam_model)[1],seWithMean=T,shade=T,scheme=1,shade.col='lightgrey',col='black',pch = 19,cex.lab=1.6,cex.axis=1.5,xlab=expression('sc('*'log[Stability(sec'^-2*')])'), lwd=2,ylim=c(0,1))
abline(h = plogis(coef(my_gam_model)[1]), lty='dashed', lwd=0.4)
abline(v = 0, lty='dashed', lwd=0.4)
p2<-plot(my_gam_model,trans=plogis,select=3,shift = coef(my_gam_model)[1],seWithMean=T,shade=T,scheme=1,shade.col='lightgrey',col='black',pch = 19,cex.lab=1.6,cex.axis=1.5,xlab=expression('sc('*'log[Stability(sec'^-2*')])'), lwd=2,ylim=c(0,1))
abline(h = plogis(coef(my_gam_model)[1]), lty='dashed', lwd=0.4)
abline(v = 0, lty='dashed', lwd=0.4)
plot(my_gam_model,trans=plogis,shift = coef(my_gam_model)[1],select=4,seWithMean=T,shade=T,scheme=1,shade.col='lightgrey',col='black',pch = 19,cex.lab=1.6,cex.axis=1.5,ylim=c(0,1),xlab='Quassian Quantiles')
#plotGAM(my_gam_model,smooth.cov = 'mean_Temp', groupCovs = "Thermocline")
# plot(effect('Thermoclinepresent', my_gam_model1))

# Presenting coefficients - beta values - model estimates
# exp(beta-values) # because you log transformed values 
coeff.model <- tidy(my_gam_model$coefficients)
coeff.model_Nt <- coeff.model %>% filter(str_detect(names, 'Nt')) %>% dplyr::select(x)
coeff.model_ <- coeff.model %>% filter(str_detect(names, 'mean_Temp_s')) %>% dplyr::select(x)

convert_back(final_df$mean_Temp_s,coeff.model_,'mean_Temp')
(convert_back(final_df$log_Nt,coeff.model_Nt,'log_Nt') %>% exp())


b <- getViz(my_gam_model)
s1 <- sm(b, 2)
# plot(s1)
s2 <- sm(b, 3)
plotDiff.mgcv.smooth.1D(s1, s2, unconditional = TRUE) + l_ciPoly(level = 0.95, mul = NULL) + l_fitLine(lwd=1) + xlab(expression('log[Stability(sec'^-2*')]')) + ylab('Mixed-Stratified') + theme_bw(base_size = 20) + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank()) + geom_hline(yintercept = 0, lty='dashed', lwd=0.4) + geom_vline(xintercept = 0, lty='dashed', lwd=0.4) 

check(b,type = "pearson", a.hist = list(bins=100),a.respoi = list(size=1), a.qq = list(showReps = TRUE, CI = "none"),k.rep = 1000,k.sample=10000)
mgcv:::k.check(b, subsample = 1000, n.rep = 1000)

simulationOutput <- simulateResiduals(fittedModel = my_gam_model)
plot(simulationOutput)


# check1D(b, "mean_Temp") + l_gridQCheck1D(qu = 0.5) + xlab('Seal ID') + ylab('Proportion of neg.resid') + theme_bw(base_size = 20)
# #print(plot(b, allTerms = T), pages = 1) # Plots everything on one page
# o <- plot( sm(b, 2) )
# o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
#   l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
#   l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
# 
# b <- getViz(my_gam_model)
# o = sm(b,3)
# o$gObj$model

my_gam_model1$smooth[[2]]$Xu

# abline(h=0,lty=2,col="grey")

## Odds ratio - e.g. x1=1.79,x2=0.25 - For a unit increase in x1 we expect a 79% increase in the odds of divetype 2.  
exp(coef(my_gam_model))

vis.gam(my_gam_model)
plot.gam(my_gam_model)
qq.gam(my_gam_model)
confint.default(my_gam_model)
fderiv(my_gam_model)
+ geom_hline(yintercept = 0, linetype = 2)
## In the generalized case the 'gam' object is based on the working
## model used in the PQL fitting. Residuals for this are not
## that useful on their own as the following illustrates...

gam.check(my_gam_model) 

## But more useful residuals are easy to produce on a model
## by model basis. For example...

fv <- exp(fitted(my_gam_model$lme)) ## predicted values (including re)
rsd <- (my_gam_model$gam$y - fv)/sqrt(fv) ## Pearson residuals (Poisson case)
op <- par(mfrow=c(1,2))
qqnorm(rsd);plot(fv^.5,rsd)
par(op)

residuals_ = resid(my_gam_model1, type='pearson')
Overdispersion = sum(residuals_^2)/my_gam_model1$df.residual

plot(x = fitted(my_gam_model, type = 'response'), y = residuals_, xlab = 'Fitted values', ylab = 'Pearson residuals')
abline(0, 0)
# Alternative models ------------------------------------------------------

# Single overall interaction term - therefore not good for distinguishing between categories of factor variable
# Factor smoothes are good for accounting for categorical variables that are not our main variable of interest 
# Tensor smooths are appropriate for interactions between variables with incomparable units (eg. distance and elevation (one is on a larger scale than the other, or concentration and time))
# 2d smoothes for when the variables are interacting. 
# or use tensor smooths when the interacting variables are not scaled. 2 smoothing parameters - one for each parameter, therefore different number of k values (basis functions)
# tensor interactions allows us to moderate the interactions effect independently from inependent univariate effects
# Can evaluate the significance of different components 
my_gam_model <- gamm(divetype~s(abs(Nt))+s(mean_Temp)+Thermocline, random = list(sealID=~1), data=summer_dives, family = binomial, method = 'REML') 
saveRDS(my_gam_model,file.path('Models/',"GAM_Nt_meanT_Therm.rds"))
my_gam_model = readRDS(file.path('Models/',"GAM_Nt_meanT_Therm.rds"))
coef(my_gam_model)
# termplot() visualisation of model
# Logistic function converts numbers of any value to a number between zero and one. LOGIT function is the inverse function 
##### GAM
# Fit the GAM
gam_mod_sk <- gam(accel~s(times, k=50), data = mcycle,sp=0.0001)
my_gam_model_s <- gam(divetype~s(Nt, by = Thermocline)+s(mean_Temp, by = Thermocline) + Thermocline, random = list(sealID=~1), data=summer_dives, family = binomial, method = 'REML')
my_gam_model_s <- gam(divetype~s(Nt)+s(mean_Temp)+ti(Nt,mean_Temp)+Thermocline, random = list(sealID=~1), data=summer_dives, family = binomial, method = 'REML')
# plogis(intercept) predicts how much % of positive outcome at baseline (i.e. if fixed variables are at their average values)
# What does the log_mod model estimate the probability of a person making a purchase who has mean values of all variables?

#Visualize the model
plot(my_gam_model$gam, residuals = TRUE, pch = 1)
# Plot the model on one page
plot(my_gam_model$lme, pages = 1)
AIC(my_gam_model$lme)
coef(my_gam_model$gam)
# 2D modewlling with gams
summary(my_gam_model$gam)
summary(my_gam_model$lme)

vis.gam(my_gam_model$gam,view = c('Thermocline','mean_Temp'),se=2,phi=20,type = 'response',plot.type = 'persp',r=0)


####### Winter #######
my_gam_model_w <- gamm(divetype~s(Nt)+s(mean_Temp), random = list(sealID=~1), data=winter_dives, family = binomial, method = 'REML') 







# Logistic gams

# my_gam_model_s <- gam(divetype~s(Nt,by=Thermocline)+s(mean_Temp,by=Thermocline)+ti(Nt,mean_Temp)+Thermocline, random = list(sealID=~1), data=mydata, family = binomial, method = 'REML')
# my_gam_model_s <- gam(divetype~s(Nt,by=Thermocline)+s(mean_Temp,by=Thermocline)+ti(Nt,mean_Temp)+Thermocline +s(sealID,bs='re'), data=mydata, family = binomial, method = 'REML')

## overall effect of Thermocline
# binomial, sealID as random effect. (s(Nt) - global or average smooth effect, whereas s(Nt, by=Thermocline) - models group-specific smooths)
m1 <- gam(divetype~s(Nt)+s(mean_Temp)+ti(Nt,mean_Temp)+Thermocline +s(sealID,bs='re'), data=mydata, family = binomial, method = 'REML')
summary(m1)

# binomial, sealID as random effect on random factor-smooth interaction basis.
m2 <- gam(divetype~s(Nt)+s(mean_Temp)+ti(Nt,mean_Temp)+Thermocline+s(mean_Temp,sealID,bs='fs')+s(Nt,sealID,bs='fs'), data=mydata, family = binomial, method = 'REML')
summary(m2)

## Modelling Thermocline specific curves/group-specific smooths::
#binomial, sealID as random effect, number of base functions set to 20
m3 <- gam(divetype~Thermocline+s(mean_Temp, by=Thermocline, k=20)+s(Nt, by=Thermocline, k=20)+
                      ti(Nt,mean_Temp)+s(sealID,bs='re'), data=mydata, family = binomial, method = 'REML')
summary(m3)

#Number of base functions set to 8
m4 <- gam(divetype~Thermocline+s(mean_Temp, by=Thermocline, k=8)+s(Nt, by=Thermocline, k=8)+
            ti(Nt,mean_Temp)+s(sealID,bs='re'), data=mydata, family = binomial, method = 'REML')
summary(m4)

#binomial, sealID as random effect, m=1(first derivative). Penalty here is on the first derivative (m = 1) for this difference smoother, it is penalising departure from a flat line, which when added to the global or average smooth term (s()`) reflects a deviation from the global or average effect.
m5 <- gam(divetype~Thermocline+s(mean_Temp, by=Thermocline, m=1)+s(Nt, by=Thermocline, m=1)+
            ti(Nt,mean_Temp)+s(sealID,bs='re'), data=mydata, family = binomial, method = 'REML')
summary(m5)



## Modeling global smooths and Thermocline specific smooths::
# m7 <- gam(divetype~Thermocline+s(mean_Temp, by=Thermocline, k=20)+s(mean_Temp)+s(Nt, by=Thermocline, k=20)+s(Nt)+s(mean_Temp,sealID,bs='fs')+s(Nt,sealID,bs='fs')+s(sealID,bs='re'), data=mydata, family = binomial, method = 'REML')
# summary(m7)

AIC(m1, m2, m3, m4, m5, m6) # less than 50000 is respectable..

# Plot with shift. Because you've used logistic gams to get a probability between 0 and 1 you must log the data back to original using trans=plogis.
plot(my_gam_model_s$gam,trans=plogis, pages=1, shift=coef(my_gam_model_s$gam)[1],seWithMean=TRUE, rug=TRUE, shade=TRUE,shade.col='lightgrey',col='purple', cex.lab=1.4) #shidt - to incorporate model intercept #lightgreen
title('gam(divetype~Thermocline+s(mean_Temp, by=Thermocline, k=20)+s(mean_Temp)+s(Nt, by=Thermocline, k=20)+s(Nt)+s(mean_Temp,sealID,bs='"fs"')+s(Nt,sealID,bs='"fs"')+s(sealID,bs='"re"'), data=mydata, family = binomial, method = '"REML"')',cex.main= 0.85)
# partial effects plot shows probability of outcome at their own average values
# seWithMean - adds intercept uncertainty to smooth uncertainty
plot(my_gam_model$lme, pages=1, rug=FALSE, shade=TRUE,shade.col='lightgreen',col='purple') #shift - to incorporate model intercept


augment(my_gam_model_s)
tidy(my_gam_model_s)
glance(my_gam_model_s)



plot(mydata$Nt,mydata$mean_Temp)
title('summer dives')


# Bruto mda  --------------------------------------------------------------


# Linear model ------------------------------------------------------------

my_gam_model1 <- gam(divetype~Thermocline+mean_Temp_s+log_Nt+s(sealID,bs='re'), data=final_df %>% filter(season=='summer'), family = binomial(link = 'logit'), method = 'REML')

my_lm_model <- glmer(divetype~Thermocline+mean_Temp+abs_Nt+(1|sealID),data=final_df %>% filter(season=='summer'), family = binomial(link = 'logit'))

my_lm_model1 <- glmer(divetype~Thermocline+mean_Temp_s+log_Nt+(1|sealID),data=final_df %>% filter(season=='summer'), family = binomial(link = 'logit'))

summary(my_gam_model1)
AIC(my_lm_model1)
BIC(my_lm_model1)

augment(my_lm_model)
tidy(my_lm_model)
glance(my_lm_model)



# predict -----------------------------------------------------------------

m1_pred <- bind_cols(new_data,
                     as.data.frame(predict(m1_gam, newdata = new_data,
                                           se.fit = TRUE)))


# linear model for divetype categories ------------------------------------

my_lm_model <- glmer(divetype~ht_rat+max.d+dive_efficiency+dive_res+X.bt.dt+(1|sealID),data=final_df %>% filter(season=='summer'), family = binomial(link = 'logit'))
# Model does not coverge

