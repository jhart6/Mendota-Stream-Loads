#Script to develop C load estimates to Lake Mendota from 4 streams:
  #Pheasant Branch, Dorn, Sixmile Creek, Yahara
  #12/2/16
  
#Methods based on Lathrop et al. 1998
  #1: Calculate loads for days where C measurements were collected
    #C concentration * Daily Average Q
    #Generates a data set of C loads
  #2: Build second order polynomial regression model to estimate days w/o discrete C data
    #Predict those C loads calculated in step #1 using daily average Q
    #log(C load) = log(Q) + log(Q)^2
  #3: Use regression model to fill in days with missing C loads

#UNITS by Data Column:
  #DISCHARGE: ft^3/sec - do we need to change this? Or need some conversion?
  #POC: mg/L
  #DOC: mg/L
  #DIC: mg/L

#Data included from 4/1/16 through 11/15/16

#Import data from local drive (CSV's are also available in Github repository)
setwd("~/Dropbox/Mendota Summer 16/GLM Stream Files/") #JAH
stream<-read.csv("Yahara.csv") #JAH

####1: Calculate Loads for Days with Discrete Data####
#create OC variable = POC + DOC (mg/L)
OC<-stream$POC+stream$DOC
stream<-cbind(stream,OC)

#calculate loads
load<-stream$DISCHARGE*stream$OC*0.0864
stream<-cbind(stream,load)

#isolate this load dataset
load_estimate<-as.vector(na.omit(load))
Q_with_load<-as.vector(stream$DISCHARGE[which(is.na(stream$load)==F)])

####2: 2nd Order Polynomial Regression to Estimate Days w/o Discrete Data####
#predictor = Q_with_load
#response = y (load_estimate)

log_Q_with_load = log(Q_with_load)
log_load_estimate = log(load_estimate)

model<-lm(log_load_estimate~poly(log_Q_with_load,2),raw=TRUE)
summary(model)

#check model residuals and normality
plot(resid(model))
shapiro.test(resid(model))

#plotting the model output
plot(log_Q_with_load,log_load_estimate,pch=16)
points(log_Q_with_load,predict(model),col='red',pch=16)
legend('topleft',col=c('black','red'),c('Observed','Modeled'),pch=c(16,16))

####3: Use regression model to fill in days with missing C loads####
log_q<-log(stream$DISCHARGE)

#modeled loads in kg/day using 2nd order polynomial model
modeled_loads<-exp(model$coefficients[1]+model$coefficients[2]*log_q+model$coefficients[3]*(log_q)^2)

#Save output as stream specific datafile
#Pheasant Branch Output
pheasantbranch<-data.frame(stream$DATETIME,stream$DISCHARGE,modeled_loads)
names(pheasantbranch)<-c("DATETIME","DISCHARGE","C_LOAD")
library("xlsx")
write.csv(pheasantbranch,'Pheasant Branch C Loading.csv')

#Dorn Output
dorn<-data.frame(stream$DATETIME,stream$DISCHARGE,modeled_loads)
names(dorn)<-c("DATETIME","DISCHARGE","C_LOAD")
library("xlsx")
write.csv(dorn,'Dorn C Loading.csv')

#Sixmile Output
sixmile<-data.frame(stream$DATETIME,stream$DISCHARGE,modeled_loads)
names(sixmile)<-c("DATETIME","DISCHARGE","C_LOAD")
library("xlsx")
write.csv(sixmile,'Sixmile C Loading.csv')

#Yahara Output
yahara<-data.frame(stream$DATETIME,stream$DISCHARGE,modeled_loads)
names(yahara)<-c("DATETIME","DISCHARGE","C_LOAD")
library("xlsx")
write.csv(yahara,'Yahara C Loading.csv')
