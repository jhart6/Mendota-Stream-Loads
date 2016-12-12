#Script to develop C load estimates to Lake Mendota from Yahara
#12/12/16

#Methods based on Lathrop et al. 1998
#1: Calculate loads for days where C measurements were collected
#C concentration * Daily Average Q
#Generates a data set of C loads
#2: Build second order polynomial regression model to estimate days w/o discrete C data
#Predict those C loads calculated in step #1 using daily average Q
#log(C load) = log(Q) + log(Q)^2
#3: Use regression model to fill in days with missing C loads
#4: Convert solute and discharge units to units suitable for GLM
#5: Write new GLM-savvy CSV

#UNITS by Data Column:
#DISCHARGE: ft^3/sec 
#POC: mg/L
#DOC: mg/L
#DIC: mg/L

#Data included from 4/1/16 through 11/15/16

#Import data from local drive (CSV's are also available in Github repository)
setwd("~/Dropbox/Mendota Summer 16/GLM Stream Files/") #JAH
stream<-read.csv("Yahara.csv") #JAH

####1: Calculate Loads for Days with Discrete Data####
#calculate loads
load<-stream$DISCHARGE*stream$POC*0.0864
load<-stream$DISCHARGE*stream$DOC*0.0864
stream<-cbind(stream,load)

#isolate this load dataset
load_estimate<-as.vector(na.omit(load))
Q_with_load<-as.vector(stream$DISCHARGE[which(is.na(stream$load)==F)])

####2: 2nd Order Polynomial Regression to Estimate Days w/o Discrete Data####
#predictor = Q_with_load
#response = y (load_estimate)

log_Q_with_load = log(Q_with_load)
log_load_estimate = log(load_estimate)

poly_model<-lm(log_load_estimate~poly(log_Q_with_load,2),raw=TRUE)
summary(poly_model)

linear_model<-lm(log_load_estimate~log_Q_with_load)
summary(linear_model)

#check model residuals and normality
plot(resid(poly_model))
shapiro.test(resid(poly_model)) 

plot(resid(linear_model))
shapiro.test(resid(linear_model)) 

#compare the two models
mod_compare<-anova(poly_model,linear_model)
summary(mod_compare)

AIC(poly_model)
AIC(linear_model) 

#yahara[poc] = linear model
#yahara[doc] = linear model

#plotting the model output
plot(log_Q_with_load,log_load_estimate,pch=16)
points(log_Q_with_load,predict(linear_model),col='red',pch=16)
points(log_Q_with_load,predict(poly_model),col='red',pch=16)
legend('topleft',col=c('black','red'),c('Observed','Modeled'),pch=c(16,16))

####3: Use regression model to fill in days with missing C loads####
log_q<-log(stream$DISCHARGE)

#modeled loads in kg/day using linear model
modeled_loads_linear<-exp(linear_model$coefficients[1]+linear_model$coefficients[2]*log_q)

#modeled loads in kg/day using 2nd order polynomial model
modeled_loads_poly<-exp(poly_model$coefficients[1]+poly_model$coefficients[2]*log_q+poly_model$coefficients[3]*(log_q)^2)

#Creating solute specific vector names for posterity
yahara_poc_kg_day<-modeled_loads_linear
yahara_doc_kg_day<-modeled_loads_linear


####4: Convert Q and Solute Units for GLM####
cf_per_cm = 0.0283168 
sec_per_day = 86400
carbon_kg_to_mmol = 83263.95

#poc
Q_m3_s<-stream$DISCHARGE*cf_per_cm
Q_m3_day<-Q_m3_s*sec_per_day
poc_load_over_Q<-(yahara_poc_kg_day/Q_m3_day)
yahara_poc_mmol<-(poc_load_over_Q*carbon_kg_to_mmol)

#doc
Q_m3_s<-stream$DISCHARGE*cf_per_cm
Q_m3_day<-Q_m3_s*sec_per_day
doc_load_over_Q<-(yahara_doc_kg_day/Q_m3_day)
yahara_doc_mmol<-(doc_load_over_Q*carbon_kg_to_mmol)

####5: Export GLM Suitable CSV####
yahara<-data.frame(stream$DATETIME,stream$TEMP,Q_m3_s,yahara_poc_mmol,yahara_doc_mmol)
colnames(yahara)<-c("Time","TEMP","FLOW","OGM_poc","OGM_doc")

library("xlxs")
write.csv(yahara,file='Mendota_yahara.csv',row.names=FALSE)
