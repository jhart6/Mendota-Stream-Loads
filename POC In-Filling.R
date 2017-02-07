#POC
#load relevant stream data
setwd("~/Dropbox/Mendota Summer 16/GLM Stream Files/")
stream<-read.csv('Pheasant Branch.csv')
stream<-read.csv('Yahara.csv')
stream<-read.csv('Six Mile.csv')
stream<-read.csv('Dorn.csv')

#create load dataset
ConvertLoadToKgDay <- 2.446572
load_kg_day <- stream$DISCHARGE*stream$POC*ConvertLoadToKgDay
stream<- cbind(stream,load_kg_day)

#isolate load dataset
load<-as.vector(na.omit(load_kg_day))
discharge<-as.vector(stream$DISCHARGE[which(is.na(stream$load_kg_day)==F)])

#build predictive model
log_load <- log10(load)
log_discharge <- log10(discharge)

linear_model <- lm(log_load~log_discharge)
summary(linear_model)

plot(resid(linear_model))

plot(log_discharge,log_load,pch=16)
points(log_discharge,predict(linear_model),col='red',pch=16)

#Fill in days with no DIC load data
log_all_discharge <- log10(stream$DISCHARGE)

log_modeled_loads <- linear_model$coefficients[1] + linear_model$coefficients[2]*log_all_discharge
modeled_loads <- 10^log_modeled_loads #in kg/day

#convert units for GLM
cf_per_cm = 0.0283168 
sec_per_day = 86400
carbon_kg_to_mmol = 83263.95

discharge_glm <- stream$DISCHARGE*cf_per_cm #m3/s
discharge_m3_day <- discharge_glm * sec_per_day #m3/day

concentration_mmol_m3 <- (modeled_loads/discharge_m3_day)*carbon_kg_to_mmol

#export new concentrations
df <- data.frame(stream$DATETIME,discharge_glm,concentration_mmol_m3)

library(xlsx)
write.csv(df,file='pheasant_poc.csv',row.names=FALSE)
write.csv(df,file='yahara_poc.csv',row.names=FALSE)
write.csv(df,file='sixmile_poc.csv',row.names=FALSE)
write.csv(df,file='dorn_poc.csv',row.names=FALSE)


