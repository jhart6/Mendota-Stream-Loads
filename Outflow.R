#Script to develop C load estimates to Lake Mendota from Yahara
#12/12/16

#Build Outflow CSV for 2016 GLM Mendota Simulation
#1: Read in outflow discharge data
#2: Convert units from ft3/s to m3/s
#3: Export CSV in GLM-ready format

#1: Read in outflow discharge data 
#data currently in ft3/s

setwd("~/Dropbox/Mendota Summer 16/GLM Stream Files/") #JAH
stream<-read.csv("Outflow.csv") #JAH

#2: Convert units from ft3/s to m3/s
cf_per_cm = 0.0283168 
Q_m3_s <- stream$DISCHARGE*cf_per_cm

#3: Export CSV in GLM-ready format
outflow<-data.frame(stream$DATETIME,Q_m3_s)
colnames(outflow)<-c('Time','FLOW')

write.csv(outflow,file='Mendota_outflow.csv',row.names=FALSE)
