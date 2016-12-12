#Stream C Load Comparison - Mendota Inflows
  #Run stream specific load estimate scripts first to generate CSV's to load into this script

setwd('~/Dropbox/Mendota Summer 16/GLM Stream Files/')
pb<-read.csv("Mendota_pheasant.csv")
dorn<-read.csv("Mendota_dorn.csv")
sixmile<-read.csv("Mendota_sixmile.csv")
yahara<-read.csv("Mendota_yahara.csv")


julian<-seq(92,320,1)
xlab=expression("Julian")
ylab_poc=expression("POC (mmol/m3)")
ylab_doc=expression("DOC (mmol/m3)")

quartz()
par(mar=c(5,5,2,1))
par(mfrow=c(4,2))

plot(julian,pb$OGM_poc,type='l')
plot(julian,pb$OGM_doc,type='l')

plot(julian,dorn$OGM_poc,type='l')
plot(julian,dorn$OGM_doc,type='l')

plot(julian,sixmile$OGM_poc,type='l')
plot(julian,sixmile$OGM_doc,type='l')

plot(julian,yahara$OGM_poc,type='l')
plot(julian,yahara$OGM_doc,type='l')



