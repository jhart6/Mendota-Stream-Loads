#Stream C Load Comparison - Mendota Inflows
  #Run C load estimates script first to generate stream specific dataframes

View(pheasantbranch)
View(dorn)
View(sixmile)
View(yahara)

julian<-seq(92,320,1)
xlab=expression("Julian")
ylab=expression("OC Load (kg/day)")

quartz()
par(mar=c(5,5,2,1))
par(mfrow=c(2,2))
plot(julian,pheasantbranch$C_LOAD,type='l',xlab=xlab,ylab=ylab,main=expression("Pheasant Branch"))
plot(julian,dorn$C_LOAD,type='l',col='red',xlab=xlab,ylab=ylab,main=expression("Dorn"))
plot(julian,sixmile$C_LOAD,type='l',col='green',xlab=xlab,ylab=ylab,main=expression("Six Mile"))
plot(julian,yahara$C_LOAD,type='l',col='blue',xlab=xlab,ylab=ylab,main=expression("Yahara"))





