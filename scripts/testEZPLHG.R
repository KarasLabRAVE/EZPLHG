library(matrixStats)
data("pt01EcoG")
timeWindow <- c(-30, 20)
epoch <- Epoch(pt01EcoG)
fs=1000
sozIndex <- attr(pt01EcoG, "sozIndex")
windowParams<-c(0.25,0.1)

epoch <- Epoch(pt01EcoG)
visuIEEGData(epoch)

pt01plhg<-calc_PLHG(epoch)

fs=1000
sizeWindow=3000
sizeSkip=333
plhgTimeWindow=c(0,20)
baseTimeWindow=c(-30,-20)
