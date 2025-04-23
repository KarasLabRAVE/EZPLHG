library(matrixStats)
data("pt01EcoG")
timeWindow <- c(-30, 20)
epoch <- Epoch(pt01EcoG)
fs=1000
sozIndex <- attr(pt01EcoG, "sozIndex")
windowParams<-c(0.25,0.1)

epoch <- Epoch(pt01EcoG)
visuIEEGData(epoch)

pt01PLHG<-calc_PLHG(epoch)

plotPLHG<-plotPLHGHeatmap(plhg=pt01PLHG,sozIndex=sozIndex)
plotPLHG


fs=1000
sizeWindow=3000
sizeSkip=333
plhgTimeWindow=c(0,20)
baseTimeWindow=c(-30,-20)


####################################################

data("pt01EcoG")

## sozIndex is the index of the electrodes we assume are in the SOZ
sozIndex <- attr(pt01EcoG, "sozIndex")
## precomputed Epileptogenic Index object
data("pt01PLHG")

## plot the mean power heatmap
plotPLHG<-plotPLHGHeatmap(plhg=pt01PLHG,sozIndex=sozIndex)
plotPLHG
