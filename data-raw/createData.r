
## load pt01epochdata.mat
## Patient PT01 from the Fragility data set

library(R.matlab)
library(readxl)
data <- readMat('data-raw/subpt01sz1_m30sp30s.mat')
pt01EpochRaw <- data$a

## add channel names to the rows
goodChannels <- c(1:4,7:24,26:36,42:43,46:54,56:69,72:95)
sozChannels<-c(33:34,62:69)
channelNames <- read_excel('data-raw/Pt01ictalRun01EcoGChannels.xls')
rownames(pt01EpochRaw) <- channelNames$name[goodChannels]

sozIndex<-which(goodChannels%in%sozChannels==TRUE)
sozNames<-channelNames$name[sozChannels]

## Add time stamps to the columns
times <- seq(-30, 30, length.out=ncol(pt01EpochRaw))
times_with_sign <- ifelse(times >= 0, paste0("+", times), as.character(times))
colnames(pt01EpochRaw)<-times_with_sign

display <- c(sozIndex, 75:82)
pt01EcoG<-pt01EpochRaw[display,1:50001]
sozIndex<-c(1:10)
attr(pt01EcoG, "sozIndex") <- sozIndex
attr(pt01EcoG, "sozNames") <- sozNames
usethis::use_data(pt01EcoG, overwrite = TRUE)

epoch <- Epoch(pt01EcoG)
pt01PLHG<-calc_PLHG(epoch)

usethis::use_data(pt01PLHG, overwrite = TRUE)






