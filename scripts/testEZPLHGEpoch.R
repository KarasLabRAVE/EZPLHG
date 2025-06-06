# test EZPLHG with Epoch package
# data download one patient one seizure
dl <- EpochDownloader()
pt01sz1<-dl$Retrostudy_subpt01_1

#pt01sz1ieeg<-tblData(pt01sz1)

# crop
pt01sz1m30p20s<-crop(pt01sz1, start=-30, end=20)
sozIndex<-which(pt01sz1m30p20s@rowData$soz==TRUE)

display <- c(sozIndex, 77:80)
# subset on 14 electrodes
pt01sz1m30p20s14e<-pt01sz1m30p20s[display,]
sozIndex14e<-which(pt01sz1m30p20s14e@rowData$soz==TRUE)

fspat=1000

pt01PLHG<-calc_PLHG(epoch=pt01sz1m30p20s14e, fs=fspat)


plotPLHG<-plotPLHGHeatmap(plhg=pt01PLHG,sozIndex=sozIndex14e)
plotPLHG
