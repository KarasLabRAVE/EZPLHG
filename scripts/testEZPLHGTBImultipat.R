## 042925
## Run all patients
##############################
library(R.matlab)
library(readxl)
library(parallel)
library(doSNOW)
library(gsignal)
library(pROC)


###########################################
# ---- Analysis script --------------------------------------------------------

pts <- dipsaus::parse_svec("1")
pipeline_xls <- readxl::read_xlsx("/Volumes/bigbrain/ACL/RAVE_Projects/PipelineScripts/TBI_SEEGDataset_pipeline_update_042825.xlsx")

window <- 250
step <- 125
nSearch <-200

pathdataBHI<-'/Volumes/bigbrain/ACL/RAVE_Projects/TBIDatam30sp30s/'
#pathres<-paste('/Volumes/bigbrain/ACL/RAVE_Projects/Fragility/ResEZFragility/',sep='')
pathres<-paste('/Volumes/bigbrain/ACL/RAVE_Projects/TBIStudy/ResEZEI/',sep='')


i=1

  
for(i in pts){
  
  patname <- pipeline_xls$subject[i]
  print(patname)

  ## add channel names to the rows
  goodChannels <- dipsaus::parse_svec(pipeline_xls$good_electrodes[i])
  sozChannels  <- dipsaus::parse_svec(pipeline_xls$soz[i])
  sozExtChannels  <- dipsaus::parse_svec(pipeline_xls$resect[i])
  channelfile<-paste(pathdataBHI,patname,"IctalSEEGChannels.xls",sep="")
  channelNames <- read_excel(channelfile)
  sozIndex<-which(goodChannels%in%sozChannels==TRUE)
  sozNames<-channelNames$name[sozChannels]
  
  sozExtIndex<-which(goodChannels%in%sozExtChannels==TRUE)
  sozExtNames<-channelNames$name[sozExtChannels]
  
  
  fs<-pipeline_xls$sample_rate[i]
  if(class(fs)=='character'){
    fs=as.numeric(fs)
  }
  
  
  ictal_runs <- dipsaus::parse_svec(pipeline_xls$ictal_runs[i])
  #ictal_runs<-1
  for(j in ictal_runs){
  
 #j=1
    
    print(paste("seizure",as.character(j)))
    
  #datafile<-paste(pathdataBHI,patname,"_seizure",as.character(j),"m30sp30s.mat",sep="")
    datafile<-paste(pathdataBHI,patname,"sz",as.character(j),"_m30sp30s.mat",sep="")
    datamat<-readMat(datafile)
    if(fs==1000){
       ptEpochRaw<-datamat$a
    #ptEpochRaw<-datamat$data
    }else{
      dumm<-t(datamat$a)  
      dumm2<-gsignal::resample(dumm,1000,fs)
      elec_resamp<-t(dumm2)
      ptEpochRaw<-elec_resamp
    }
    
    rownames(ptEpochRaw) <- channelNames$name[goodChannels]
    
    ## Add time stamps to the columns
    times <- seq(-30, 30, length.out=ncol(ptEpochRaw))
    times_with_sign <- ifelse(times >= 0, paste0("+", times), as.character(times))
    colnames(ptEpochRaw)<-times_with_sign 
    
    ptEcoGt<-ptEpochRaw
    attr(ptEcoGt, "sozIndex") <- sozIndex
    attr(ptEcoGt, "sozNames") <- sozNames
  
    
    epocht <- Epoch(ptEcoGt)
    windowParams = c(1, 0.2) 
    display<-c(sozExtIndex)
    visuIEEGData(epocht[display,])
    
    
    ptPLHG<-calc_PLHG(epocht)
    
    title <- paste(patname,'seizure',as.character(j))
    plotPLHG<-plotPLHGHeatmap(plhg=ptPLHG,sozIndex=sozExtIndex)
    plotPLHG
    ggplot2::ggsave(paste(pathres,'PLHGHeatmap_',patname,'sz',as.character(j),'_.png',sep=""))
    
    electrodes<-ptPLHG$electrodes
    plhg<-ptPLHG$plhg
    colnames(plhg)<-ptPLHG$startTimes
    voteThres<-ptPLHG$voteThres
    timeDetect<-ptPLHG$sigTime
     
    dfPLHG<-data.frame(electrodes,voteThres,timeDetect,plhg)
     
    write.csv(dfPLHG,paste(pathres,"PLHGResults_",patname,"sz",as.character(j),'.csv',sep="")) 
         
 }

  
  
}

