#' Computes the Phase Locked High Gamma matrix for ictal iEEG
#' Implementation of PLHG as described in:
# Ictal high frequency oscillations distinguish two types of seizure territories in humans
# Shennan A. Weiss, Garrett P. Banks, Guy M. McKhann, Jr, Robert R. Goodman, Ronald G. Emerson, Andrew J. Trevelyan, and Catherine A. Schevon
# Brain. 2013 Dec; 136(12): 3796?3808.
#'
#' @param ieegts Numeric. A matrix of iEEG time series x(t),
#' with time points as rows and electrodes names as columns
#' @param sizeWindow Integer. The number of time points to use in each window
#' @param sizeSkip Integer.The number of time points to move the window each time
#' @param fs Numeric. Acquisition frequency
#' @param tBaseline Numeric. baseline duration in seconds
#'
#' @return list
#' plhgMaster. Numeric. matrix of PLHG values. row number of time window. column electrode number
#' timeVals. Integer. List of window start
#'
#' @export
#'
#' @examples
#' data("PT01Epochm30sp30s")
#' sizeWindow <- 3000
#' sizeSkip <- 333
#' fs=1000
#' tBaseline=20
#' time_window_ictal=c(-10,10)
#' time_window=c(-30,30)
#' resPLHG<-calc_PLHG(ieegts = PT01Epochm30sp30s, sizeWindow=sizeWindow, sizeSkip=sizeSkip,fs=fs,tBaseline=tBaseline,time_window=time_window,time_window_ictal=time_window_ictal)
calc_PLHG<- function(ieegts,sizeWindow,sizeSkip,fs,tBaseline,time_window_ictal,time_window) {

  np<-reticulate::import('numpy')

  ## Number of electrodes and time points
  n_tps <- nrow(ieegts)
  n_elec <- ncol(ieegts)

  nyquist <- fs/2
  transitionWidth <- 0.1


  electrode_list <- colnames(ieegts)

  # number of windows for PLHG analysis
  nw<-floor(n_tps-sizeWindow)/sizeSkip

  #
  ntb=tBaseline*fs
  #tsBaseline=matrix(0,ntb,n_elec)

  tsBaseline=data.matrix(ieegts[1:ntb,1:n_elec], rownames.force=NA)

  #Epoch Ictal signal
  its=as.integer((time_window_ictal[1]-time_window[1])*fs)
  nt=as.integer((time_window_ictal[2]-time_window_ictal[1])*fs)
  ite=its+nt

  dt <- as.numeric(1/fs)

  ts <- 1:nt
  ts <- ts*dt+time_window_ictal[1]
  tsIctal<-data.matrix(ieegts[its:ite,1:n_elec], rownames.force=NA)

  # number of windows for PLHG analysis
  nw<-floor((nt-sizeWindow)/sizeSkip)


  stepsBuffer=matrix(0,sizeWindow,nw)

  for(ii in 1:nw){
    stepsBuffer[1:sizeWindow,ii]<-(ii-1)*sizeSkip+1:sizeWindow
  }
  for(ii in 1:sizeWindow){
    if(stepsBuffer[ii,nw]>nt) stepsBuffer[ii,nw]<-0
  }

  #check<-stepsBuffer[,nw]
  timeVals<-stepsBuffer[1,]

  PLVMaster<-matrix(0,nw,n_elec)

  ##########################################
  # Filter signal low frequency

  filterwindow<-c(4,30)
  fvec   <- vector(mode="numeric", length=6)
  fvec[1] <- 0.0
  fvec[2] <- (1 - transitionWidth) * filterwindow[1]/nyquist
  fvec[3] <- filterwindow[1]/nyquist
  fvec[4] <- filterwindow[2]/nyquist
  fvec[5] <- (1 + transitionWidth) * filterwindow[2]/nyquist
  fvec[6] <- 1.0
  idealresponse<-c(0, 0, 1, 1, 0, 0)

 # sprintf(" Filter Data in low Frequency Band 4-30 Hz")
  # build firls filter
  fir_4_30<-gsignal::firls(499,fvec,idealresponse)
  filter_4_30 <- gsignal::filtfilt(fir_4_30,tsIctal)
  hilbert_4_30<-gsignal::hilbert(filter_4_30)
  phi_4_30<-np$angle(hilbert_4_30)

  ##########################################
  # Filter signal high gamma

  filterwindow<-c(80,150)
  fvec   <- vector(mode="numeric", length=6)
  fvec[1] <- 0.0
  fvec[2] <- (1 - transitionWidth) * filterwindow[1]/nyquist
  fvec[3] <- filterwindow[1]/nyquist
  fvec[4] <- filterwindow[2]/nyquist
  fvec[5] <- (1 + transitionWidth) * filterwindow[2]/nyquist
  fvec[6] <- 1.0
  idealresponse<-c(0, 0, 1, 1, 0, 0)

#  sprintf(" Filter Data in Frequency Band 80-150 Hz")

  # build firls filter
  fir_80_150<-gsignal::firls(499,fvec,idealresponse)
  filter_80_150 <- gsignal::filtfilt(fir_80_150,tsIctal)
  hilbert_80_150<-gsignal::hilbert(filter_80_150)

  a_80_150<-abs(hilbert_80_150)
  hilbert_a_80_150<-gsignal::hilbert(a_80_150)
  phi_a_80_150<-np$angle(hilbert_a_80_150)


  ##########################################
  # Filter pre seizure baseline

#  sprintf(" Filter pre seizure baseline in Frequency Band 80-150 Hz")
  filter_80_150_baseline <- gsignal::filtfilt(fir_80_150,tsBaseline)
  hilbert_80_150_baseline<-gsignal::hilbert(filter_80_150_baseline)
  a_80_150_baseline<-abs(hilbert_80_150_baseline)

  a_80_150_baseline<-colMeans(a_80_150_baseline)


  plhgMaster<-matrix(0,nw,n_elec)


  for(jj in 1:nw){
    #jj<-1
    #print(jj)
    currentTime<-stepsBuffer[,jj]
    phi_4_30_jj<-phi_4_30[currentTime,]
    phi_a_80_150_jj<-phi_a_80_150[currentTime,]

    PLV<-abs(colMeans(exp(1i*(phi_4_30_jj-phi_a_80_150_jj))))

    a_80_150_jj<-a_80_150[currentTime,]
    a_80_150_norm_jj<-colMeans(a_80_150_jj)/a_80_150_baseline
    PLHG=a_80_150_norm_jj*PLV

    PLVMaster[jj,]<-PLV
    plhgMaster[jj,]<-PLHG
  }

  namesElectrodes<-colnames(ieegts)

  colnames(plhgMaster)<-namesElectrodes

  stimes<-c(1:nw)/nw*(time_window_ictal[2]-time_window_ictal[1])+time_window_ictal[1]

  rownames(plhgMaster)<-stimes

  return(list(
    plhgMaster=plhgMaster,
    timeVals=timeVals
    ))



}

#' Ictal core recruitment
#'
#' Electrode classification in Ictal core derived from
#' PLHG values increasing to 2.5SD over the mean
#'
#' @param resPLHG PLHG values
#' @param fs signal acquisition frequency
#' @param ElectrodesData Electrodes data names, soz/non soz epileptologist marking
#' @param time_window_ictal time window around seizure onset
#'
#' @return electrode classification with PLHG values increasing to 2.5SD over the mean
#' @export
#'
#' @examples
#' data("ElectrodesDataPT01")
#' data("resPLHG")
#' ElectrodesData=ElectrodesDataPT01
#' time_window_ictal=c(-10,10)
#' subject_code='PT01'
#' j=1
#' fs=1000
#' resvotethres<-votethres(resPLHG=resPLHG,fs=fs,ElectrodesData=ElectrodesData,time_window_ictal=time_window_ictal)
votethres<-function(resPLHG,fs,ElectrodesData,time_window_ictal){

  plhgMaster<-resPLHG[[1]]
  timeVals<-resPLHG[[2]]
  maxVal<-apply(plhgMaster,2,max)
  mu<-mean(maxVal)
  sigma<-sd(maxVal)
  coeffVar<-sigma/mu
  mu=mean(plhgMaster)
  sigma<-sd(plhgMaster)
  sigThres<-mu+sigma*2.5
  sigLeads<-maxVal>sigThres

  nel<-ncol(plhgMaster)

  sigTime<-matrix(NaN,1,nel)

  votethres   <- vector(mode="numeric", length=nel)

  for(jj in 1:nel){
    if(sigLeads[jj]==TRUE){
      votethres[jj]=1
    }
    currentInd<-which(plhgMaster[,jj]>=sigThres)
    if(length(currentInd)>0){
      #print(jj)
      sigTime[1,jj]=timeVals[currentInd[1]]/fs
    }
  }

  elecsoz=which(ElectrodesData$insoz==TRUE)
  elecsozc=which(ElectrodesData$insoz==FALSE)
  elecsozsozc=c(elecsoz,elecsozc)

  votethres<-votethres[elecsozsozc]
  sigTime[1,]<-sigTime[1,elecsozsozc]+time_window_ictal[1]

  resvotethres<-data.frame(elecsozsozc,ElectrodesData$nameselec[elecsozsozc],votethres,sigTime[1,])
  return(resvotethres)

}
