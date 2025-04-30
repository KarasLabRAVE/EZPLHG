standardizeIEEG <- function(data) {
  scaling <- 10^floor(log10(max(data)))
  plotData <- data / scaling
}

#' compute the mean power analysis over the frequency band using the multitaper method
#'
#' @param epoch Matrix or Epoch object. iEEG data matrix or Epoch object. If matrix, the row names are the electrode names and the column names are the time points
#' @param fs Numeric. frequency of signal iEEG acquisition
#'
#' @return A power analysis matrix
#' @export
#'
#' @examples
#' data("pt01EcoG")
#' epoch <- Epoch(pt01EcoG)
#' pt01plhg<-calc_PLHG(epoch)
calc_PLHG <- function(epoch, fs=1000, sizeWindow=3000, sizeSkip=333, plhgTimeWindow=c(0,20), baseTimeWindow=c(-30,-20)){


  np<-reticulate::import('numpy')
  timeNum <- ncol(epoch)
  timesOnset<-epoch$times
  nyquist <- fs/2
  transitionWidth <- 0.1
  elecNum <- nrow(epoch)

  startBaseIndex<-which.min(abs(timesOnset - baseTimeWindow[1]))
  endBaseIndex<-which.min(abs(timesOnset - baseTimeWindow[2]))

  startEpochIndex<-which.min(abs(timesOnset - plhgTimeWindow[1]))
  endEpochIndex<-which.min(abs(timesOnset - plhgTimeWindow[2]))

  timesOnset<-timesOnset[startEpochIndex:endEpochIndex]

  tsBaseline<-t(epoch$data[,startBaseIndex:endBaseIndex])
  tsIctal<-t(epoch$data[,startEpochIndex:endEpochIndex])

  nt<-endEpochIndex-startEpochIndex+1

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

  PLVMaster<-matrix(0,nw,elecNum)


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


  plhgMaster<-matrix(0,nw,elecNum)


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

  plhg<-t(plhgMaster)

  startTimes<-timeVals/fs

  maxVal<-apply(plhgMaster,2,max)
  mu<-mean(maxVal)
  sigma<-sd(maxVal)
  coeffVar<-sigma/mu
  mu=mean(plhgMaster)
  sigma<-sd(plhgMaster)
  sigThres<-mu+sigma*2.5
  sigLeads<-maxVal>sigThres

  voteThres   <- vector(mode="numeric", length=elecNum)
  sigTime   <- vector(mode="numeric", length=elecNum)

  for(jj in 1:elecNum){
    sigTime[jj]=NaN
    if(sigLeads[jj]==TRUE){
      voteThres[jj]=1
    }
    currentInd<-which(plhgMaster[,jj]>=sigThres)
    if(length(currentInd)>0){
      #print(jj)
      sigTime[jj]=timeVals[currentInd[1]]/fs
    }
  }



  PLHG(
    plhg = plhg,
    voteThres = voteThres,
    sigTime = sigTime,
    startTimes = startTimes,
    electrodes = epoch$electrodes
  )



}
