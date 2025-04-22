#' Visualization iEEGData
#'
#' @param ieegts Numeric. A matrix of iEEG time series x(t),
#' with time points as rows and electrodes names as columns
#' @param scaling Scaling factor
#' @param displayChannels Channels to display
#'
#' @return plot raw signal
#' @export
#'
#' @examples
#' data("PT01Epochm30sp30s")
#' data("ElectrodesDataPT01")
#' displayChannels=which(ElectrodesDataPT01$insoz==TRUE)
#' visuiEEGdata(ieegts=PT01Epochm30sp30s,1000000, displayChannels = displayChannels)
visuiEEGdata<-function( ieegts, scaling, displayChannels){


  plotData<-ieegts[,displayChannels]/scaling
  gaps<-2
  displayNames=colnames(ieegts)[displayChannels]

  for(i in seq_along(plotData)){
    plotData[, i] <- (plotData[, i]- mean(plotData[, i]))+
      (ncol(plotData)-i)*gaps
  }

  plot(plotData[, 1],type="l" ,cex=0.1,
       ylim = range(plotData), yaxt = "n")
  for(i in 2:ncol(plotData)){
    lines(plotData[, i])
  }
  axis(2, at = rev(seq_along(displayChannels) - 1)*gaps,
       labels = displayNames,las=1)


}


#' Heatmap visualization
#'
#' @param resPLHG Matrix result of PLHG
#' @param ElectrodesData Electrodes data
#' @param ieegts Numeric. A matrix of iEEG time series x(t),
#' with time points as rows and electrodes names as columns
#' @param time_window_ictal Fragility heatmap time window around seizure onset
#' @param subject_code patient name
#' @param j seizure number
#'
#' @return Heatmap plot of the PLHG matrix with soz electrodes in blue in the bottom
#' @export
#'
#' @examples
#' data("PT01Epochm30sp30s")
#'data("ElectrodesDataPT01")
#'data("resPLHG")
#'ElectrodesData=ElectrodesDataPT01
#'time_window_ictal=c(-10,10)
#'subject_code='PT01'
#'j=1
#'heatmap_PLHG(plhgMaster=resPLHG[[1]], ElectrodesData=ElectrodesDataPT01,ieegts=PT01Epochm30sp30s,time_window_ictal,subject_code,j)
heatmap_PLHG<-function(plhgMaster, ElectrodesData,ieegts,time_window_ictal,subject_code,j){


  titlepng=paste(subject_code,'Seizure',as.character(j),sep=" ")
  elecsoz=which(ElectrodesData$insoz==TRUE)
  elecsozc=which(ElectrodesData$insoz==FALSE)
  elecsozsozc=c(elecsoz,elecsozc)

  elecnum <- colnames(ieegts)[elecsozsozc]
  n_elec <- ncol(ieegts)
  nw<- nrow(plhgMaster)
  colorelec<-elecnum
  nsoz=length(elecsoz)
  colorelec[1:n_elec]="black"
  colorelec[1:nsoz]="blue"

  plhgord<-plhgMaster[,elecsozsozc]
  stimes=c(1:nw)*(time_window_ictal[2]-time_window_ictal[1])/nw+time_window_ictal[1]
  rownames(plhgord)<-stimes
  elecnum<-colnames(plhgord)

  plhgmap_data <- expand.grid(Time = stimes, Electrode = elecnum)
  plhgmap_data$Value <- c(plhgord)

  ggplot2::ggplot(plhgmap_data, ggplot2::aes(x = Time, y = Electrode, fill = Value)) +
    ggplot2::geom_tile() +
    ggplot2::ggtitle(titlepng)+
    ggplot2::labs(x = "Time (s)", y = "Electrode",size=2) +
    viridis::scale_fill_viridis(option = "turbo") +  #

    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size=3,colour=colorelec),     # Adjust depending on electrodes
    )


}
