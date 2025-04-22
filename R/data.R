#' Pt01 seizure 1 (-30:30s) around seizure onset
#'
#' This data corresponds to the first seizure of patient PT01 from the Fragility Data Set.
#' The data contains only the good channels.
#' It has been notch filtered and common average referenced in RAVE
#' It has been epoched -30:30s around the seizure onset
#' The acquisition frequency is 1000 Hz
#' EcoG recording gathered in collaboration with the National Institute of Health
#'
#' @docType data
#'
#' @usage data(PT01Epochm30sp30s)
#'
#' @format A Matrix with 20001 rows (time points) and 84 columns (electrodes)
#'
#' @keywords datasets
#'
#' @source Fragility Multi-Center Retrospective Study
#' (\href{https://openneuro.org/datasets/ds003029/versions/1.0.0}{OpenNeuro})
#'
#' @examples
#' data(PT01Epochm30sp30s)
"PT01Epochm30sp30s"

#'Electrodes description
#'
#' Patient PT01 Data frame with good electrode number, classification by epileptologist as in soz and electrode name
#'
#' @docType data
#'
#' @usage data(ElectrodesDataPT01)
#'
#' @format A Matrix with 84 rows (electrodes) and 3 columns
#'
#' @keywords datasets
#'
#' @source Fragility Multi-Center Retrospective Study
#' (\href{https://openneuro.org/datasets/ds003029/versions/1.0.0}{OpenNeuro})
#'
#' @examples
#' data(ElectrodesDataPT01)
"ElectrodesDataPT01"
