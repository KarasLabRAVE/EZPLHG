.PLHG<- setClass(
    "PLHG",
    slots = list(
        plhg= "matrix",
        startTimes = "numeric",
        electrodes = "character"
    )
)


PLHG<- function(plhg, startTimes, electrodes) {

  .PLHG(
    plhg = plhg,
    startTimes = startTimes,
    electrodes = electrodes
  )
}



#' Print the PLHG object
#' @param object A PLHG object
#' @rdname show-PLHG-method
#' @export
setMethod("show", "PLHG", function(object) {
  cat("\nPLHG object\n")
    slots <- c("plhg","startTimes","electrodes")
  printSlots(object, slots = slots)
  cat("Use '$attr' to access the data\n")
  invisible(object)
})


#' Get the number of rows or columns of a PLHG object
#'
#' @param x A PLHG object
#'
#' @rdname dim-PLHG-method
setMethod("nrow", "PLHG", function(x) {
  nrow(x@plhg)
})

#' @rdname dim-PLHG-method
setMethod("ncol", "PLHG", function(x) {
  ncol(x@plhg)
})


#' Subset a PLHG object
#'
#' @param x A PLHG object
#' @param i A logical vector or a numeric vector of indices to subset the electrodes
#' @param j A logical vector or a numeric vector of indices to subset the time windows
#' @param ... Additional arguments (not used)
#' @param drop Additional arguments (not used)
#'
#' @rdname subset-PLHG-method
setMethod("[", "PLHG", function(x, i, j, ..., drop = FALSE) {

  if (!missing(i)){
    i <- checkIndex(i, x$electrodes)
  }else{
    i <- TRUE
  }
  if(missing(j)){
    j <- TRUE
  }

  plhg_subset <- x@plhg[i, j, drop = FALSE]
  startTimes_subset <- x@startTimes[j]
  electrodes_subset <- x@electrodes[i]
  .PLHG(
    plhg = plhg_subset,
    startTimes = startTimes_subset,
    electrodes = electrodes_subset,
  )
})

setMethod("$", "PLHG", function(x, name) {
  slot(x, name)
})

setMethod("$<-", "PLHG", function(x, name, value) {
  slot(x, name) <- value
  invisible(x)
})
