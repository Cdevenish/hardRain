
#'@export

#' @title Get thresholds for rain detection
#'
#' @param wav A vector of wav filenames (including directories)
#' @param freqLo a vector of Lower frequency cut offs - defaults to 2 bands (0.6-1.2 kHz and 4.4-5.6 kHz)
#' @param freqHi a vector of Higher frequency cut off - defaults to 2 bands: (0.6-1.2 kHz and 4.4-5.6 kHz)
#' @param fn uses spec or meanspec function (seewave package) to ...
#' @param threshold threshold method (one of "min" or "IQR" - see details). If missing no threshold is produced
#' @param ID vector of IDs (character or factor) for each wav file, e.g. rain or non-rain (optional)
#' @param parallel Logical. Whether to use multicore processing with parallel package
#' @return a list with vectors of \code{psd} and \code{s2n} for each wav file in \code{wav}
#' @examples
#'


getThreshold <- function(wav, freqLo = c(0.6, 4.4), freqHi = c(1.2,5.6),
                         fn = c("meanspec", "spec"), parallel = F){


  if(mode(wav) == "character" & is.vector(wav)){
    tmp <- getMetrics(wav, fn = fn, freqLo=freqLo, freqHi=freqHi, parallel = parallel)
  } else {
    if(class(wav) == "matrix") tmp <- wav else {
      stop("If wav is not vector of filenames, it should be a matrix - output of getMetrics()")
      }
  }

  ## threshold part
  res <- matrix(rbind(apply(tmp, 2, min, na.rm = T),
                      apply(tmp, 2, function(x) fivenum(x, na.rm=T)[2])),
                nrow = 2, ncol = length(freqLo)*2, dimnames = list(c("min", "Q2"), colnames(tmp)))

  return(res)
}




