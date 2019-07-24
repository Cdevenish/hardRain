
#' @export

#' @title Calculate minimum and 2nd quartile thresholds for rain detection in audio files
#' @param wav A vector of wav filenames (including directories)
#' @param freqLo a vector of Lower frequency cut offs - defaults to 2 bands (0.6-1.2 kHz and 4.4-5.6 kHz)
#' @param freqHi a vector of Higher frequency cut off - defaults to 2 bands: (0.6-1.2 kHz and 4.4-5.6 kHz)
#' @param fn a character vector, which seewave function to use: spec or meanspec (see details)
#' @param parallel Logical. Whether to use multicore processing with the parallel package (must be loaded)
#'
#' @return a matrix of \code{min} and \code{Q2} thresholds (rows) for \code{psd} and \code{s2n} at each band (columns)
#' in \code{freqLo} and \code{freqHi}
#' @examples
#'
#' # Get filenames of training data (known rain recordings in wav files). Only five files are used here for purposes
#' of this example
#' train.fn <- system.file("extdata/rain", package = "hardRain")
#'
#' # Calculate the threshold using default settings - for two frequency bands
#' trBR <- getThreshold(train.fn, fn = "spec")
#' trBR


getThreshold <- function(wav, freqLo = c(0.6, 4.4), freqHi = c(1.2,5.6), fn = c("meanspec", "spec"), parallel = F){


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




