
#' @export

#' @title Calculate minimum and 2nd quartile thresholds for rain detection in audio files

#' @inheritParams getMetrics
#' @return a matrix of \code{min} and \code{Q2} thresholds (rows) for \code{psd} and \code{s2n} at each band (columns)
#' in \code{freqLo} and \code{freqHi}
#' @examples
#'
#' # Get filenames of training data (known rain recordings in wav files). Only five files are used here for purposes
#' of this example
#' train.fn <- list.files(system.file("extdata/rain", package = "hardRain"), "\\.wav$", full.names = T)
#'
#' # Calculate the threshold using default settings - for two frequency bands
#' trBR <- getThreshold(train.fn, fn = "spec")
#' trBR


getThreshold <- function(wav, freqLo = c(0.6, 4.4), freqHi = c(1.2,5.6), parallel = F){


  if(mode(wav) == "character" & is.vector(wav)){
    tmp <- getMetrics(wav, freqLo=freqLo, freqHi=freqHi, parallel = parallel)
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




