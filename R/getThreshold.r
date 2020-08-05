
#' @export

#' @title Calculate thresholds for rain detection in audio files
#' @description Calculate minimum and 2nd quartile thresholds for Power Spectrum Density (PSD) and Signal-to-Noise
#' (s2n) ratio from files known to contain rain, at multiple frequency bands - defaults to 2 bands (0.6-1.2 kHz and
#' 4.4-5.6 kHz). For use by the classifyRain() function in order to detect the presence of rain in audio files

#' @inheritParams getMetrics
#' @param wav A vector of wav filenames (including directories) of known rain recordings. Or, the output matrix from
#' \code{getMetrics} function.
#' @return a matrix of \code{min} and \code{Q2} thresholds (rows) for \code{psd} and \code{s2n} at each band (columns)
#' in \code{freqLo} and \code{freqHi}
#' @examples
#'
#' # Calculate thresholds using 5 files of training data (known rain recordings in wav files).
#' # Only five files are used here for purposes of this example - but see recommendations in Metcalf et al. 2019
#'
#' # Get filenames of training data
#' train.fn <- list.files(system.file("extdata/rain", package = "hardRain"), "\\.wav$", full.names = T)
#'
#' # Calculate the threshold using default settings
#' trBR <- getThreshold(train.fn, fn = "spec")
#' trBR


getThreshold <- function(wav, freqLo = c(0.6, 4.4), freqHi = c(1.2,5.6), parallel = 0){


  if(mode(wav) == "character" & is.vector(wav)){
    tmp <- getMetrics(wav, freqLo=freqLo, freqHi=freqHi, parallel = parallel)
  } else {
    if(class(wav) == "matrix") tmp <- wav else { #  & grepl("psd|s2n", colnames(wav)) # FINISH CHECKS
      stop("If wav is not vector of filenames, it should be a matrix - output of getMetrics()")
      }
  }

  ## threshold part
  res <- matrix(rbind(apply(tmp, 2, min, na.rm = T),
                      apply(tmp, 2, function(x) fivenum(x, na.rm=T)[2])),
                nrow = 2, ncol = length(freqLo)*2, dimnames = list(c("min", "Q2"), colnames(tmp)))

  return(res)
}




