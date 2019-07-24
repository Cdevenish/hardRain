
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
#' \dontrun{
#' # NOTE: this will download 100 15s wav files (120 MB) to a new directory created in your home directory.
#'
#' # Create a temporary directory for the rain files
#' dir.create(tmp <- tempfile("rainBR_", tmpdir = getwd()))
#' # Download the brazil rain data - 100 wav files known to be hard rain and get filenames
#' download.file(url = , destfile = tmp)
#' wav.fn <- list.files(path = tmp, pattern = "\\.wav$")
#'
#' # Calculate the threshold using default settings - for two frequency bands
#' trBR <- getThreshold(wav.fn, fn = "spec")
#' trBR
#' }


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




