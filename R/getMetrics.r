
#' @export

#' @title Get psd and signal to noise ratio for audio files
#' @description This function does not generally need to be called directly. It is the workhorse function that
#' reads wav files, extracts psd and signal to noise for specified frequency bands using seewave
#' functions spec() or meanspec(). This function is called by getThreshold() and classifyRain() which will generally
#' be used directly.
#'
#' @param wav A vector of wav filenames (including directories)
#' @param freqLo a vector of Lower frequency cut offs - defaults to 2 bands (0.6-1.2 kHz and 4.4-5.6 kHz)
#' @param freqHi a vector of Higher frequency cut off - defaults to 2 bands: (0.6-1.2 kHz and 4.4-5.6 kHz)
#' @param fn a character vector, which seewave function to use: spec or meanspec (see details)
#' @param parallel Logical. Whether to use multicore processing with the parallel package (must be loaded)
#' @return a numeric matrix with columns \code{psd} and \code{s2n} for each wav file in \code{wav},
#' filenames are conserved in the rownames
#' @examples
#' see examples in getThreshold() and \code{\link{classifyRain}}


getMetrics <- function(wav, freqLo = c(0.6, 4.4), freqHi = c(1.2,5.6),
                       fn = c("meanspec", "spec"), parallel = F){

  # These are in dependencies, so don't need to be here for final package functions
  # library(seewave)
  # library(tuneR)


  if(length(wav) == 0 | is.null(wav)) stop("wav filenames input does not exit")


  # check for presence of fftw package and use if present
  if(requireNamespace("fftw", quietly = TRUE)) {
    fftw <- T
  } else {
    fftw <- F
  }

  # check that freqLo and freqHi are same length
  if(length(freqLo) != length(freqHi)) stop("freqLo and freqHi must be the same length")

  # check freqLo and freqHi are within sensible limits
  if(!all(freqLo > 0, freqHi < 10.0)) stop("Check values of freqLo and freqHi are >0, <10, respectively")

  # check that freqHi > freqLo
  if(!all(freqHi > freqLo)) stop("freqHi must be higher than freqLo pairwise")

  fn <- match.arg(fn)

  if(parallel){

    # library(parallel) # in base R.. so ok just to ::

    noCores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(noCores)

    parallel::clusterExport(cl, c("wav", "fn", "fftw", "freqLo", "freqHi"), envir = environment())
    parallel::clusterEvalQ(cl, {
      library(seewave)
      library(tuneR)
    }
    )

    mfs.lst <- parallel::parLapply(cl, wav, function(x) {

      b <- tuneR::readWave(x) # read in audiofile
      f <- as.numeric(b@samp.rate)

      #mean frequency spectrum # x is frequency (kHz); y is amplitude
      mfs <- switch(fn,

                    spec = seewave::spec(b, PSD=T, wn="rectangle", ovlp=0, fftw=fftw, plot=F, f=f),
                    meanspec = seewave::meanspec(b, PSD=T, wn="rectangle", ovlp=0, fftw=fftw, plot=F, f=f)
      )

      # take psd scores for each rain frequency window in khz
      mapply(function(lo,hi) mfs[mfs[,1] > lo & mfs[,1] < hi, 2], freqLo, freqHi, SIMPLIFY = F)

    })

    parallel::stopCluster(cl)

  } else {

    pb <- txtProgressBar(min = 0, max = length(wav), style = 3)

    mfs.lst <- lapply(wav, function(x) {

      setTxtProgressBar(pb, which(x == wav))

      b <- tuneR::readWave(x) # read in audiofile
      f <- as.numeric(b@samp.rate)

      #mean frequency spectrum # x is frequency (kHz); y is amplitude
      mfs <- switch(fn,

                spec = seewave::spec(b, PSD=T, wn="rectangle", ovlp=0, fftw=fftw, plot=F, f=f),
                meanspec = seewave::meanspec(b, PSD=T, wn="rectangle", ovlp=0, fftw=fftw, plot=F, f=f)
      )
      #dim(mfs); head(mfs)

      # take psd scores for each rain frequency window in khz
      mapply(function(lo,hi) mfs[mfs[,1] > lo & mfs[,1] < hi, 2], freqLo, freqHi, SIMPLIFY = F)

    })
    close(pb)
  }

  #str(mfs.lst, max.level = 2)
  # return(mfs.lst)

  res <- lapply(mfs.lst, function(x) {

    psd <- sapply(x, mean) # psd of filtered frequency window
    s2n <- sapply(x, function(y) mean(y)/sd(y)) # sig2noise ratio
    list(psd=psd, s2n=s2n)

  })

  #head(res)
  # return(res)

  # reformat list - store as numeric matrix with rownames attributes as filenames
  tmp <- lapply(1:2, function(x) do.call(rbind, sapply(res, function(y) y[x])))
  res2 <- do.call(cbind, tmp)
  # head(res2)
  cNames <- apply(expand.grid("band", seq_along(freqLo), c("psd", "s2n"), KEEP.OUT.ATTRS = F),
                  1, paste0, collapse = ".")
  dimnames(res2) <- list(basename(wav), cNames)
  rm(tmp, cNames)

  return(res2)


}
