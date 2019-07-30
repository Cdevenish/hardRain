
#' @export

#' @title Get PSD and Signal to Noise ratio for audio files
#' @description This function does not generally need to be called directly. It is the workhorse function that
#' reads wav files, extracts psd and signal to noise for specified frequency bands using seewave
#' functions spec() or meanspec(). This function is called by getThreshold() and classifyRain() which will generally
#' be used directly.
#'
#' @param wav A vector of wav filenames (including directories)
#' @param freqLo A numeric vector of Lower frequency cut offs for each band - defaults
#' to 2 bands (0.6-1.2 kHz and 4.4-5.6 kHz)
#' @param freqHi A numeric vector of Higher frequency cut off - defaults to 2 bands: (0.6-1.2 kHz and 4.4-5.6 kHz)
#' @param t.step NULL or a numeric vector giving time in seconds in which to divide
#' longer files. If NULL, it is assumed that all files analysed are suitably short (e.g. 15 s each)
#' and do not need to be subdivided (see details)
#' @param parallel Logical. Whether to use multicore processing with the parallel package (Windows only)
#' (must be loaded)
#' @return A numeric matrix with columns \code{psd} and \code{s2n} for each wav file in \code{wav},
#' filenames are conserved in the rownames
#' @examples See examples in getThreshold() and \code{\link{classifyRain}}


getMetrics <- function(wav, freqLo = c(0.6, 4.4), freqHi = c(1.2,5.6), t.step = NULL, parallel = F){

  # These are in dependencies, so don't need to be here for final package functions
  # library(seewave)
  # library(tuneR)


  if(length(wav) == 0 | is.null(wav)) stop("wav filenames input does not exit")

  if(!is.null(t.step)){

    if(!is.numeric(t.step)) stop("t.step must be numeric - see details") else {

      if(t.step > 60) warning("Long time divisions may not give sensible results, consider 20 s or less.")
    }

  }


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

  if(parallel){

    # library(parallel) # in base R.. so ok just to ::

    noCores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(noCores)

    parallel::clusterExport(cl, c("wav", "t.step", "fftw", "freqLo", "freqHi"), envir = environment())
    parallel::clusterEvalQ(cl, {
      library(seewave)
      library(tuneR)
    }
    )
    # appFn <- parallel::parLapply
    mfs.lst <- parallel::parLapply(cl, wav, function(x) {

      #if(!parallel) setTxtProgressBar(pb, which(x == wav))

      b <- tuneR::readWave(x) # read in audiofile
      f <- as.numeric(b@samp.rate)

      # get wl from t.step (a few ms will probably be left unprocessed at end of each file with step)
      if(!is.null(t.step)) wl <- t.step*f else wl <- length(b)
      wl <- wl - wl%%2 # make sure it's even

      # get freq spectrum
      fs <- seewave::spectro(b, wl = wl, wn="rectangle", fftw=fftw, plot=F, dB = NULL) # add , ...
      # str(fs)
      # with dB = NULL, then this gives a ^2 already, even if dBref is NULL
      # 'dB' argument computes 20*log10(x) where x is the FFT, which is equivalent to 10*log10(x^2)

      # take psd scores for each rain frequency window in khz
      mapply(function(lo,hi) fs$amp[fs$freq > lo & fs$freq < hi, ,drop = F],
             freqLo, freqHi, SIMPLIFY = F)
      # str(tmp2)
    })

    parallel::stopCluster(cl)



  } else {

    # appFn <- lapply
    pb <- txtProgressBar(min = 0, max = length(wav), style = 3) # length of all wavs?

    mfs.lst <- lapply(wav, function(x) {

      setTxtProgressBar(pb, which(x == wav))

      b <- tuneR::readWave(x) # read in audiofile
      f <- as.numeric(b@samp.rate)

      # get wl from t.step (a few ms will probably be left unprocessed at end of each file with step)
      if(!is.null(t.step)) wl <- t.step*f else wl <- length(b)
      wl <- wl - wl%%2 # make sure it's even

      # get freq spectrum
      fs <- seewave::spectro(b, wl = wl, wn="rectangle", fftw=fftw, plot=F, dB = NULL) # add , ...
      # str(fs)
      # with dB = NULL, then this gives a ^2 already, even if dBref is NULL
      # 'dB' argument computes 20*log10(x) where x is the FFT, which is equivalent to 10*log10(x^2)

      # take psd scores for each rain frequency window in khz
      mapply(function(lo,hi) fs$amp[fs$freq > lo & fs$freq < hi, ,drop = F],
             freqLo, freqHi, SIMPLIFY = F)
      # str(tmp2)
    })

    close(pb)
    }

  # str(mfs.lst)

  # Get metrics here
  res <- lapply(mfs.lst, function(x) {

    psd <- sapply(x, colMeans) # psd of filtered frequency window
    s2n <- sapply(x, function(y) apply(y, 2, function(z) mean(z)/sd(z))) # sig2noise ratio
    list(psd=matrix(psd, ncol = length(freqLo)), s2n=matrix(s2n, ncol = length(freqLo)))
    # above, make format consistent as matrix..  if slow, then use if(is.null(t.step))

  })

  #head(res)
  # return(res)

  # reformat list - store as numeric matrix with rownames attributes as filenames
  tmp <- lapply(1:2, function(x) do.call(rbind, sapply(res, function(y) y[x])))
  res2 <- do.call(cbind, tmp)
  # head(res2)
  cNames <- apply(expand.grid("band", seq_along(freqLo), c("psd", "s2n"), KEEP.OUT.ATTRS = F),
                  1, paste0, collapse = ".")
  name.exp <- sapply(res, function(x) sapply(x, nrow)[1])
  dimnames(res2) <- list(mapply(function(x,y) rep(x, each= y), basename(wav), name.exp), cNames)

  if(!is.null(t.step)) {

    duration <- lapply(mapply(function(x,y) rep(x, y), t.step, name.exp, SIMPLIFY = F), cumsum)
    attributes(res2) <-  c(attributes(res2), t.step = t.step, duration = list(unlist(duration)))
  }
  rm(tmp, cNames)

  return(res2)


}
