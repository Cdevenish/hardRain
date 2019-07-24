#' @title Classify audio files for presence/absence of rain
#'
#' Using thresholds from \code{getThreshold} function, audio files are classified as TRUE/FALSE for
#' presence of rain using a minimum or quartile based threshold.
#'
#' This function is based on rain classification techniques in Bedoya... Metcalfe et al... thresholds are calculated
#' using minimum psd and signal to noise ratio (mean/sd) etc...
#'
#' @inheritParams getMetrics
#' @param threshold threshold method (one of "min" or "Q2") - see details
#' @param ID vector of IDs (character or factor) for each wav file identifying rain status,
#' e.g. rain or non-rain (optional). This can be used for testing and calculating accuracy metrics.
#' @return a dataframe with the following columns: filename (of wav files), ID (if provided),
#' logical columns with results of each threshold classification
#' @export
#' @examples
#' # Get filenames of training data (known rain recordings in wav files). Only five files are used
#' # here for purposes of this example
#'
#' train.fn <- list.files(system.file("extdata/rain", package = "hardRain"), "\\.wav$", full.names = T)
#'
#' # Calculate the threshold using default settings - for two frequency bands
#' trBR <- getThreshold(train.fn, fn = "spec")
#' trBR
#'
#' # Get the test filenames (10 wav files with rain / non-rain)
#' test.fn <- list.files(system.file("extdata/test", package = "hardRain"), "\\.wav$", full.names = T)
#'
#' # Classify the test files using the thresholds obtained above
#' resBR <- classifyRain(test.fn, t.values = trBR, fn = "spec")
#' head(resBR)
#'
#' # How many files identified as rain/non-rain for each frequency band and threshold?
#' lapply(split(resBR, list(resBR$band, resBR$threshold)), function(x) table(x[,"value"]))


classifyRain <- function(wav, t.values, freqLo = c(0.6, 4.4), freqHi = c(1.2,5.6), fn = c("meanspec", "spec"), threshold = c("min", "Q2"), ID = NULL, parallel = F){


  if(mode(wav) == "character" & is.vector(wav)){
    tmp <- getMetrics(wav, fn = fn, freqLo=freqLo, freqHi=freqHi, parallel = parallel)
    f.names <- basename(wav)
    } else {if(class(wav) == "matrix") {
      tmp <- wav
      f.names <- rownames(wav)
      } else stop("If wav is not vector of filenames, it should be a matrix - output of getMetrics()")
    }

  # check names on threshold type...
  if(!all(colnames(tmp) == colnames(t.values))) stop("Threshold column names do not match data")

  if(missing(ID)) ID <- NULL

  # how many thresholds are there?
  noBands <- length(freqLo)

  threshold <- match.arg(threshold, several.ok = T)

  # extract thresholds to use
  # t.values <- switch(threshold,
  #                    min = t.values["min",],
  #                    Q2 = t.values["Q2",]
  #                    )

  ## produce results for both thresholds

  res2 <- lapply(threshold, function(x){


    t.val <- t.values[x,]
    # evaluate data against thresholds
    res <- t(t(tmp) >= t.val)
    # head(res); tail(res)

    # sum by thresholds (cumulative)
    # rearrange order of columns in res for easier summing below
    res <- res[,c(seq(1, noBands*2,2), seq(2, noBands*2, 2))]

    # sum first two bands, then first 4 bands, first 6 bands, etc.
    bands <- lapply(seq_len(noBands), function(x) unname(apply(res[,1:(x*noBands)], 1, all)))
    # bands <- setNames(bands, paste0("band", seq_len(noBands)))
    # bands <- do.call(cbind, bands)
    bands <- data.frame(value = unlist(bands))
    bands$band <- rep(paste0("band", seq_len(noBands)), each = nrow(tmp))
    bands <- bands[,c(2,1)] # swap order

    # head(bands)
    f.names <- rep(f.names, noBands)
    threshMethod <- rep(x, noBands*nrow(tmp))

    if(is.null(ID)) {data.frame(filename = f.names, threshold = threshMethod, bands, stringsAsFactors = F)} else {
      data.frame(filename = f.names, threshold = x, bands, ID = rep(ID, noBands), stringsAsFactors = F)
    }
    }
    )

  return(do.call(rbind, res2))

  ## Do confusion matrix and accuracy stats??

}

