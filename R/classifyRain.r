#' @title Classify audio files for presence/absence of rain
#'
#' @description Using thresholds from \code{getThreshold} function, audio files are classified as TRUE/FALSE for
#' presence of rain using a minimum or quartile based threshold.
#'
#' @details This function is based on rain classification techniques in Bedoya... Metcalfe et al... thresholds are calculated
#' using minimum psd and signal to noise ratio (mean/sd) etc...
#'
#' @inheritParams getMetrics
#' @param thresh.vals A matrix or vector of thresholds obtained from \code{getThreshold}
#' @param threshold threshold method ("min" or "Q2") default calculated both - see details
#' @param ID vector of IDs (character or factor) for each wav file identifying rain status,
#' e.g. rain or non-rain (optional). This can be used for testing and calculating accuracy metrics.
#' @return a dataframe with the following columns: filename (of wav files), ID (if provided),
#' logical columns with results of each threshold classification. If t.step is not NULL, its value is
#' included in the data frame attributes
#' @export
#' @examples
#' # Get filenames of training data (known rain recordings in wav files). Only five files are used
#' # here for purposes of this example
#'
#' train.fn <- list.files(system.file("extdata/rain", package = "hardRain"), "\\.wav$", full.names = T)
#'
#' # Calculate the threshold using default settings - for two frequency bands
#' trBR <- getThreshold(train.fn)
#' trBR
#'
#' # Get the test filenames (10 wav files with rain / non-rain)
#' test.fn <- list.files(system.file("extdata/test", package = "hardRain"), "\\.wav$", full.names = T)
#'
#' # Classify the test files using the thresholds obtained above
#' resBR <- classifyRain(test.fn, thresh.vals = trBR)
#' head(resBR)
#'
#' # How many files identified as rain/non-rain for each threshold?
#' lapply(split(resBR, list(resBR$threshold)), function(x) table(x[,"value"]))


classifyRain <- function(wav, thresh.vals, freqLo = c(0.6, 4.4),
                         freqHi = c(1.2,5.6), t.step = NULL,
                         threshold = c("min", "Q2"), ID = NULL, parallel = F){


  if(mode(wav) == "character" & is.vector(wav)){
    tmp <- getMetrics(wav, freqLo=freqLo, freqHi=freqHi, t.step = t.step, parallel = parallel)
    f.names <- basename(wav)
    } else {if(class(wav) == "matrix") {
      tmp <- wav
      f.names <- rownames(wav)
      } else stop("If wav is not vector of filenames, it should be a matrix - output of getMetrics()")
    }

  # check names on threshold type...
  if(!all(colnames(tmp) == colnames(thresh.vals))) stop("Threshold column names do not match data")

  if(missing(ID)) ID <- NULL

  # how many thresholds are there?
  noBands <- length(freqLo)

  threshold <- match.arg(threshold, several.ok = T)

  ## produce results for both thresholds

  res2 <- lapply(threshold, function(x){


    t.val <- thresh.vals[x,]
    # evaluate data against thresholds
    res <- t(t(tmp) >= t.val)
    # head(res); tail(res)

    ### sum by thresholds (cumulative)

    # rearrange order of columns in res for easier summing below
    res <- res[,c(seq(1, noBands*2,2), seq(2, noBands*2, 2))]

    # sum first two bands, then first 4 bands, first 6 bands, etc.
    bands <- lapply(seq_len(noBands), function(x) unname(apply(res[,1:(x*noBands)], 1, all)))

    bands <- data.frame(value = unlist(bands))
    bands$band <- rep(paste0("band", seq_len(noBands)), each = nrow(tmp))
    bands <- bands[,c(2,1)] # swap order

    # head(bands)
    f.names <- rep(f.names, noBands)
    threshMethod <- rep(x, noBands*nrow(tmp))

    if(is.null(ID)) {data.frame(filename = f.names,
                                threshold = threshMethod,
                                bands, stringsAsFactors = F)} else {

                      data.frame(filename = f.names,
                                 threshold = x,
                                 bands,
                                 ID = rep(ID, noBands), stringsAsFactors = F)
                                }
  }
    )

  res3 <- do.call(rbind, res2)

  ## return just band1 + band2 results
  res4 <- subset(res3, band == "band2")
  res4$band <- NULL

  # for both band results separately, substitute res3 for res4 below (when adding t.step column)

  if("t.step" %in% names(attributes(tmp))){

    # add time column - probably should just add this column in the metrics function...
    tmp2 <- data.frame(filename = f.names, t.step = attr(tmp, "duration"))
    res4 <- merge(res4, tmp2, by = "filename", sort = F)
    res4 <- res4[order(res4$filename, res4$threshold, res4$t.step),]
    rownames(res4) <- NULL
    attributes(res4) <- c(attributes(res4), t.step = t.step)
  }

  return(res4)
}
