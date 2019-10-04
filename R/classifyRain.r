#' @title Classify audio files for presence/absence of rain
#'
#' @description Using thresholds from \code{getThreshold} function, audio files are classified as TRUE/FALSE for
#' presence of rain using a minimum or second quartile (more conservative) based threshold at multiple frequency
#'  bands - defaults to 2 bands (0.6-1.2 kHz and 4.4-5.6 kHz).
#'
#' @details This function is based on rain classification techniques in Metcalfe et al. (2019). Thresholds are
#'  calculated using minimum psd and signal to noise ratio (mean/sd).
#'
#'  Metcalf, O. C., Lees, A.C., Barlow, J. & Devenish, C. (2019). hardRain: an R package for quick, automated
#'  rainfall detection in ecoacoustic datasets using a threshold-based approach. Ecological Indicators (in press)
#'
#' See also:
#'  Bedoya C, Isaza C, Daza JM, López JD. (2017). Automatic identification of rainfall in acoustic recordings.
#'  Ecological Indicators 75:95–100.
#'
#'
#' @inheritParams getMetrics
#' @param thresh.vals A named matrix of thresholds obtained from \code{getThreshold}. Alternatively,
#' you can create your own matrix (see examples for format).
#' @param threshold threshold type ("min" or "Q2") defaults to both - see details
#' @param ID vector of IDs (character or factor) for each wav file identifying rain status,
#' e.g. rain or non-rain (optional). This can be used for testing and calculating accuracy metrics.
#' @return a dataframe with the following columns: filename (of wav files), ID (if provided),
#' logical columns with threshold values for each frequency band and measurement type. If t.step is not NULL,
#' its value is included in the data frame attributes.
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
#' tapply(resBR$value, list(resBR$threshold), table)
#'
#' #Using a custom matrix of thresholds:
#' thresh.m2 <- matrix(c(0.02995, 0.01507, 1.8849, 1.8340, 0.0324, 0.01715, 1.8880, 1.8792),
#'                    nrow = 2,
#'                    ncol = 4,
#'                    byrow = T,
#'                    dimnames = list(c("min", "Q2"), c("band.1.psd", "band.2.psd", "band.1.s2n", "band.2.s2n")))
#'
#' # A matrix is required for just one threshold:
#' thresh.m1 <- matrix(c(0.02995, 0.01507, 1.8849, 1.8340),
#'                    nrow = 1,
#'                    ncol = 4,
#'                    byrow = T,
#'                    dimnames = list("min", c("band.1.psd", "band.2.psd", "band.1.s2n", "band.2.s2n")))
#'
#' # Classify with custom thresholds
#' resBR2 <- classifyRain(test.fn, thresh.vals = thresh.m2)
#' head(resBR2)
#'
#' resBR3 <- classifyRain(test.fn, threshold = "min", thresh.vals = thresh.m1)
#' head(resBR3)



classifyRain <- function(wav, thresh.vals, freqLo = c(0.6, 4.4),
                         freqHi = c(1.2,5.6), t.step = NULL,
                         threshold = c("min", "Q2"), ID = NULL, parallel = F){


  if(mode(wav) == "character" & is.vector(wav)){
    tmp <- getMetrics(wav, freqLo=freqLo, freqHi=freqHi, t.step = t.step, parallel = parallel)
    } else {if(class(wav) == "matrix") {
      tmp <- wav
      } else stop("If wav is not vector of filenames, it should be a matrix - output of getMetrics()")
    }

  f.names <- unname(rownames(tmp))

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

    if(is.null(ID)) {tmp2 <- data.frame(filename = f.names,
                                threshold = threshMethod,
                                bands, stringsAsFactors = F)} else {

                      tmp2 <- data.frame(filename = f.names,
                                 threshold = x,
                                 bands,
                                 ID = rep(ID, noBands), stringsAsFactors = F)
                                }
    # add duration column
    # probably should just add this column in the getMetrics function...
    if("t.step" %in% names(attributes(tmp))){

      dur.names <- rep(attr(tmp, "duration"), noBands)
      tmp2 <- cbind(tmp2, duration = dur.names)
    }

    tmp2

  }
    )

  res3 <- do.call(rbind, res2)

  ## return just band1 + band2 results
  if(noBands == 2){
    res4 <- subset(res3, band == "band2")
    res4$band <- NULL
    rownames(res4) <- NULL
  } else res4 <- res3
  # res4

  # for both band results separately, substitute res3 for res4 below (when adding t.step column)
  ## get rid of multiple band options and simplify all this...

  if("t.step" %in% names(attributes(tmp))){
    res4 <- res4[order(res4$filename, res4$threshold, res4$duration),]
    attributes(res4) <- c(attributes(res4), t.step = t.step)
  }

  return(res4)
}
