#' @export

#' @title Classify wav files as having rain or not using thresholds from getThresholds()
#' @inheritParams getMetrics
#' @param threshold threshold method (one of "min" or "Q2") - see details
#' @param ID vector of IDs (character or factor) for each wav file identifying rain status,
#' e.g. rain or non-rain (optional). This can be used for testing and calculating accuracy metrics.
#' @return a dataframe with the following columns: filename (of wav files), ID (if provided),
#' logical columns with results of each threshold classification
#' @examples
#'
#' #' \dontrun{
#' # NOTE: this will download 100 15s wav files (120 MB) to a new directory created in your home directory.
#'
#' # Create a temporary directory for the rain files
#' dir.create(tmp1 <- tempfile("rainBR_", tmpdir = getwd()))
#' # Download the brazil rain data - 100 wav files known to be hard rain and get filenames
#' download.file(url = xxx, destfile = tmp1)
#' train.fn <- list.files(path = tmp1, pattern = "\\.wav$")
#'
#' # Calculate the threshold using default settings - for two frequency bands
#' trBR <- getThreshold(train.fn, fn = "spec")
#' trBR
#'
#' # Create a temporary directory for the test files
#' dir.create(tmp2 <- tempfile("testBR_", tmpdir = getwd()))
#' # Download the brazil test data - 100 wav files (50 rain, 50 non-rain) and get filenames
#' download.file(url = xxx, destfile = tmp2)
#' test.fn <- list.files(path = tmp2, pattern = "\\.wav$")
#'
#' # Classify the test files using the thresholds obtained above
#' resBR <- classifyRain(test.fn, t.values = trBR, fn = "spec", threshold = "min")
#'
#' # How many files identified as rain/non-rain?
#' ## table(resBR)
#' }

classifyRain <- function(wav, t.values, freqLo = c(0.6, 4.4), freqHi = c(1.2,5.6),
                         fn = c("meanspec", "spec"), threshold = c("min", "Q2"), ID = NULL, parallel = F){


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

