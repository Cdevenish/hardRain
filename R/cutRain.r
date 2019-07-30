#' @title Cut and save wav files according to rain classification
#'
#' @description Using results from the \code{classifyRain} function, input wav files will be cut and saved according to classification results. Contiguous snippets of non-rain will be saved as new wav files
#'
#' @details This function will take results from \code{classifyRain} and join together contiguous snippets (in snippets of lengths given in the t.step argument).
#'
#' @param x A matrix of results from \code{classifyRain}
#' @param threshold A character vector with the threshold to use either "min" or "Q2". Defaults to "min"
#' @param outF Destination folder for new wav files. Defaults to home directory if missing.
#' @param inF Source folder for wav files. Defaults to home directory if missing.
#' @param start Optional. A vector of dateTime objects giving the start time for each file in class.results
#' @return A dataframe detailing the new wav files created, with, file path, start and end times
#' logical columns with results of each threshold classification
#' @export
#' @examples
#'
#'

cutRain <- function(x, threshold = c("min", "Q2"), outF, inF, start){


  # check x is results format from classifyRain()
  if(!all(class(x) == "matrix", c("filename", "threshold", "t.step") %in% colnames(x))) stop("x must be a results matrix from the classifyRain function where the t.step argument is not NULL")

  threshold <- match.arg(threshold)

  if(missing(inF)) inF <- getwd()
  if(missing(outF)) outF <- getwd()

  if(!missing(start) && !class(start) %in% c("POSIXlt", "POSIXct")) stop("start must be a dateTime class - see ?
DateTimeClasses")

  t.step <- attr(x, "t.step")

  ## get contiguous file snippets to bind together

  ## Remove unused threshold data and keep only files where no rain
  x <- x[x$threshold == threshold & !x$value,]

  # group by filenames
  fts <- tapply(x$t.step, list(x$filename), c, simplify = F)


  ## get file paths of remaining files (with no rain)
  fn <- names(fts)
  inP <- file.path(inF, fn)

  # check they exist
  if(!all(file.exists(inP))) stop ("One or more files cannot be found - please check your file paths")

  # group snippets by contiguous
  contg <- lapply(fts, function(z) diff(z) == t.step)

  # if length one, then put T,.
  l <- lengths(fts)

  # make outpaths
  suffix <- ""
  outP <- file.path(outF, x$filename, suffix)



}


