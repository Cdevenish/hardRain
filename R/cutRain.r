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
#' @param label Optional. A character vector, either "audacity" or "raven", to include a label file to
#' be written in \code{outF} for the original wav, labelling all non-rain sections (ie those that will be exported)
#' @return A dataframe detailing the new wav files created, with, file path, start and end times
#' logical columns with results of each threshold classification
#' @export
#' @examples
#'

cutRain <- function(x, threshold = c("min", "Q2"), outF, inF, start, label, label.only = F){


  # check x is results format from classifyRain()
  if(!all(class(x) == "data.frame", c("filename", "threshold", "duration") %in% colnames(x))) stop("x must be a results data.frame from the classifyRain function where the t.step argument is not NULL")

  threshold <- match.arg(threshold)

  if(missing(inF)) inF <- getwd()
  if(missing(outF)) outF <- getwd()

  if(!missing(start) && !class(start) %in% c("POSIXlt", "POSIXct")) stop("start must be a dateTime class - see ?
DateTimeClasses")

  if(label.only & missing(label)) stop("Must provide label if label.only is TRUE")

  t.step <- attr(x, "t.step")

  # make sure there's some hard rain...
  if(all(x$value)) stop("No rain detected in any file!")

  ## get contiguous file snippets to bind together
  ## Remove unused threshold data and keep only files where no rain
  x <- x[x$threshold == threshold & !x$value,]

  if(all(x$value)) stop("No rain detected in any file!")

  # group by filenames
  fts <- tapply(x$duration, list(x$filename), c, simplify = F)

  # if length one, then just extract single segemnt
  l <- lengths(fts) == 1
  #sum(l)

  if(any(l)){

    fn1 <- names(fts[l])
    stop1 <- unlist(fts[l])
    start1 <- stop1 - t.step
    res0 <- data.frame(filename = fn1, start = start1, stop = stop1, suffix=1, row.names = NULL)

  } else res0 <- NULL

  fts <- fts[!l]
  # group rest of snippets by contiguous
  #fts[[1]] <- c(5,10,15,30,35,40,50,55,60)

  # helper function to get contiguous indices
  diff2index <- function(n){

    f <- diff(n) == t.step
    cs <- cumsum(f)
    dups <- c(T, duplicated(cs))
    start <- which(dups)
    stop <- c(start[-1]-1, length(n))
    rng <- mapply(function(x, y) range(n[x:y]), start, stop, SIMPLIFY = T)

  }

  contg <- lapply(fts, diff2index)
  # contg
  # str(contg)
  fn2 <- names(contg)
  l2 <- unname(sapply(contg, ncol))
  sf <- unlist(lapply(l2, function(x) 1:x))

  res1 <- data.frame(filename = rep(fn2, l2), t(do.call(cbind, contg)), suffix = sf)
  colnames(res1)[2:3] <- c("start", "stop")
  res1$start <- res1$start-t.step

  res2 <- rbind(res0, res1)

  ##  make complete filepaths
  res2$fullP <- file.path(inF, res2$filename)

  # check they exist
  if(!all(file.exists(res2$fullP))) stop ("One or more files cannot be found - please check your file paths")

  # make outpaths
  res2$outP <- file.path(outF, paste0(sub("\\.wav$", "", res2$filename), "_", res2$suffix, ".wav"))


  ## import segments and save
  if(!label.only){

    wavs <- mapply(function(x,y,z) tuneR::readWave(x, from = y, to = z, units = "seconds"),
                   res2$fullP, res2$start, res2$stop)

    mapply(function(x,y) tuneR::writeWave(x, filename = y), wavs, res2$outP)
}

  if(!missing(label)){

    label <- match.arg(label, c("audacity", "raven"))

    switch(label,

           audacity = {
             txt <- by(res2, list(res2$filename),function(x) cbind(x[,c("start", "stop")], status = "clear"))
             sapply(names(txt), function(x) {
               write.table(txt[x],
                           file = file.path(inF, paste0(sub("\\.wav$", "", x), ".txt")),
                           row.names = F,
                           col.names = F,
                           sep = "\t")
               })
           },

           raven = {

             stop("raven selection table not implemented yet")
             # txt <- by(res2, list(res2$filename),function(x) cbind(Selection = "clear",
             #                                                       View = "Spectogram",
             #                                                       Channel = 1,
             #                                                       x[,c("start", "stop")],
             #                                                       Low = 1000,
             #                                                       High = 10000))
             #
             # sapply(names(txt), function(x) {
             #   write.table(txt[x],
             #               file = file.path(inF, paste0(sub("\\.wav$", "", x), ".txt")),
             #               row.names = F,
             #               col.names = F,
             #               sep = "\t")
             # })

           }
           )


  }
}




