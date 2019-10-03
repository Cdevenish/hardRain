#' hardRain: A package for detecting rain events in audio data
#'
#' The hardRain package provides two main functions:
#' getThreshold() for calculating PSD and signal to noise thresholds on known rain data, and
#' classifyRain() which uses the thresholds to identify rain events in other audio data files. A further function,
#'  cutRain() will label rain events in audio software (e.g. Audacity), and optionally, save contiguous non-rain audio
#'  as new .wav files.
#'
#'
#' @docType package
#' @name hardRain
NULL
