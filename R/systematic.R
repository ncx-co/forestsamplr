#' @title Summarize systematic sample
#' @description Summarizes population-level statistics for
#' systematic sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean, hence the
#' \code{sampleSize} terms in the denominators.
#' @usage summarize_systematic(data, attribute = 'attr', type = 'vector', 
#'                             popSize = NA, desiredConfidence = 0.95)
#' @param data data frame or vector containing observations of
#' variable of interest. Variable of interest must already be expanded
#' to the level of interest (e.g. stand-level).
#' @param attribute character name of attribute to be summarized.
#' Must be defined if data is input as a data frame.
#' @param type object type of data, e.g. 'dataframe' or 'vector'.
#' @param popSize numeric population size. Defaults to NA (unknown popSize).
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9).
#' @return a data frame of population mean, variance, standard error, and
#' high and low confidence limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' 
#' # See Forest Sampling vignette for more details
#' 
#' # Data frame input data can be expressed as:
#' 
#' trainingData <- data.frame(bapa = c(120, 140, 160, 110, 100, 90),
#'                            plots = c(1, 2, 3, 4, 5, 6))
#' attribute = 'bapa'
#' desiredConfidence = 0.9
#' 
#' # Vector input data can be expressed as:
#' 
#' vector <- c(120, 140, 160, 110, 100, 90)
#' 
#' }
#' @export

summarize_systematic <- function(data, attribute = 'attr', type = 'vector', popSize = NA, desiredConfidence = 0.95) {

  # return data frame of key values
  output <- summarize_simple_random(data = data, attribute, type, popSize, desiredConfidence, FALSE)
  
  
  return(output)

}



