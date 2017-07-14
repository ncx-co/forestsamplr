#' @title Summarize all simple random sample
#' @description Summarizes population-level statistics for
#' simple random sample data.
#' @param data dataframe or vector containing observations of
#' variable of interest. Variable of interest must already be expanded
#' to the level of interest (e.g. stand-level).
#' @param attribute character name of attribute to be summarized.
#' Must be defined if data is input as a dataframe.
#' @param type object type of data, e.g. 'dataframe' or 'vector'.
#' @param popSize numeric population size. Defaults to NA (unknown popSize).
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9).
#' @param infiniteReplacement logical true if sample was done with replacement
#' or from an infinite population. False if sampled without replacement,
#' from a finite population. Defaults to False.
#' @return a dataframe of population mean, variance, standard error, and
#' high and low confidence limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' #trainingDataFrame
#' data <- data.frame(bapa = c(120, 140, 160, 110, 100, 90),
#'   plots = c(1, 2, 3, 4, 5, 6))
#' #trainingVector
#' data <- c(120, 140, 160, 110, 100, 90)
#' attribute <- 'bapa'
#' desiredConfidence <- 0.9
#' }
#' @export


summarize_all_srs <- function(data, attribute = 'attr', type = 'vector', popSize = NA,
                              desiredConfidence = 0.95, infiniteReplacement = F, bernoulli = F) { 
  
  if (bernoulli == F) {
    
    out <- summarize_simple_random(data, attribute, type, popSize, 
                                   desiredConfidence, infiniteReplacement)
    
  } else {
    
    out <- summarize_simple_random_discrete(data, attribute, popTot = popSize, desiredConfidence)
    
  }
  
  return(out)
  
}
