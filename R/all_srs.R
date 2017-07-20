#' @title Summarize all simple random sample
#' @description Summarizes population-level statistics for
#' simple random sample data. This function has three options: (1) SRS 
#' of a finite population or sampled without replacement,
#' (2) SRS of an infinite population or sampled with replacement,
#' and (3) SRS with a bernoulli distribution.
#' @usage summarize_all_srs(data, attribute = 'attr',
#'                          popSize = NA, desiredConfidence = 0.95, 
#'                          infiniteReplacement = F, bernoulli = F)
#' @param data data frame or vector containing observations of
#' variable of interest. Variable of interest must already be expanded
#' to the level of interest (e.g. stand-level).
#' @param attribute character name of attribute to be summarized.
#' Must be defined if data is input as a data frame.
#' @param popSize numeric population size. Defaults to NA (unknown popSize).
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9).
#' @param infiniteReplacement logical true if sample was done with replacement
#' or from an infinite population. False if sampled without replacement,
#' from a finite population. Defaults to False.
#' @param bernoulli logical TRUE if data fitting the Bernoulli 
#' distribution is used.
#' @return a data frame of population mean, variance, standard error, and
#' high and low confidence limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' 
#' # See Forest Sampling vignette for more details
#' 
#' # Vector data example:
#' 
#' data <- c(120, 140, 160, 110, 100, 90)
#' 
#' 
#' # Data frame data example:
#' 
#' data <- data.frame(bapa = c(120, 140, 160, 110, 100, 90), 
#'                    plots = c(1, 2, 3, 4, 5, 6))
#' attribute <- 'bapa'
#' 
#' 
#' # Bernoulli data example:
#' 
#' data <- data.frame(alive = c(T, T, F, T, F, F), 
#'                    plots = c(1, 2, 3, 4, 5, 6))
#' attribute <- 'alive'
#' 
#' }
#' @export


summarize_all_srs <- function(data, attribute = 'attr', popSize = NA,
                              desiredConfidence = 0.95, infiniteReplacement = F, bernoulli = F) { 
  
  if (bernoulli == F) {
    
    out <- summarize_simple_random(data, attribute, popSize, 
                                   desiredConfidence, infiniteReplacement)
    
  } else {
    
    out <- summarize_simple_random_discrete(data, attribute, popTot = popSize, desiredConfidence)
    
  }
  
  return(out)
  
}
