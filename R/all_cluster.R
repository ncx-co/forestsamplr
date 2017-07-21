#' @title Summarize all cluster sample
#' @description Summarizes population-level statistics for
#' cluster sample data. This function has two options: (1) 
#' Cluster sample with a normal distribution and (2) Cluster 
#' sample with a Bernoulli distribution.
#' @usage summarize_all_cluster(data, attribute = NA, element = TRUE, 
#'                              plotTot = NA, 
#'                              desiredConfidence = 0.95,
#'                              bernoulli = F)
#' @param data data frame containing observations of variable of
#' interest for either cluster-level or plot-level data.
#' @param attribute character name of attribute to be summarized.
#' @param element logical true if parameter data is plot-level, false if
#' parameter data is cluster-level. Default is True.
#' @param plotTot numeric population size. Equivalent to the 
#' total number of possible elements in the population.
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9).
#' @param bernoulli logical TRUE if data fitting the Bernoulli 
#' distribution is used.
#' @return data frame of stand-level statistics including
#' standard error and confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' 
#' # See Forest Sampling vignette for more details
#' 
#' # Plot level data can be expressed as:
#' 
#' plotLevelDataExample <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2,  
#'                                                  2, 3, 4, 4, 4, 4, 
#'                                                  4, 4, 5, 5, 5, 5, 
#'                                                  5), 
#'                                    attr = c(1000, 1250, 950, 900, 
#'                                             1005, 1000, 1250, 950,
#'                                             900, 1005, 1000, 1250, 
#'                                             950, 900, 1005, 1000, 
#'                                             1250, 950, 900), 
#'                                    isUsed = c(T, T, T, T, T, T, T, 
#'                                               T, T, T, T, T, T, T, 
#'                                               F, F, F, F, F))
#' 
#' # Cluster level data can be expressed as: 
#' 
#' clusterLevelDataExample <- data.frame(clusterID = c(1, 2, 3, 4, 5), 
#'                                       clusterElements = c(4, 2, 9, 
#'                                                           4, 10), 
#'                                       sumAttr = c(1000, 1250, 950,
#'                                                   900, 1005), 
#'                                       isUsed = c(T, T, F, T, T))
#' # Set element = FALSE
#' 
#' 
#' # Bernoulli data can be expressed as: 
#' 
#' bernoulliData <- data.frame(plots = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'                             propAlive = c(0.75, 0.80, 0.80, 0.85, 
#'                                           0.70, 0.90, 0.70, 0.75, 
#'                                           0.80, 0.65))
#' # Set parameter bernoulli = TRUE
#' 
#' }
#' @export


summarize_all_cluster <- function(data, attribute = NA, element = TRUE, 
                                  plotTot = NA, desiredConfidence = 0.95, bernoulli = F) { 
  
  if (bernoulli == F) {
    
    out <- summarize_cluster(data, element, attribute, desiredConfidence)
    
  } else {
    
    out <- summarize_cluster_discrete(data, attribute, plotTot, desiredConfidence)
    
  }
  
  return(out)
  
}
