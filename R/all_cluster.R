#' @title Summarize all cluster sample
#' @description Summarizes population-level statistics for
#' cluster sample data.
#' @param data data frame containing observations of variable of
#' interest for either cluster-level or plot-level data.
#' @param attribute character name of attribute to be summarized.
#' @param plot logical true if parameter data is plot-level, false if
#' parameter data is cluster-level. Default is True.
#' @param plotTot numeric population size. Equivalent to the 
#' total number of possible plots in the population.
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9).
#' @param bernoulli logical TRUE if data fitting the Bernoulli 
#' distribution is used.
#' @return data frame of stand-level statistics including
#' standard error and confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' dataPlot <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
#'                        attr = c(1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005, 1000,
#'                                 1250, 950, 900, 1005, 1000, 1250, 950, 900),
#'                        isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))
#' plot = TRUE
#' attribute = 'attr'
#' }
#' @export


summarize_all_cluster <- function(data, attribute = NA, plot = TRUE, 
                                  plotTot = NA, desiredConfidence = 0.95, bernoulli = F) { 
  
  if (bernoulli == F) {
    
    out <- summarize_cluster(data, plot, attribute, desiredConfidence)
    
  } else {
    
    out <- summarize_cluster_discrete(data, attribute, plotTot, desiredConfidence)
    
  }
  
  return(out)
  
}
