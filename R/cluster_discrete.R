#' @title Summarize cluster sample for discrete attributes
#' @description Summarizes population-level statistics for
#' cluster sample for attribute data. The calculations are
#' derived from Chapter 3 in Avery and Burkhart's (1967)
#' Forest Measurements, Fifth Edition. The variance terms refer
#' to the variance of the mean.
#' @param data dataframe containing observations of variable of
#' interest. Attribute must already be expanded. Attribute must 
#' be coded as either TRUE and FALSE or 1 and 0.
#' @param attribute character name of attribute to be summarized.
#' @param plotTot numeric population size. Equivalent to the 
#' total number of possible plots in the population.
#' @param desiredConfidence numeric desired confidence level.
#' @return dataframe of stand-level statistics. Includes standard error and
#' 95% confidence interval.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' data <- data.frame(plots = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
#'                    propAlive = c(0.75, 0.80, 0.80, 0.85, 0.70,
#'                    0.90, 0.70, 0.75, 0.80, 0.65))
#' attribute = 'propAlive'
#' plotTot = 250
#' desiredConfidence = 0.95
#' }
#' @export




summarize_cluster_discrete <- function(data, attribute, plotTot = NA, desiredConfidence = 0.95) {
  
  attrTemp <- unlist(data %>% dplyr::select(one_of(attribute)))
  data$attr <- attrTemp
  
  if (is.na(plotTot)) {
    stop("Total number of units is required as input.")
  }
  
  calculations <- data %>%
    mutate(attrSq = attr ^ 2) %>%
    summarize(sampleSize = length(attr),
              mean = sum(attr) / sampleSize,
              variance = (sum(attrSq) - sum(attr) ^ 2 / sampleSize) / (sampleSize - 1),
              standardError = sqrt((variance / sampleSize) * (1 - sampleSize / plotTot)),
              # 2-tailed
              upperLimitCI = mean + qt(1 - ((1 - desiredConfidence) / 2), sampleSize - 1) * standardError,
              lowerLimitCI = mean - qt(1 - ((1 - desiredConfidence) / 2), sampleSize - 1) * standardError
    )
  
  return(calculations)
  
}
