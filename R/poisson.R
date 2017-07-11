#' @title Summarize Poisson sample
#' @description Summarizes population-level statistics for
#' Poisson sample data. The calculations are derived from open 
#' resources provided by Penn State Eberly College.
#' @param data vector containing number of instances per desired 
#' unit (e.g. 6 trees were alive at plot 10 -> c(6)).
#' @param desiredConfidence numeric desired confidence level 
#' (e.g. 0.9).
#' @return dataframe of statistics including standard error and 
#' confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' data <- c(2, 3, 4, 3, 4, 5, 2, 7)
#' desiredConfidence <- 0.95
#' }
#' @export


summarize_poisson <- function(data, desiredConfidence = 0.95) {
  
  # Calculate frequency of the input values, and convert to a dataframe
  tableFreq <- table(data)
  dataFrame <- data.frame(values = as.numeric(names(tableFreq)),
                          frequency = as.vector(tableFreq)
                          )
  
  summary <- dataFrame %>%
    mutate(sampleSize = length(frequency)) %>%
    mutate(xi = values * frequency) %>%
    mutate(means = 1 / sampleSize[1] * sum(xi)) %>%
    summarize(sampleSize = sampleSize[[1]],
              mean = means[[1]],
              lambdaHat = mean,
              lowerBoundCI = lambdaHat - qnorm(1 - (1 - desiredConfidence) / 2)
                             * sqrt(mean / sampleSize[[1]]),
              upperBoundCI = lambdaHat + qnorm(1 - (1 - desiredConfidence) / 2) 
                             * sqrt(mean / sampleSize[[1]])
              )
  
  overdispersionLimit <- summary$mean * (summary$sampleSize - summary$mean) / 
                         summary$sampleSize
  
  # Check for overdispersion
  if (var(data) > overdispersionLimit) {
    warning('Variance significantly higher than the mean. The data is influenced
            by overdispersion.')
  }

return(summary)

}









