#' @title Summarize Poisson sample
#' @description Summarizes population-level statistics for
#' Poisson sample data. The calculations are derived from open 
#' resources provided by Penn State Eberly College.
#' @param data vector containing number of instances per desired 
#' unit (e.g. 6 trees were alive at plot 10 -> c(6)).
#' @param desiredConfidence numeric desired confidence level 
#' (e.g. 0.9).
#' @return data frame of statistics including standard error and 
#' confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' data <- c(2, 3, 4, 3, 4, 5, 2, 7)
#' }
#' @export


summarize_poisson <- function(data, desiredConfidence = 0.95) {
  
  # Calculate frequency of the input values, and convert to a dataframe
  tableFreq <- table(data)
  dataFrame <- data.frame(values = as.numeric(names(tableFreq)),
                          frequency = as.vector(tableFreq)
                          )
  
  summary <- dataFrame %>%
    mutate(xi = values * frequency) %>%
    summarize(sampleSize = length(data),
              mean = 1 / sampleSize * sum(xi),
              lambdaHat = mean,
              se = sqrt(mean / sampleSize),
              lowerBoundCI = lambdaHat - qnorm(1 - (1 - desiredConfidence)
                                               / 2) * se,
              upperBoundCI = lambdaHat + qnorm(1 - (1 - desiredConfidence) 
                                               / 2) * se
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









