#' @title Summarize simple random sample
#' @description Summarizes population-level statistics for
#' simple random sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean, hence the
#' \code{n} terms in the deonminators.
#' @param trainingData dataframe containing observations of variable of
#' interest, and stratum assignment for each plot
#' @param attribute character name of attribute to be summarized
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9)
#' @param infReplacement logical true if sample was done with replacement
#' or from an infite population. False if sampled without replacement,
#' from a finite population.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' trainingData <- data.frame(bapa = c(120, 140, 160, 110, 100, 90),
#'   plots = c(1, 2, 3, 4, 5, 6)) #not accurate
#' attribute = 'bapa'
#' desiredConfidence = 0.9
#' }
#' @export

summarize_simple_random <- function(y, popN, desiredConfidence = 0.9,
                                    infReplacement) {

  # give the variable of interest a generic name
#  attrTemp <- unlist(trainingData %>% dplyr::select(one_of(attribute)))
#  trainingData$attr <- attrTemp

#  stratumSummaries <- trainingData %>%
#    left_join(stratumTab) %>%
#    mutate(attrExpanded = attr * acres) %>%
#    group_by(stratum)

#  y = c(5, 6, 3, 1, 8, 10)
#  n = length(y)
#  infReplacement = F
#  popN = 50

    #    inputs(N = population_size, # n = sample size found using n()
    #           y = attr = attribute_of_interest
    #           infReplacement = logic true if sampled with replacement and/or infinite population
    #    )
  n <- length(y)
  if (!is.na(popN) && popN < n) {
    stop("Population size is less than sample size.")
  }

  test <- (!is.na(popN) && (!infReplacement))
  simpRandomSummary <- data.frame(y) %>%     #<- trainingData %>%
    mutate(popMean = sum(y)/n) %>% #change to just base mean function
    mutate(var = (sum(y ^ 2) - (sum(y ^ 2) / n)) / (n - 1)) %>%
    mutate(se = ifelse(test, sqrt((var / n) * ((popN - n) / popN)),  # without replacement, finite population
           sqrt(var / n))) %>%  # with replacement, infinite population
    mutate(highCL = mean(y) + qt(1 - ((1 - desiredConfidence) / 2), n - 1) * se) %>%  # 2-tailed
    mutate(lowCL = mean(y) - qt(1 - ((1 - desiredConfidence) / 2), n - 1) * se) %>%
    summarize('mean' = popMean[1], 'variance' = var[1], 'standardError' = se[1],
              'upperLimitCI' = highCL[1], 'lowerLimitCI' = lowCL[1])

  # return dataframe of
  output <- simpRandomSummary

  return(output)

}



