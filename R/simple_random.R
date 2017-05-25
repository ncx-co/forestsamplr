#' @title Summarize simple random sample
#' @description Summarizes population-level statistics for
#' simple random sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean, hence the
#' \code{n} terms in the deonminators.
#' @param trainingData dataframe containing observations of variable of
#' interest.
#' @param attribute character name of attribute to be summarized. Attribute
#' must already be expanded.
#' @param popN numeric population size. Assumes popN is not known.
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9).
#' @param infReplacement logical true if sample was done with replacement
#' or from an infite population. False if sampled without replacement,
#' from a finite population. Assumes without replacement, from a finite
#' population.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' trainingData <- data.frame(bapa = c(120, 140, 160, 110, 100, 90),
#'   plots = c(1, 2, 3, 4, 5, 6))
#' attribute = 'bapa'
#' desiredConfidence = 0.9
#' }
#' @export

summarize_simple_random <- function(trainingData, attribute, popN = NA,
                                    desiredConfidence = 0.9, infReplacement = F) {

  # give the variable of interest a generic name
  attrTemp <- unlist(trainingData %>% dplyr::select(one_of(attribute)))
  trainingData$attr <- attrTemp

  n <- length(trainingData$attr)
  if (!is.na(popN) && popN <= n) {
    stop("Population size must be greater than sample size.")
  }

  if (is.na(infReplacement)) {
    infReplacement <- FALSE
  }

  test <- (!is.na(popN) && (!infReplacement))
  simpRandomSummary <- trainingData %>%
    # set NA in attribute to 0
    mutate(attr = ifelse(is.na(attr), 0, attr)) %>% #can I do this?? or is there a better way?
    mutate(popMean = mean(attr)) %>%
    mutate(var = (sum(attr ^ 2) - (sum(attr ^ 2) / n)) / (n - 1)) %>%
    mutate(se = ifelse(test, sqrt((var / n) * ((popN - n) / popN)),  # without replacement, finite population
           sqrt(var / n))) %>%  # with replacement, infinite population
    mutate(highCL = mean(attr) + qt(1 - ((1 - desiredConfidence) / 2), n - 1) * se) %>%  # 2-tailed
    mutate(lowCL = mean(attr) - qt(1 - ((1 - desiredConfidence) / 2), n - 1) * se) %>%
    summarize('mean' = popMean[1], 'variance' = var[1], 'standardError' = se[1],
              'upperLimitCI' = highCL[1], 'lowerLimitCI' = lowCL[1])

  # return dataframe of
  output <- simpRandomSummary

  return(output)

}



