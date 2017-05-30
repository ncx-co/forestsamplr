#' @title Summarize simple random sample
#' @description Summarizes population-level statistics for
#' simple random sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean, hence the
#' \code{sampleSize} terms in the denominators.
#' @param trainingData dataframe containing observations of variable of
#' interest.
#' @param attribute character name of attribute to be summarized. Attribute
#' must already be expanded to the level of interest (e.g. stand-level).
#' @param popSize numeric population size. Defaults to NA (unknown popSize).
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9).
#' @param infiniteReplacement logical true if sample was done with replacement
#' or from an infite population. False if sampled without replacement,
#' from a finite population. Assumes without replacement, from a finite
#' population.
#' @return a dataframe of population mean, variance, standard error, and
#' high and low confidence limits.
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

summarize_simple_random <- function(trainingData, attribute, popSize = NA,
                                    desiredConfidence = 0.9, infiniteReplacement = F) {

  # give the variable of interest a generic name
  attrTemp <- unlist(trainingData %>% dplyr::select(one_of(attribute)))
  trainingData$attr <- attrTemp

  sampleSize <- length(trainingData$attr)
  if (!is.na(popSize) && popSize <= sampleSize) {
    stop("Population size must be greater than sample size.")
  }

  if(any(is.na(trainingData$attr))) {
    stop("NA values are present in the input data.")
  }

  if (is.na(infiniteReplacement)) {
    infiniteReplacement <- FALSE
  }

  test <- (!is.na(popSize) && (!infiniteReplacement))
  simpRandomSummary <- trainingData %>%
    summarize(
      mean = mean(attr),
      variance = (sum(attr ^ 2) - (sum(attr ^ 2) / sampleSize)) / (sampleSize - 1),
      standardError = ifelse(test, sqrt((variance / sampleSize) * ((popSize - sampleSize) / popSize)),
                  # without replacement, finite population
                  sqrt(variance / sampleSize)),  # with replacement, infinite population
      upperLimitCI = mean(attr) + qt(1 - ((1 - desiredConfidence) / 2), sampleSize - 1) * standardError,  # 2-tailed
      lowerLimitCI = mean(attr) - qt(1 - ((1 - desiredConfidence) / 2), sampleSize - 1) * standardError
    )

  # return dataframe of key values
  return(simpRandomSummary)

}



