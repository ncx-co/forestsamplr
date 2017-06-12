#' @title Summarize simple random sample
#' @description Summarizes population-level statistics for
#' simple random sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean, hence the
#' \code{sampleSize} terms in the denominators.
#' @param trainingData dataframe or vector containing observations of
#' variable of interest. Variable of interest must already be expanded
#' to the level of interest (e.g. stand-level).
#' @param attribute character name of attribute to be summarized.
#' Must be defined if data is input as a dataframe.
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

summarize_simple_random <- function(data, attribute = 'vector', popSize = NA,
                                    desiredConfidence = 0.9, infiniteReplacement = F) {

  # converts variable of interest into a vector with a generic name
  if (attribute == 'vector') {
    attr <- data
  } else {
    # makes sure data is expressed as a numeric vector
    attr <- as.numeric(as.vector(data %>% dplyr::select(one_of(attribute)))[[1]])
  }

  sampleSize <- length(attr)
  if (!is.na(popSize) && popSize <= sampleSize) {
    stop("Population size must be greater than sample size.")
  }

  if(any(is.na(attr))) {
    stop("NA values are present in the input data.")
  }

  if (is.na(infiniteReplacement)) {
    infiniteReplacement <- FALSE
  }

  test <- (!is.na(popSize) && (!infiniteReplacement))
  simpRandomSummary <- data.frame(attr) %>%
    summarize(
      mean = mean(attr),
      variance = (sum(attr ^ 2) - (sum(attr) ^ 2 / sampleSize)) / (sampleSize - 1),
      standardError = ifelse(test, sqrt((variance / sampleSize) * ((popSize - sampleSize) / popSize)),
                  # without replacement, finite population
                  sqrt(variance / sampleSize)),  # with replacement, infinite population
      upperLimitCI = mean(attr) + qt(1 - ((1 - desiredConfidence) / 2), sampleSize - 1) * standardError,  # 2-tailed
      lowerLimitCI = mean(attr) - qt(1 - ((1 - desiredConfidence) / 2), sampleSize - 1) * standardError
    )

  # return dataframe of key values
  return(simpRandomSummary)

}



