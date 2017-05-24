#' @title Summarize systematic sample
#' @description Summarizes population-level statistics for
#' systematic sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean, hence the
#' \code{n} terms in the deonminators.
#' @param trainingData dataframe containing observations of variable of
#' interest #not exactly necessary
#' @param y attribute: vector of attribute to be summarized
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9)
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

summarize_systematic <- function(y, popN, desiredConfidence = 0.9) {

  summarize_simple_random(y, popN, desiredConfidence = 0.9, FALSE)

  # return dataframe of
  output <- simpRandomSummary

  return(output)

}



