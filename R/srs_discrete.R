#' @title Summarize simple random sample for discrete attributes
#' @description Summarizes population-level statistics for
#' simple random sample for attribute data. The calculations are
#' derived from Chapter 3 in Avery and Burkhart's (1967)
#' Forest Measurements, Fifth Edition. The variance terms refer
#' to the variance of the mean.
#' @param data data frame containing observations of variable of
#' interest. Attribute must be coded as either TRUE and FALSE or
#' 1 and 0.
#' @param attribute character name of attribute to be summarized.
#' @param popTot numeric population size. Equivalent to total 
#' number of individuals.
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9).
#' @return data frame of stand-level statistics. Includes standard error and
#' 95% confidence interval.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' data <- data.frame(alive = c(T, T, F, T, F, F),
#'   tree = c(1, 2, 3, 4, 5, 6))
#' attribute = 'alive'
#' popTot = 50
#' }
#' @export

summarize_simple_random_discrete <- function(data, attribute = 'attr', 
                                             popTot = NA, desiredConfidence = 0.95) {

  attrTemp <- unlist(data %>% dplyr::select(one_of(attribute)))
  data$attr <- attrTemp

  if (is.na(popTot)) {
    stop("Total number of units is required as input.")
  }

  calculations = data.frame(sampTot = length(data$attr),
                            sampAttr = sum(data$attr)) %>%
    summarize(totalSample = sampTot,
              attribute = sampAttr,
              totalPopulation = popTot,
              P = sampAttr / sampTot, # proportion 
              SE = sqrt(((P * (1 - P)) / (sampTot - 1)) * (1 - (sampTot / popTot))),
              lowerLimitCI = P - (qt(1 - ((1 - desiredConfidence) / 2), sampAttr - 1)
                                  * SE + 1 / (2 * sampTot)),
              upperLimitCI = P + (qt(1 - ((1 - desiredConfidence) / 2), sampAttr - 1) 
                                  * SE + 1 / (2 * sampTot)))

  return(calculations)

}
