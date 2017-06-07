#' @title Summarize simple random sample for discrete attributes
#' @description Summarizes population-level statistics for
#' simple random sample for attribute data. The calculations are
#' derived from Chapter 3 in Avery and Burkhart's (1967)
#' Forest Measurements, Fifth Edition. The variance terms refer
#' to the variance of the mean.
#' @param data dataframe containing observations of variable of
#' interest.
#' @param attribute character name of attribute to be summarized. 
#' Attribute must already be expanded.
#' @param popTot numeric population size. Equivalent to total 
#' number of individuals.
#' @return dataframe of stand-level statistics. Includes standard error and
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

summarize_simple_random_discrete <- function(data, attribute = 'attr', popTot = NA) {

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
              lowerLimitCI = P - (2 * SE + 1 / (2 * sampTot)), # 95% confidence interval
              upperLimitCI = P + (2 * SE + 1 / (2 * sampTot)))

  return(calculations)

}
