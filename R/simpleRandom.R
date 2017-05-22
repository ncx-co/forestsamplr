#' @title Summarize simple random sample
#' @description Summarizes population-level statistics for
#' simple random sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean, hence the
#' \code{n} terms in the deonminators.
#' @param trainingData dataframe containing observations of variable of
#' interest, and stratum assignment for each plot
#' @param attribute character name of attribute to be summarized
#' @param stratumTab dataframe containing acreages for each stratum
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9)
#' @param post logical true if post-stratification was used
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' trainingData <- data.frame(bapa = c(120, 140, 160, 110, 100, 90),
#'   stratum = c(1, 1, 1, 2, 2, 2))
#' stratumTab <- data.frame(stratum = c(1, 2), acres = c(200, 50))
#' attribute = 'bapa'
#' desiredConfidence = 0.9
#' }
#' @export

summarize_simple_random <- function(trainingData, attribute,
                                 stratumTab, desiredConfidence = 0.9, post = T) {

  # give the variable of interest a generic name
  attrTemp <- unlist(trainingData %>% dplyr::select(one_of(attribute)))
  trainingData$attr <- attrTemp


    stratumSummaries <- trainingData %>%
      left_join(stratumTab) %>%
      mutate(attrExpanded = attr * acres) %>%
      group_by(stratum) %>%


#    inputs(N = population_size, # n = sample size found using n()
#           y = attribute_of_interest
#    )

      summarize(popMean = sum(y)/n(),
                var = (sum(y ^ 2) - (sum(y ^ 2) / n())) / (n - 1),
                SE = sqrt(popVar / n()), #IF w/replacement
                SE = sqrt((popVar / n()) * ((N - n()) / N)), #w/o replacement
                highCL = mean(y) + qt(desiredConfidence / 2, n - 1) * SE, #2-tailed
                lowCL = mean(y) - qt(desiredConfidence / 2, n - 1) * SE,
                sampIntensityReplacement = (qt(desiredConfidence / 2, n - 1) * SE
                                          / ((highCL - lowCL) / 2)) ^ 2, #w/replacement
                sampIntensityNoReplacement = 1 / (sampIntensityReplacement + (1 / N))
                                          #w/o replacement
                #plot size influences?
                )



  # return list of
  outList <- list(stratumSummaries = data.frame(stratumSummaries),
                  totalSummary = data.frame(totalSummary))

  return(outList)

}
