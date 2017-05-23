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
#' @param infReplacement logical true if sample was done with replacement
#' or from an infite population. False if sampled without replacement,
#' from a finite population.
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
    group_by(stratum)

  y = c(5, 6, 3, 1, 8, 10)
  n = length(y)
  infReplacement = T
  popN = NA

    #    inputs(N = population_size, # n = sample size found using n()
    #           y = attr = attribute_of_interest
    #           infReplacement = logic true if sampled with replacement and/or infinite population
    #    )

    simpRanSummary <- trainingData %>%

    mutate(popMean = sum(y)/n) %>%
    mutate(var = (sum(y ^ 2) - (sum(y ^ 2) / n)) / (n - 1)) %>%
    if (!is.na(popN)) {
      mutate(se = ifelse(infReplacement, sqrt(var / n), #with replacement, infinite population
                        sqrt((var / n) * ((popN - n) / popN)))) #without replacement, finite population
    } else {
      #IS IT EVEN AN ISSUE??
      # from OSU coursepage http://oregonstate.edu/instruct/bot440/wilsomar/Content/SRS.htm
      # if sampled without replacement considering a finite population,
      # only bother with the correction factor if >10% sampling intensity
#      sampIntensityReplacement <- (qt(1 - ((1 - desiredConfidence) / 2), n - 1) * SE
#                                   / ((highCL - lowCL) / 2)) ^ 2
#      sampIntensity <- ifelse(infReplacement, sampIntensityReplacement, #with replacement
#                              1 / (sampIntensityReplacement + (1 / popN))) #without replacement
#      if (sampIntensity > 0.10) { #SE: finite population correction factor not used if sampling intensity is more than 10%
#       if (!infReplacement) {
#         se <- sqrt((var / n()) * ((popN - n()) / popN))
#       }
#      } else {
        se <- sqrt(var / n())
      }

    }

              highCL = mean(y) + qt(1 - ((1 - desiredConfidence) / 2), n - 1) * se, #2-tailed
              lowCL = mean(y) - qt(1 - ((1 - desiredConfidence) / 2), n - 1) * se
              #percentError = qt(((1 - desiredConfidence) / 2), n - 1) * se / popMean * 100 # not from avery and burkhart??
    )

    } else {


    }
  )



  # return dataframe of
  output <- simpRanSummary

  return(output)

}



