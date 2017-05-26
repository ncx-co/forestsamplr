#' @title Summarize cluster sample
#' @description Summarizes population-level statistics for
#' cluster sample data. The calculations are derived from Chapter 3 in
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

summarize_cluster <- function(trainingData, attribute, popN = NA,
                                    desiredConfidence = 0.9, infReplacement = F) {

  # give the variable of interest a generic name
  attrTemp <- unlist(trainingData %>% dplyr::select(one_of(attribute)))
  trainingData$attr <- attrTemp

  trainingData <- data.frame('m' = c(5, 7, 3, 90), #number of elements in one cluster
             'clusterID' = c(1, 2, 1, 5), #distinct clusters
             'isUsed' = c(T, T, T, F), #whether or not the cluster is included in the sample
             'attr' = c(10, 15, 20, 80) #attribute, or 'y'
             )


  popValues <- trainingData %>%
    summarize(mPop = sum(m),
              nPop = n(),
              mPopBar = mPop / nPop)
    if (mPopBar = NA) { #if Mbar (pop) is unknown, approximate it with mbar (samp)
      mPopBar <- mSampBar
    }

  sampValues <- trainingData[trainingData$isUsed,]
  vals <- uniqueCluster = unique(sampValues$clusterID) %>%
    mutate(mSum <- for (all.uniqueCluster) {sum(sampValues$m)})
    summarize(mSamp = sum(m),
              nSamp = n(),
              mSampBar = mSamp / nSamp,
              ySum = sum(attr)) #doesn't make sense.


  values <- bind_cols(popValues, sampValues)

  clusterSummary <- trainingData[trainingData$isUsed,] %>%
    mutate(yBar = ySum / mSamp) %>%
    mutate(ySE = sqrt(((nPop - nSamp) / (nPop * nSamp * (mPopBar ^ 2)))
               * (for_each(yValue - yBar * mVlaue) / (nSamp - 1)))) %>%
    mutate(highCL = yBar + 2 * SE) %>% #for 95% confidence interval
    mutate(lowCL = yBar - 2 * SE)


  # return dataframe of
  output <- clusterSummary

  return(output)

}



