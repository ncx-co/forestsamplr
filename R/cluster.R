#' @title Summarize cluster sample
#' @description Summarizes population-level statistics for
#' cluster sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean.
#' @param cluster dataframe containing observations of variable of
#' interest.
#' @return dataframe of cluster-level statistics including clusterID,
#' standard error, and confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' cluster = data.frame(clusterID = c(1, 2, 3, 4, 5),
#'                      clusterElements = c(4, 2, 9, 4, 10),
#'                      sumAttr = c(1000, 1250, 950, 900, 1005),
#'                      isUsed = c(T, T, F, T, T))
#' }
#' @export


#edge cases:
#Allow for plots to be entered
#Allow for different confidence levels
#could return stand-level statistics, as well



summarize_cluster <- function(cluster) {


#  # give the variable of interest a generic name
#  attrTemp <- unlist(trainingData %>% dplyr::select(one_of(attribute)))
#  trainingData$attr <- attrTemp


  if(any(is.na(cluster))) {
    stop("NA values are present in the input data.")
  }

  if (as.integer(anyDuplicated(cluster$clusterID)) == 1) {
    stop("Data cannot have repeated clusterID.")
  }

  if (length(cluster$clusterID) == 1) {
    stop("Must have multiple clusters. Consider other analysis.")
  }

  # notes on variables:
  # m(i) is the number of elements in a cluster
  # m___ is the sum of elements for a specified set of clusters
  # n___ is the number of clusters in a specified set of clusters
  # bar indicates an average value

  # basic values: sample-level
  sampValues <- cluster[cluster$isUsed,] %>%
    mutate(nSamp = n()) %>% # num clusters
    mutate(mSampBar = sum(clusterElements) / nSamp)


  # basic values: population-level
  popValues <- cluster %>%
  summarize(mPop = sum(clusterElements),
            nPop = n(), # num clusters
            mPopBar = mPop / nPop)
  if (is.na(popValues$mPopBar)) { # if Mbar (pop) is unknown, approximate it with mbar (samp)
    popValues$mPopBar <- sum(sampValues$mSampBar[1]) / sum(sampValues$mSampBar[1])
  }


  finalCalc <- sampValues %>%
    mutate(yBar = sum(sumAttr) / sum(clusterElements)) %>%
    mutate(ySETempNum = (sumAttr - yBar * clusterElements) ^ 2) %>%
    mutate(ySE = sqrt(((popValues$nPop - nSamp) / (popValues$nPop * nSamp * (popValues$mPopBar ^ 2)))
                      * ySETempNum / (nSamp - 1))) %>%
    mutate(highCL = yBar + 2 * ySE) %>% # for 95% confidence interval
    mutate(lowCL = yBar - 2 * ySE)


  clusterSummary <- select(finalCalc,
                           clusterID = clusterID,
                           clusterElements = clusterElements,
                           totalAttrubite = sumAttr,
                           standardError = ySE,
                           upperLimitCI = highCL,
                           lowerLimitCI = lowCL
                           )


  # return dataframe of cluster-level statistics
  return(clusterSummary)

}



