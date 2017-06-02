#' @title Summarize two-stage sample
#' @description Summarizes population-level statistics for
#' two-stage sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean.
#' @param data dataframe containing observations of variable of
#' interest for either cluster-level of plot-level data.
#' @param plot logical true if parameter data is plot-level, false if
#' parameter data is cluster-level. Default is True.
#' @param attriute character name of attribute to be summarized.
#' @return dataframe of stand-level statistics including
#' standard error and confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' data <- data.frame(clusterID = c(1, 2, 3, 4, 5),
#'                      totElements = c(4, 2, 9, 4, 10),
#'                      usedElements = c(2, 2, 2, 2, 2)
#'                      sumAttr = c(1000, 1250, 950, 900, 1005),
#'                      isUsed = c(T, T, F, T, T))
#'
 data <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
                     attr = c(1000, 1250, NA, 900, 1005, 1000, 1250, 950, 900, 1005, NA, 1250, 950, 900, 1005,
                     1000, 1250, NA, 900),
                     isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))
#' }
#' @export


summarize_two_stage <- function(data, plot = TRUE, attribute = NA) {

  if (!is.na(attribute) && (attribute %in% colnames(data))) {

    attrTemp <- unlist(data %>% dplyr::select(one_of(attribute)))

    if (plot) {

      data$attr <- attrTemp

    } else {

      data$sumAttr <- attrTemp

    }

  }


  if (plot) {

    # calculates cluster values from plot data
    data$attr <- ifelse(is.na(data), 0, attr)
    temp <- data %>%
      mutate(attr = ifelse(is.na(attr), 0, attr))
    attrSum <- aggregate(temp$attr, by = list(Category = data$clusterID), FUN = sum) # sum attributes by cluster

    clusterUsed <- aggregate(data$isUsed, by = list(Category = data$clusterID), FUN = sum) #see if any secondary is used in any primary/cluster
    clusterUsed$x <- ifelse(clusterUsed$x > 0, T, F) #convert above to T/F

    totElementsInPrimaryM <- count(data, clusterID) # tally of elements in each cluster
    sampElementsInPrimarym <- count(data[data$isUsed,], clusterID)

    # attach the sum of attributes and tally of elements to unique cluster
    # produce table with key values
    cluster <- merge(clusterUsed, attrSum, by.x = "Category", by.y = "Category", all = TRUE) %>%
      merge(totElementsInPrimaryM, by.x = "clusterID", by.y = "clusterID", all = TRUE) %>%
      rename(totClusterElements = n) %>%
      merge(sampElementsInPrimarym, by.x = "clusterID", by.y = "clusterID", all = TRUE) %>%
      select(clusterID = clusterID, totClusterElements = totClusterElements, sampElementsInPrimarym = n, isUsed = isUsed, sumAttr = x) %>%

  } else {

    # reassigns data as cluster, if input data is cluster-level data
    cluster <- data

  }

##########################################################################################################


  if (as.integer(anyDuplicated(cluster$clusterID)) == 1) {
    stop("Data cannot have repeated clusterID.")
  }

  if (length(cluster$clusterID) == 1) {
    stop("Must have multiple clusters. Consider other analysis.")
  }

  # basic values: sample-level
  sampValues <- cluster[cluster$isUsed,] %>%
    mutate(nSamp = n()) %>% # num clusters
    mutate(mSamp = sum(clusterElements)) %>%
    mutate(mSampBar = mSamp / nSamp) # avg num elements in a cluster

  # basic values: population-level
  popValues <- cluster %>%
    summarize(mPop = sum(clusterElements),
              nPop = n(), # num clusters
              mPopBar = mPop / nPop)
  if (is.na(popValues$mPopBar) | popValues$mPopBar == sampValues$mSampBar[[1]]) { # if Mbar (pop) is unknown, approximate it with mbar (samp)
    popValues$mPopBar <- sum(sampValues$mSampBar[[1]])
  }

  yij = sum of attributes: all plots in all blocks
  yj = sum of attributes in all the plots in one block
  i(yj) = sum for all blocks
  m - sample plots
  n = sample blocks

  primaryValues <- data %>%
    mutate(ySumBlock = sum attr by block
    mutate(ySqSumBlock = mutate(attrSq <- attr ^ 2) THEN sum by block
         #convert to df with no repeated blocks

  finalCalc <- sampValues %>%

  # df = clusterID, primary (blocks), secondary tot (plot sum), ySumBlock, ySqSumBlock
  yBar = sum(ySumBlock) / ( m * n)
  s2b = ((sum(ySumBlock ^ 2) / m) - (sum(ySumBlock) ^ 2 / (m * n))) / (n - 1)
  s2w = (sum(ySqSumBlock) - sum(ySumBlock ^ 2) / m) / (n * (m - 1))
  #if primaries are equal in size, num of elements per cluster is equal:
  SELargeEqual = sqrt((1 / (m * n)) (s2b * (1 - n / N) + n * s2w / N * (1 - m / M)))
  #if n is a small fraction of N, SE is simplified to:
  SEn = sqrt(s2b ^ 2 / (m * n))
  #when n/N is fairly large, but num of secondaries (m) sampled in
  #each selected primary is only a fraction of the total secondaries (M) in each primary (n)
  SEm = sqrt(1 / (m * n) * (s2b * (1 - n / N) + (n * s2w) / N))


  finalCalc <- sampValues %>%
    mutate(yBar = sum(sumAttr) / sum(clusterElements)) %>%
    mutate(ySETempNum = (sumAttr - yBar * clusterElements) ^ 2) %>%
    mutate(ySE = sqrt(((popValues$nPop - nSamp[[1]]) / (popValues$nPop * nSamp[[1]] * (popValues$mPopBar ^ 2)))
                      * (sum(ySETempNum) / (nSamp[[1]] - 1)))) %>%
    mutate(highCL = yBar + 2 * ySE) %>% # for 95% confidence interval
    mutate(lowCL = yBar - 2 * ySE)

  clusterSummary <- merge(
    merge(
      summarize(sampValues,
                nSamp = nSamp[[1]],
                mSampBar = mSampBar[[1]]),
      popValues
    ),
    summarize(finalCalc,
              standardError = ySE[[1]],
              upperLimitCI = highCL[[1]],
              lowerLimitCI = lowCL[[1]])
  )



  # return dataframe of stand-level statistics
  return(clusterSummary)

}
