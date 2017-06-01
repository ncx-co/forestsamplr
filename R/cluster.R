#' @title Summarize cluster sample
#' @description Summarizes population-level statistics for
#' cluster sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean.
#' @param data dataframe containing observations of variable of
#' interest for either cluster-level of plot-level data.
#' @param plot logical true if parameter data is plot-level, false if
#' parameter data is cluster-level. Default is True.
#' @param attriute character name of attribute to be summarized.
#' Defaults to 'attr'.
#' @return dataframe of cluster-level statistics including clusterID,
#' standard error, and confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' data <- data.frame(clusterID = c(1, 2, 3, 4, 5),
#'                      clusterElements = c(4, 2, 9, 4, 10),
#'                      sumAttr = c(1000, 1250, 950, 900, 1005),
#'                      isUsed = c(T, T, F, T, T))
#'
#' data <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
#'                     attr = c(1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005,
#'                     1000, 1250, 950, 900),
#'                     isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))
#' }
#' @export


#wish list:
#Allow for different confidence levels
#test to see if 'attrbute = NA' concept works


summarize_cluster <- function(data, plot = TRUE, attribute = NA) { # attribute = 'attr',
  # other column headers are not generallized within the function

  if (any(is.na(data))) {
    stop("Data input without NA values is required.")
  }

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
  attrSum <- aggregate(data$attr, by = list(Category = data$clusterID), FUN = sum) # sum attributes by cluster

  clusterT <- distinct(data, clusterID, .keep_all = TRUE) # maintain T/F isUsed for each cluster
  elements <- count(data, clusterID) # tally of elements in each cluster

  # attach the sum of attributes and tally of elements to unique cluster
  # produce table with key values
  cluster <- merge(clusterT, attrSum, by.x = "clusterID", by.y = "Category", all = TRUE) %>%
    merge(elements, by.x = "clusterID", by.y = "clusterID", all = TRUE) %>%
    select(clusterID = clusterID, clusterElements = n, isUsed = isUsed, sumAttr = x)

} else {

  #reassigns data as cluster, if input data is already totalled by cluster
  cluster <- data

}

  if (as.integer(anyDuplicated(cluster$clusterID)) == 1) {
    stop("Data cannot have repeated clusterID.")
  }

  if (length(cluster$clusterID) == 1) {
    stop("Must have multiple clusters. Consider other analysis.")
  }

  # basic values: sample-level
  sampValues <- cluster[cluster$isUsed,] %>%
    mutate(nSamp = n()) %>% # num clusters
    mutate(mSampBar = sum(clusterElements) / nSamp) # avg num elements in a cluster

  # basic values: population-level
  popValues <- cluster %>%
  summarize(mPop = sum(clusterElements),
            nPop = n(), # num clusters
            mPopBar = mPop / nPop)
  if (is.na(popValues$mPopBar) | popValues$mPopBar == 0) { # if Mbar (pop) is unknown, approximate it with mbar (samp)
    popValues$mPopBar <- sum(sampValues$mSampBar[1]) / sum(sampValues$mSampBar[1])
  }

  finalCalc <- sampValues %>%
    mutate(yBar = sum(sumAttr) / sum(clusterElements)) %>%
    mutate(ySETempNum = (sumAttr - yBar * clusterElements) ^ 2) %>%
    mutate(ySE = sqrt(((popValues$nPop - nSamp) / (popValues$nPop * nSamp * (popValues$mPopBar ^ 2)))
                      * (sum(ySETempNum) / (nSamp - 1)))) %>%
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



