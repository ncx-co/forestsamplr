#' @title Summarize cluster sample
#' @description Summarizes population-level statistics for
#' cluster sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean.
#' @param clusterLevel dataframe containing observations of variable of
#' interest by cluster. Attribute is the total for the cluster.
#' @param plotLevel dataframe containing observations of the variable
#' of interest by plot. Attribute is the total for each individual
#' plot (cluster element).
#' @param attriute character name of attribute to be summarized.
#' Defaults to 'attr'.
#' @return dataframe of cluster-level statistics including clusterID,
#' standard error, and confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' clusterLevel <- data.frame(clusterID = c(1, 2, 3, 4, 5),
#'                      clusterElements = c(4, 2, 9, 4, 10),
#'                      sumAttr = c(1000, 1250, 950, 900, 1005),
#'                      isUsed = c(T, T, F, T, T))
#'
#' plotLevel <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
#'                     attr = c(1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005,
#'                     1000, 1250, 950, 900),
#'                     isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))
#' }
#' @export


#wish list:
#Allow for different confidence levels
#could return stand-level statistics, as well



summarize_cluster <- function(clusterLevel = data.frame(c(NA), c(NA)), plotLevel = data.frame(c(NA), c(NA)), attribute = 'attr') {
  # other column headers are not generallized within the function

  if (!any(is.na(plotLevel))) { # calculates cluster values from plot data

    if (attribute %in% colnames(plotLevel)) {
      # give the variable of interest a generic name
      attrTemp <- unlist(plotLevel %>% dplyr::select(one_of(attribute))) # repeated below for 'cluster'
      plotLevel$attr <- attrTemp
    }

    attrSum <- aggregate(plotLevel$attr, by = list(Category = plotLevel$clusterID), FUN = sum) # sum attributes by cluster

    clusterT <- distinct(plotLevel, clusterID, .keep_all = TRUE) # maintain T/F isUsed for each cluster
    elements <- count(plotLevel, clusterID) # tally of elements in each cluster

    # attach the sum of attributes and tally of elements to unique cluster
    # produce table with key values
    cluster <- merge(clusterT, attrSum, by.x = "clusterID", by.y = "Category", all = TRUE) %>%
      merge(elements, by.x = "clusterID", by.y = "clusterID", all = TRUE) %>%
      select(clusterID = clusterID, clusterElements = n, isUsed = isUsed, sumAttr = x)

  } else if (!any(is.na(clusterLevel))) { # generalizes cluster attribute

    if (attribute %in% colnames(clusterLevel)) {
      # give the variable of interest a generic name
      attrTemp <- unlist(clusterLevel %>% dplyr::select(one_of(attribute))) # repeated above for 'plot'
      clusterLevel$attr <- attrTemp
      cluster <- clusterLevel
    }

  } else {
    stop("Data input without NA values is required.")
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



