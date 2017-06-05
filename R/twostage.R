#' @title Summarize two-stage sample
#' @description Summarizes population-level statistics for
#' two-stage sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean.
#' @param data dataframe containing observations of variable of
#' interest for either cluster-level of plot-level data.
#' @param plot logical True if parameter data is plot-level, False if
#' parameter data is cluster-level. Default is True.
#' @param attriute character name of attribute to be summarized.
#' @return dataframe of stand-level statistics including
#' standard error and confidence interval limits. All final values are
#' on a 'per plot' basis.
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
#' data <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
#'                    attr = c(1000, 1250, NA, 900, 1005, 1000, 1250, 950, 900, 1005, NA, 1250, 950, 900, 1005,
#'                    1000, 1250, NA, 900),
#'                    isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))
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
    temp <- data %>%
      mutate(attr = ifelse(is.na(attr), 0, attr)) %>%
      mutate(attrSq = attr ^ 2)
    attrSum <- aggregate(temp$attr, by = list(Category = temp$clusterID), FUN = sum) # sum attributes by cluster
    attrSqSum <- aggregate(temp$attrSq, by = list(Category = temp$clusterID), FUN = sum)

    clusterUsed <- aggregate(data$isUsed, by = list(Category = data$clusterID), FUN = sum) #see if any secondary is used in any primary/cluster
    clusterUsed$x <- ifelse(clusterUsed$x > 0, T, F) #convert above to T/F

    totElementsInPrimaryM <- count(data, clusterID) # tally of elements for each cluster
    sampElementsInPrimarym <- count(data[data$isUsed,], clusterID) # tally of elements used for each cluster

    # attach the sum of attributes and tally of elements to unique cluster
    # produce table with key values
    cluster <- merge(clusterUsed, attrSum, by.x = "Category", by.y = "Category", all = TRUE) %>%
      merge(totElementsInPrimaryM, by.x = "Category", by.y = "clusterID", all = TRUE) %>%
      rename(totClusterElements = n, clusterID = Category) %>%
      merge(sampElementsInPrimarym, by.x = "clusterID", by.y = "clusterID", all = TRUE) %>%
      select(clusterID = clusterID, totClusterElements = totClusterElements, sampledElements = n, isUsed = x.x, attrSumCluster = x.y) %>%
      mutate(sampledElements = ifelse(is.na(sampledElements), 0, sampledElements)) %>%
      mutate(attrSqSumCluster = attrSqSum$x)

  } else {

    # reassigns data as cluster, if input data is cluster-level data
    # data must include unique ID ('clusterID'), number of cluster elements
      # in each cluster ('totClusterElements'), number of cluster elements
      # sampled in each cluster ('sampledElements'), logical true if the
      # cluster has any sampled elements ('isUsed'), sum of attributes in
      # each cluster ('attrSumCluster'), squared sum of attributes in each
      # cluster ('attrSqSumCluster')
    cluster <- data

  }


  if (as.integer(anyDuplicated(cluster$clusterID)) == 1) {
    stop("Data cannot have repeated clusterID.")
  }

  if (length(cluster$clusterID) == 1) {
    stop("Must have multiple clusters. Consider other analysis.")
  }

  n <- sum(cluster$isUsed)
  EN <- length(cluster$isUsed)
  m <- sum(cluster$sampledElements)
  EM <- sum(cluster$totClusterElements)

  tempCalc <- cluster %>%

  # df = clusterID, primary (clusters), secondary tot (plot sum), attrSumCluster, attrSqSumCluster
    mutate(yBar = sum(attrSumCluster) / m) %>% # denominator written for clarity: average m per cluster * n
    mutate(s2b = ((sum(attrSumCluster ^ 2) / m) - (sum(attrSumCluster) ^ 2 / (m * n))) / (n - 1)) %>%
    mutate(s2w = (sum(attrSqSumCluster) - sum(attrSumCluster ^ 2) / m) / (n * (m - 1)))

  finalCalc <- data.frame(yBar = tempCalc$yBar[[1]], s2b = tempCalc$s2b[[1]],
                           s2w = tempCalc$s2w[[1]]) %>%
    mutate(ySE = ifelse(identical(cluster$totClusterElements, mean(cluster$totClusterElements)),
                        #if clusters are equal in size, num of elements per cluster is equal:
                        sqrt((1 / (m * n)) * ((s2b * (1 - n / EN) + n * s2w / EN * (1 - m / EM))[[1]])),

                        ifelse((n / EN < 0.2),
                               #if n is a small fraction of N, SE is simplified to:
                               sqrt(s2b ^ 2 / (m * n)),

                               ifelse((m / EM < 0.2),
                                      # when n/N is fairly large, but num of secondaries (m)
                                      # sampled in each selected primary is only a fraction
                                      # of the total secondaries (M) in each primary (n)
                                      sqrt(1 / (m * n) * (s2b * (1 - n / EN) + (n * s2w) / EN)),

                                      if (n / EN < m / EM) {

                                        # SE is approximated based on available methods and may not be accurate
                                        sqrt(s2b ^ 2 / (m * n))

                                      } else {

                                        # SE is approximated based on available methods and may not be accurate
                                        sqrt(1 / (m * n) * (s2b * (1 - n / EN) + (n * s2w) / EN))

                                      }
                               )
                        )
                 )
    ) %>%
    mutate(highCL = yBar + 2 * ySE) %>% # for 95% confidence interval
    mutate(lowCL = yBar - 2 * ySE)

    clusterSummary <- finalCalc %>%
      summarize(mean = yBar, varianceB = s2b, varianceW = s2w, standardError = ySE,
                upperlimitCI = highCL, lowerLimitCI = lowCL)

  # return dataframe of stand-level statistics
  return(clusterSummary)

}

