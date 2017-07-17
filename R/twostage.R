#' @title Summarize two-stage sample
#' @description Summarizes population-level statistics for
#' two-stage sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean.
#' @param data data frame containing observations of variable of
#' interest for either cluster-level of plot-level data.
#' @param plot logical TRUE if parameter data is plot-level, FALSE if
#' parameter data is cluster-level. Default is TRUE.
#' @param attribute character name of attribute to be summarized.
#' @param populationClusters numeric total number of clusters in the 
#' population.
#' @param populationElementsPerCluster numeric total number of
#' elements in the population.
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9).
#' @return dataframe of stand-level statistics including
#' standard error and confidence interval limits. All final values are
#' on a 'per plot' basis.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' dataCluster <- data.frame(clusterID = c(1, 2, 3, 4, 5),
#'                           totClusterElements = c(5, 2, 1, 6, 5),
#'                           sampledElements = c(2, 2, 2, 2, 2),
#'                           isUsed = c(T, T, T, T, F),
#'                           attrSumCluster = c(1000, 1250, 950, 900, 1005))
#' summarize_two_stage(dataCluster, F, 'attr')
#'
#' # realistic data
#' dataPlot <- data.framedata.frame(clusterID = c(1, 1, 1, 2, 2, 2, 3, 3, 3,  
#'                                            4, 4, 4, 5, 5, 5, 6, 6, 6), 
#'                              volume = c(500, 650, 610, 490, 475, 505, 
#'                                         940, 825, 915, 210, 185, 170, 
#'                                         450, 300, 500, 960, 975, 890),
#'                              isUsed = c(T, T, T, T, T, T, T, T, T, T, 
#'                                         T, T, T, T, T, T, T, T))
#' summarize_two_stage(redData, T, 'volume', populationClusters = 16,
#'                     populationElementsPerCluster = 160)
#' 
#' }
#' @export


summarize_two_stage <- function(data, plot = TRUE, attribute = NA,
                                populationClusters = 0, populationElementsPerCluster = 0,
                                desiredConfidence = 0.95) {

  if (!is.na(attribute) && (attribute %in% colnames(data))) {

    attrTemp <- unlist(data %>% dplyr::select(one_of(attribute)))

    if (plot) {

      data$attr <- attrTemp

    } else {

      data$attrSumCluster <- attrTemp

    }

  }


  if (plot) {

    # calculates cluster values from plot data
    temp <- data %>%
      mutate(attr = ifelse(is.na(attr), 0, attr)) %>%
      mutate(attrSq = attr ^ 2)

    # sum attributes by cluster
    attrSum <- aggregate(temp$attr, by = list(clusterID = temp$clusterID), FUN = sum)
    attrSqSum <- aggregate(temp$attrSq, by = list(clusterID = temp$clusterID), FUN = sum)

    # checks if any secondary is used in any primary/cluster
    clusterUsed <- aggregate(data$isUsed, by = list(clusterID = data$clusterID), FUN = sum)
    clusterUsed$x <- ifelse(clusterUsed$x > 0, T, F) # converts above to T/F

    totalElementsInCluster <- count(data, clusterID) # tally of all elements in each cluster
    sampledElementsInCluster <- count(data[data$isUsed,], clusterID) # tally of elements sampled in each cluster

    # attach the sum of attributes and tally of elements to unique cluster
    # produce table with key values
    cluster <- full_join(clusterUsed, attrSum, by = 'clusterID') %>%
      full_join(totalElementsInCluster, by = 'clusterID') %>%
      rename(totClusterElements = n, isUsed = x.x, attrSumCluster = x.y) %>%
      full_join(sampledElementsInCluster, by = 'clusterID') %>%
      select(clusterID, totClusterElements, sampledElements = n, isUsed, attrSumCluster) %>%
      mutate(sampledElements = ifelse(is.na(sampledElements), 0, sampledElements)) %>%
      mutate(attrSqSumCluster = attrSqSum$x)

  } else {

    # reassigns data as cluster, if input data is cluster-level data
    # data must include unique ID ('clusterID'), number of cluster elements
      # in each cluster ('totClusterElements'), number of cluster elements
      # sampled in each cluster ('sampledElements'), logical true if the
      # cluster has any sampled elements ('isUsed'), sum of attributes in
      # each cluster ('attrSumCluster')
    cluster <- data %>%
      mutate(attrSqSumCluster = attrSumCluster ^ 2 )
    
  }


  if (as.integer(anyDuplicated(cluster$clusterID)) == 1) {
    stop("Data cannot have repeated clusterID.")
  }

  if (length(cluster$clusterID) == 1) {
    stop("Must have multiple clusters. Consider other analysis.")
  }

  n = sum(cluster$isUsed) # number of sampled clusters 
  m = sum(cluster$sampledElements) / n # average number of sampled plots among sampled clusters
  EN = ifelse(populationClusters != 0, populationClusters, length(cluster$isUsed)) # total number of clusters
  EM = ifelse(populationElementsPerCluster != 0, populationElementsPerCluster,
            sum(cluster$totClusterElements)) # total number of total plots among all clusters
  
  finalCalc <- cluster %>%
    summarize(
      # yBar denominator written for clarity: average m per cluster * n
      yBar = sum(attrSumCluster) / (m * n), 
      s2b = ((sum(attrSumCluster ^ 2) / (m)) -
                      (sum(attrSumCluster) ^ 2 / (m * n))) / (n - 1), 
      s2w = (sum(attrSqSumCluster) - sum(attrSumCluster ^ 2) / (m)) / (n * (m - 1)),
      df = sum(sampledElements)
      ) %>%
    mutate(ySE = ifelse(length(unique(cluster$totClusterElements)) == 1,
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

                                      # SE calculated below is approximated based on available 
                                        # methods and may not be accurate
                                      # These statements are intended to catch all other samples
                                      if (n / EN < m / EM) {

                                        sqrt(s2b ^ 2 / (m * n))

                                      } else {

                                        sqrt(1 / (m * n) * (s2b * (1 - n / EN) + (n * s2w) / EN))

                                      }
                               )
                        )
                 )
    ) %>%
    mutate(highCL = yBar + qt(1 - ((1 - desiredConfidence) / 2), df) * ySE) %>%
    mutate(lowCL = yBar - qt(1 - ((1 - desiredConfidence) / 2), df) * ySE)

  clusterSummary <- finalCalc %>%
    summarize(mean = yBar, varianceB = s2b, varianceW = s2w, standardError = ySE,
              upperLimitCI = highCL, lowerLimitCI = lowCL)

  # return dataframe of stand-level statistics
  return(clusterSummary)

}

