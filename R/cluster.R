#' @title Summarize cluster sample
#' @description Summarizes population-level statistics for
#' cluster sample data. The calculations are derived from Chapter 3 in
#' Avery and Burkhart's (1967) Forest Measurements, Fifth Edition. The
#' variance terms refer to the variance of the mean.
#' @param data dataframe containing observations of variable of
#' interest for either cluster-level or plot-level data.
#' @param plot logical true if parameter data is plot-level, false if
#' parameter data is cluster-level. Default is True.
#' @param attribute character name of attribute to be summarized.
#' @return dataframe of stand-level statistics including
#' standard error and confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' dataPlot <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
#'                        attr = c(1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005, 1000,
#'                                 1250, 950, 900, 1005, 1000, 1250, 950, 900),
#'                        isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))
#' plot = TRUE
#' attribute = attr
#' }
#' @export


summarize_cluster <- function(data, plot = TRUE, attribute = NA) {

  # ensure the data entered does not have missing values
  if (any(is.na(data))) {
    stop("Data input without NA values is required.")
  }

  # give the variable of interest a generic name
  if (!is.na(attribute) && (attribute %in% colnames(data))) {
    attrTemp <- unlist(data %>% dplyr::select(one_of(attribute)))

    if (plot) {
      # change variable of interest name to attr, unsummarized
      data$attr <- attrTemp

    } else {
      # change variable of interest name to sumAttr, summarized by element
      data$sumAttr <- attrTemp

    }

  }

if (plot) {

  # calculates cluster values from plot data

  cluster <- data %>%
    group_by(clusterID) %>%
    summarize(sumAttr = sum(attr), # sum attributes by cluster
              clusterElements = n()) %>% # tally of elements in each cluster
    left_join(distinct(data, clusterID, .keep_all = TRUE)) # maintain isUsed for each cluster

} else {

  # reassigns data as cluster, if input data is cluster-level data
  cluster <- data

}

  if (length(cluster$clusterID) == 1) {

    stop("Must have multiple clusters. Consider other analysis.")

  }

  # basic values: sample-level
  sampValues <- cluster %>%
    filter(isUsed == T) %>%
    mutate(nSamp = n()) %>% # num clusters
    mutate(mSampBar = sum(clusterElements) / nSamp) # avg num elements in a cluster

  # basic values: population-level
  popValues <- cluster %>%
  summarize(mPop = sum(clusterElements),
            nPop = n(), # num clusters
            mPopBar = mPop / nPop)
  if (is.na(popValues$mPopBar) | popValues$mPopBar == sampValues$mSampBar[[1]]) {
    # if Mbar (pop) is unknown, approximate it with mbar (samp)

    popValues$mPopBar <- sum(sampValues$mSampBar[[1]])

  }
  
  clusterSummary <- sampValues %>%
    mutate(yBar = sum(sumAttr) / sum(clusterElements)) %>%
    mutate(ySETempNum = (sumAttr - yBar * clusterElements) ^ 2) %>%
    summarize(ySE = sqrt(((popValues$nPop - nSamp[[1]]) /
                            (popValues$nPop * nSamp[[1]] * (popValues$mPopBar ^ 2)))
                         * (sum(ySETempNum) / (nSamp[[1]] - 1))),
              yBar = mean(yBar),
              nSamp = mean(nSamp),
              mSampBar = mean(mSampBar)) %>%
    mutate(highCL = yBar + qt(1 - ((1 - desiredConfidence) / 2), length(attr) - 1) * ySE) %>% # for 95% confidence interval
    mutate(lowCL = yBar - qt(1 - ((1 - desiredConfidence) / 2), length(attr) - 1) * ySE) %>%
    select(standardError = ySE, lowerLimitCI = lowCL, upperLimitCI = highCL, 
           mean = yBar, nSamp, mSampBar) %>%
    bind_cols(popValues)

  # return dataframe of stand-level statistics
  return(clusterSummary)

}

