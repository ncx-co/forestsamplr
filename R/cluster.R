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


  # BELOW DOESN'T WORK UNTIL LINE 96 - do not use.

uniqueAttr <- data.frame(unique(trainingData$clusterID), sumOfAttr <- c(0))


  trainingData <- data.frame('m' = c(5, 7, 3, 90), #number of elements in one cluster
             'clusterID' = c(1, 2, 1, 5), #distinct clusters
             'isUsed' = c(T, T, T, F), #whether or not the cluster is included in the sample
             'attr' = c(10, 15, 20, 80) #attribute, or 'y'
             ) %>%
    arrange(clusterID) %>%
    group_by(clusterID) %>%
    mutate(sum(attr))
    summarise(clusterID, sum(group_by(clusterIDattr)))
    abcd <- do(data.frame(unique(trainingData$clusterID), sum())



  trainingData %>%
    mutate(sum(attr, clusterID)))

    ###########################

#basic values: population-level
  popValues <- trainingData %>%
    summarize(mPop = sum(m),
              nPop = n(), #num clusters
              mPopBar = mPop / nPop)
    if (mPopBar = NA) { #if Mbar (pop) is unknown, approximate it with mbar (samp)
      mPopBar <- mSampBar
    }

#basic values: sample-level
  sampValues <- trainingData[trainingData$isUsed,]
  vals <- uniqueCluster = unique(sampValues$clusterID) %>%
    #mutate(mSum <- for (all.uniqueCluster) {sum(sampValues$m)})
    summarize(mSamp = sum(m),
              nSamp = n(), # num clusters
              mSampBar = mSamp / nSamp,
              yiSum = #sum of observations in a certain cluster


  values <- bind_cols(popValues, sampValues)

  clusterSummary <- trainingData[trainingData$isUsed,] %>%
    mutate(yBar = ySum / mSamp) %>%
    mutate(ySE = sqrt(((nPop - nSamp) / (nPop * nSamp * (mPopBar ^ 2))) #issue is here
               * (for_each(yValue - yBar * mVlaue) / (nSamp - 1)))) %>%
    mutate(highCL = yBar + 2 * SE) %>% #for 95% confidence interval
    mutate(lowCL = yBar - 2 * SE))

####################################

  #BELOW uses simplified input

  #no repeated cluster IDs in 'cluster df'
  cluster = data.frame(clusterID = c(1, 2, 3, 4, 5), clusterElements = c(4, 2, 9, 4, 10),
                       sumAttr = c(1000, 1250, 950, 900, 1005), isUsed = c(T, T, F, T, T))
 # plots = data.frame(clusterID = c(1, 1, 2, 3, 3, 4, 5),
#                     bapa = c(120, 140, 160, 100, 110, 100, 90))



  #basic values: sample-level
  sampValues <- cluster[cluster$isUsed,] %>%
    mutate(nSamp = n()) %>% # num clusters
    mutate(mSampBar = sum(clusterElements) / nSamp)



#basic values: population-level
  popValues <- cluster %>%
  summarize(mPop = sum(clusterElements),
            nPop = n(), #num clusters
            mPopBar = mPop / nPop) %>%
  if (mPopBar = NA) { #if Mbar (pop) is unknown, approximate it with mbar (samp)
    mPopBar <- sum(cluster$mSampBar) / sum(cluster$mSampBar)
  }


  # return dataframe of
  output <- clusterSummary

  return(output)

}



