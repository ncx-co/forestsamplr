#' @title Summarize 3P sample
#' @description Summarizes population-level statistics for
#' 3P sample data. The calculations are derived from Kim Iles
#' 'A Sampler of Inventory Topics', pg 601.
#' @param sampSize numeric 
#' @param treeCount numeric
#' @param BAF numeric basal area factor used for sampling.
#' @param cvPercent numeric average Coefficient of Variation 
#' expressed as a percent (e.g. 50)
#' @param data data frame containing plot number, VBARs, and 
#' treeHeights
#' @param height Boolean indicates that instead of the 
#' data containing treeHeight, it contains estimateVBAR
#' @param trueNetVBAR numeric measured net VBAR (volume to basal
#' area ratio)
#' @param desiredConfidence numeric desired confidence level 
#' (e.g. 0.9).
#' @return data frame of statistics including standard error and 
#' confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' data <- data.frame(plotNum = c(5, 4, 3, 5, 4),
#'                    VBARS = c(9, 8, 7, 8, 2),
#'                    treeHeight = c(1, 2, 3, 4, 5))
#' }
#' @export

summarize_threeP <- function(data, sampSize, treeCount, BAF, cvPercent, trueNetVBAR,
                             height = FALSE, desiredConfidence = 0.95) {

    # if the data input is height, VBAR will be estimated by 2 * tree height
    if (height == TRUE) {
      origData <- data %>%
        mutate(estimateVBAR = treeHeight * 2)
    } else {
      origData <- data %>%
        rename(estimateVBAR = data)
    }
  
  avgVBAR <- origData %>%
    group_by(plotNum) %>%
    mutate(avgTreeVBARSforPlot = mean(VBARS)) %>%
    select(plotNum, avgTreeVBARSforPlot) %>%
    unique()
  
  # merge data and avgVBAR
  fullData <- full_join(origData, avgVBAR) %>%
    mutate(estPlotVol = treeCount * BAF * avgTreeVBARSforPlot) %>%
    summarize(avgVol = mean(estPlotVol), 
              ratio = trueNetVBAR / mean(estimateVBAR),
              netVolume = avgVol * ratio,
              sePercent = cvPercent / sqrt(sampSize),
              se = sePercent * netVolume, # 'multiply SE% by volume to get SE units'
              lowerBoundCI = avgVol - qnorm(1 - (1 - desiredConfidence) / 2)
                  * sqrt(avgVol / sampSize[[1]]),
              upperBoundCI = avgVol + qnorm(1 - (1 - desiredConfidence) / 2) 
                  * sqrt(avgVol / sampSize[[1]])
    )
    
    return(fullData)

}






