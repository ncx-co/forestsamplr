#' @title Summarize 3P sample
#' @description Summarizes population-level statistics for
#' 3P sample data. The calculations are derived from Kim Iles
#' 'A Sampler of Inventory Topics', pg 601.
#' @param data data frame containing plot number, VBARs, and treeHeights.
#' @param cvPercent numeric average Coefficient of Variation expressed 
#' as a percent (e.g. 50).
#' @param trueNetVBAR numeric measured net VBAR (volume to basal area 
#' ratio).
#' @param height logical TRUE if data input contains height values. 
#' Otherwise, FALSE assumes data contains estimateVBAR.
#' @param plot logical TRUE if the input data is averaged by plot, 
#' FALSE if data is tree-level.
#' @param desiredConfidence numeric desired confidence level (e.g. 0.9).
#' @return data frame of statistics including standard error and 
#' confidence interval limits.
#' @author Karin Wolken
#' @import dplyr
#' @examples
#' \dontrun{
#' dataHeight <- data.frame(plotNum = c(5, 4, 3, 5, 4),
#'                          treeCount = c(4, 5, 6, 3, 4),
#'                          BAF = c(10, 10, 10, 10, 10),
#'                          avgTreeVBARSperPlot = c(9, 8, 7, 8, 2),
#'                          treeHeight = c(1, 2, 3, 4, 5))
#' summarize_threeP(dataHeight, cvPercent = 50, trueNetVBAR = 10, height = T, plot = T)
#' 
#' dataEstimateVBAR <- data.frame(plotNum = c(1, 2, 3, 4, 5),
#'                                treeCount = c(4, 5, 6, 3, 4),
#'                                BAF = c(10, 10, 10, 10, 10),
#'                                avgTreeVBARSperPlot = c(9, 8, 7, 8, 2),
#'                                estimateVBAR = c(1, 2, 3, 4, 5))
#' summarize_threeP(dataEstimateVBAR, cvPercent = 50, trueNetVBAR = 10, height = F, plot = T)
#'                                     
#' dataNotPlot <- data.frame(plotNum = c(1, 1, 2, 2, 3),
#'                           BAF = c(10, 10, 10, 10, 10),
#'                           VBAR = c(1, 2, 3, 4, 3),
#'                           estimateVBAR = c(1, 2, 3, 4, 5))
#' summarize_threeP(dataNotPlot, cvPercent = 50, trueNetVBAR = 10, height = F, plot = F)
#' 
#' }
#' @export
#'

summarize_threeP <- function(data, cvPercent, trueNetVBAR,
                             height = FALSE, plot = FALSE, desiredConfidence = 0.95) {
  
    # if the data input is height, VBAR will be estimated by 2 * tree height
    if (height) {
      origData <- data %>%
        mutate(estimateVBAR = treeHeight * 2)
    } else {
      origData <- data
    }
  
  if (!plot) {
    avgVBAR <- origData %>%
      group_by(plotNum) %>%
      summarize(avgTreeVBARSperPlot = mean(VBAR),
                treeCount = length(plotNum),
                BAF = mean(BAF),
                estimateVBAR = mean(estimateVBAR))
  }
  
  if (plot) {
    fullData <- origData
  } else {
    fullData <- data.frame(avgVBAR)
  }
  
  
  # merge data and avgVBAR
  summary <- fullData %>%
    mutate(estPlotVol = treeCount * BAF * avgTreeVBARSperPlot) %>%
    summarize(sampSize = length(plotNum), # number of plots
              avgVol = mean(estPlotVol), 
              ratio = trueNetVBAR / mean(estimateVBAR),
              netVolume = avgVol * ratio,
              sePercent = cvPercent / sqrt(sampSize),
              se = sePercent * netVolume, # 'multiply SE% by volume to get SE units'
              lowerBoundCI = avgVol - qnorm(1 - (1 - desiredConfidence) / 2)
                  * sqrt(avgVol / sampSize),
              upperBoundCI = avgVol + qnorm(1 - (1 - desiredConfidence) / 2) 
                  * sqrt(avgVol / sampSize)
              )
    
    return(summary)

}
