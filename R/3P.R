

# post sampling - pg 601
Parameters:
  sampSize = 0
  treeCount = 0
  BAF = 0
  cvPercent = 50 # (average) should probably calculate within, if possible
  #data <- VBARS #vector, volume to basal area ratio (this should be estimateVBAR, )
  data <- data.frame(plotNum, VBARS, treeHeight) # vector
  #height <- FALSE
  trueNetVBAR = 0

  data <- data.frame(plotNum = c(5, 4, 3, 5, 4),
                     VBARS = c(9, 8, 7, 8, 2),
                     treeHeight = c(1, 2, 3, 4, 5))

summarize_threeP <- function(data, sampSize, treeCount, BAF, cvPercent, trueNetVBAR,
                             height = FALSE, desiredConfidence = 0.95) {

    #if the data input is height, VBAR will be estimated by 2 * tree height
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
  fullData <- full_join(avgVBAR, origData) %>%
    mutate(estPlotVol = treeCount * BAF * avgTreeVBARSforPlot) %>%
    mutate(avgVol = mean(estPlotVol)) %>%
    mutate(ratio = trueNetVBAR / estimateVBAR) %>%
    #mutate(cv = cv(ratio)) %>%
    mutate(netVolume = avgVol * ratio) %>%
    mutate(sePercent = cvPercent / sqrt(sampSize)) %>%
    mutate(se = sePercent * netVolume) # 'multiply SE% by volume to get SE units'
    
    return()

}






