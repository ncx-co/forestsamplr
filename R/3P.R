

# post sampling - pg 601
Parameters:
  sampSize = 0
  treeCount = 0
  BAF = 0
  cvPercent = 50 # (average) should probably calculate within, if possible
  data <- VBARS #vector, volume to basal area ratio
  height <- FALSE
  trueNetVBAR = 0


summarize_poisson <- function(data, desiredConfidence = 0.95) {
  

    
    
    #if the data input is height, VBAR will be estimated by 2 * tree height
    if (height == TRUE) {
      origData <- data.frame(data) %>%
        mutate(estimateVBAR = height * 2)
    } else {
      origData <- data.frame(data) %>%
        rename(estimateVBAR = data)
    }
    
    estPlotVol <- treeCount * BAF * avgTreeVBARSforPlot
    avgVol <- avg(estPlotVol)
    ratio <- trueNetVBAR / estimateVBAR
    netVolume <- avgVol * ratio 
    sePercent <- cvPercent / sqrt(sampSize)
    se <- sePercent * netVolume # 'multiply SE% by volume to get SE units'
    
    return()

}






