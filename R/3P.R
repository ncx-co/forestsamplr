
data <- c(estimated = 5,
          measured = 5.5,
          calculated = 6)


working <- data %>%
  mutate(corrRatio = measurement / estimate) %>%
  mutate(avgRatio = sum(corrRatio) / n) %>% # number of trees(units) that have a measured value
  mutate(totalVol = sum(estimated) * avgRatio) %>% # units must match...
  mutate(SEcombined = )




Jul 10, 2017

# Point - 3P (tree) method

#pre sampling
plots <- 150
treesPerPlot <- 6
treeHt <- 140 #ft
#VBAR in bd ft per sq ft
#VBAR estimated by 2 * treeHt
measurements <- 60
sumEstVBARs <- plots * treesPerPlot * (treeHt * 2)
KZ <- sumEstVBARs / measurements

# # # # # # # # # # # # # # # # HERE
# post sampling - pg 601
sampSize = 0
treeCount = 0
BAF = 0
cvPercent = 50 # should probably calculate within, if possible
data <- VBARS #vector, volume to basal area ratio
height <- FALSE
trueNetVBAR = 0


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
se <- sePercent * netVolume # multiply SE% by volume to get SE units







