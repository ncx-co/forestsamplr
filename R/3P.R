
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

# post sampling - pg 601
sampSize = 0
treeCount = 0
BAF = 0

estPlotVol <- treeCount * BAF * avgTreeVBARSforPlot
avgVol <- avg(estPlotVol)
ratio <- trueNetVBAR / estimateVBAR
netVolume <- avgVol * ratio 
se <- # here, pg 601 #5






