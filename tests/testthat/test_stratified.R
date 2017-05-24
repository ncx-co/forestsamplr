context("Forest sampling statistics calculations: stratified sample")



trainingData <- data.frame(bapa = c(120, 140, 160, 110, 100, 90),
                              stratum = c(1, 1, 1, 2, 2, 2))
stratumTab <- data.frame(stratum = c(1, 2), acres = c(200, 50))
attribute = 'bapa'
desiredConfidence = 0.9

stratSum = data.frame("stratum" = c(1, 2), "stratMean" = c(140, 100),
                      #"stratVarTot" = c(5333333.33, 83333.33),
                      "stratVarMean" = c(133.33, 33.33), #I don't understand how this is in the output
                      "stratSE" = c(11.547, 5.773), "stratPlots" = c(3, 3),
                      "stratAcres" = c(200, 50))
totSum = data.frame("popMean" = c(132), "popSE" = c(9.30),
                    "popCIhalf" = c(19.84), "ciPct" = c(15.03))
finalSum = list('stratumSummaries' = data.frame(stratSum),
                'totalSummary' = data.frame(totSum))




test_that("stratified functions correctly", {
  strat <- summarize_stratified(trainingData, attribute,stratumTab,
                                desiredConfidence = 0.9, post = T)
  expect_equal(strat,
               finalSum, tolerance = 0.1)
  expect_equal(strat$stratumSummaries,
               stratSum, tolerance = 0.1)
  expect_equal(strat$totalSummary,
               totSum, tolerance = 0.1)
})
