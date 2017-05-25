context("Forest sampling statistics calculations: simple random sample")

#testTrees <- system.file('data', 'ph_trees.csv', package = 'forestSampling')
#testPlots <- system.file('data', 'plots.csv', package = 'forestSampling')

trainingData = data.frame(bapa = c(120, 140, 160, 110, 100, 90),
                          plots = c(1, 2, 3, 4, 5, 6))
attribute = 'bapa'


test_that("simple random functions correctly", {

  sampT <- summarize_simple_random(trainingData, attribute, popN = 50, desiredConfidence = 0.9, infReplacement = T)
  sampNA <- summarize_simple_random(trainingData, attribute, popN = NA, desiredConfidence = 0.9, infReplacement = T)
  #sampNoN <- summarize_simple_random(trainingData, attribute, desiredConfidence = 0.9, infReplacement = T)
  sampF <- summarize_simple_random(trainingData, attribute, popN = 50, desiredConfidence = 0.9, infReplacement = F)
  sampReplacementNA <- summarize_simple_random(trainingData, attribute, popN = 50, desiredConfidence = 0.9, infReplacement = NA)
  expect_equal(sampNA, sampT)
  expect_equal(sampF, sampReplacementNA)
  expect_error(summarize_simple_random(trainingData, attribute, popN = 0, desiredConfidence = 0.9, infReplacement = T))

  expect_equal(sampF, data.frame('mean' = 120, 'variance' = 14966.67, 'standardError' = 46.85,
                                 'upperLimitCI' = 214.40, 'lowerLimitCI' = 25.59),
                                 tolerance = 0.1)

  # attribute has few or no entries
  trainingDataOne <- data.frame('bapa' = c(4), 'plot' = c(10))
  trainingDataTwo <- data.frame('bapa' = c(4, 5), 'plot' = c(10, 11))
  trainingDataThree <- data.frame('bapa' = c(4, 5, NA), 'plot' = c(10, 11, 12))
  trainingDataNone <- data.frame('bapa' = c(NA), 'plot' = c(10))
  expect_warning(summarize_simple_random(trainingDataOne, attribute, popN = 50, desiredConfidence = 0.9, infReplacement = T))
  expect_warning(summarize_simple_random(trainingDataNone, attribute, popN = 50, desiredConfidence = 0.9, infReplacement = T))
  sampTwo <- summarize_simple_random(trainingDataTwo, attribute, popN = 50, desiredConfidence = 0.9, infReplacement = T)
  sampThree <- summarize_simple_random(trainingDataThree, attribute, popN = 50, desiredConfidence = 0.9, infReplacement = T)
  expect_false(is.na(sampThree$lowerLimitCI))
  expect_equal(sampTwo$mean,  mean(trainingDataTwo[[1]]))


})
