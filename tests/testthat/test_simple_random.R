context("Forest sampling statistics calculations: simple random sample")

trainingData <- data.frame(bapa = c(120, 140, 160, 110, 100, 90),
                          plots = c(1, 2, 3, 4, 5, 6))
attribute <- 'bapa'


test_that("simple random function handles missing values and zeros correctly", {

  sampT <- summarize_simple_random(trainingData, attribute, popSize = 50,
                                   desiredConfidence = 0.9, infiniteReplacement = T)
  sampNA <- summarize_simple_random(trainingData, attribute, popSize = NA,
                                    desiredConfidence = 0.9, infiniteReplacement = T)
  sampNoN <- summarize_simple_random(trainingData, attribute, desiredConfidence = 0.9,
                                     infiniteReplacement = T)
  sampNoReplacement <- summarize_simple_random(trainingData, attribute, popSize = 50,
                                               desiredConfidence = 0.9)
  sampF <- summarize_simple_random(trainingData, attribute, popSize = 50,
                                   desiredConfidence = 0.9, infiniteReplacement = F)
  sampReplacementNA <- summarize_simple_random(trainingData, attribute, popSize = 50,
                                               desiredConfidence = 0.9, infiniteReplacement = NA)

  expect_equal(sampNA, sampT)
  expect_equal(sampNA, sampNoN)
  expect_equal(sampF, sampNoReplacement)
  expect_equal(sampF, sampReplacementNA)
  expect_error(summarize_simple_random(trainingData, attribute, popSize = 0,
                                       desiredConfidence = 0.9, infiniteReplacement = T))

  expect_equal(sampF, data.frame('mean' = 120, 'variance' = 14966.67, 'standardError' = 46.85,
                                 'upperLimitCI' = 214.40, 'lowerLimitCI' = 25.59), tolerance = 0.1)

})



test_that("simple random throws errors for data with few or no entries, or missing values", {

  trainingDataOne <- data.frame('bapa' = c(4), 'plot' = c(10))
  trainingDataThree <- data.frame('bapa' = c(4, 5, NA), 'plot' = c(10, 11, 12))
  trainingDataNone <- data.frame('bapa' = c(NA), 'plot' = c(10))

  expect_warning(summarize_simple_random(trainingDataOne, attribute, popSize = 50,
                                         desiredConfidence = 0.9, infiniteReplacement = T))
  expect_error(summarize_simple_random(trainingDataNone, attribute, popSize = 50,
                                         desiredConfidence = 0.9, infiniteReplacement = T))
  expect_error(summarize_simple_random(trainingDataThree, attribute, popSize = 50,
                                       desiredConfidence = 0.9, infiniteReplacement = T))

})



test_that("simple random processes data with two or more entries", {

  trainingDataTwo <- data.frame('bapa' = c(4, 5), 'plot' = c(10, 11))
  trainingDataThree <- data.frame('bapa' = c(4, 5, 7), 'plot' = c(10, 11, 12))

  summarize_simple_random(trainingDataThree, attribute, popSize = 50,
                          desiredConfidence = 0.9, infiniteReplacement = T)

  sampTwo <- summarize_simple_random(trainingDataTwo, attribute, popSize = 50,
                                     desiredConfidence = 0.9, infiniteReplacement = T)

  expect_equal(sampTwo$mean,  mean(trainingDataTwo[[1]]))

})



