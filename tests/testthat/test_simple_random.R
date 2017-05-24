context("Forest sampling statistics calculations: simple random sample")

testTrees <- system.file('data', 'ph_trees.csv', package = 'forestSampling')
testPlots <- system.file('data', 'plots.csv', package = 'forestSampling')

y = c(5, 6, 3, 1, 8, 10)





test_that("simple random: popN and infReplacement function correctly", {

  sampT <- summarize_simple_random(y, popN = 50, infReplacement = T, desiredConfidence = 0.9, post = T)
  sampNA <- summarize_simple_random(y, popN = NA, infReplacement = T, desiredConfidence = 0.9, post = T)
  samp0 <- summarize_simple_random(y, popN = 0, infReplacement = T, desiredConfidence = 0.9, post = T)
  sampF <- summarize_simple_random(y, popN = 50, infReplacement = F, desiredConfidence = 0.9, post = T)
  expect_equal(sampNA, sampT)
  expect_error(samp0)

  expect_equal(sampF, data.frame('mean' = 5.5, 'variance' = 39.16, 'standardError' = 2.39,
                                 'upperLimitCI' = 10.32, 'lowerLimitCI' = 0.67),
                                 tolerance = 0.1)

  y2 <- c(4)
  samp2 <- summarize_simple_random(y2, popN = 50, infReplacement = T, desiredConfidence = 0.9, post = T)
  expect_equal(sampT, samp2)
  y3 <- c(NA)
  samp3 <- summarize_simple_random(y3, popN = 50, infReplacement = T, desiredConfidence = 0.9, post = T)
  expect_equal(samp2, samp3)

})
