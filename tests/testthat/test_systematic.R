context("Forest sampling statistics calculations: simple random sample")

trainingData <- data.frame(bapa = c(120, 140, 160, 110, 100, 90),
                          plots = c(1, 2, 3, 4, 5, 6))
attribute <- 'bapa'


test_that("systematic functions correctly", {

  systematic <- summarize_systematic(trainingData, attribute, popSize = 50, desiredConfidence = 0.9)
  simple <- summarize_simple_random(trainingData, attribute, popSize = 50, desiredConfidence = 0.9, infiniteReplacement = F)
  expect_equal(systematic, simple)

})
