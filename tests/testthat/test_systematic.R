context("Forest sampling statistics calculations: systematic sample")

trainingData <- data.frame(bapa = c(120, 140, 160, 110, 100, 90),
                           plots = c(1, 2, 3, 4, 5, 6))


test_that("systematic functions correctly with infiniteReplacement default", {

  systematic <- summarize_systematic(trainingData, attribute = 'bapa', popSize = 50, desiredConfidence = 0.9)
  simple <- summarize_simple_random(trainingData, attribute = 'bapa', popSize = 50, desiredConfidence = 0.9, 
                                    infiniteReplacement = F)
  expect_equal(systematic, simple)

})


test_that("systematic functions correctly with vector and data frame input", {

  dataframe <- summarize_systematic(trainingData, attribute = 'bapa', popSize = 50,
                                    desiredConfidence = 0.9)
  vector <- summarize_systematic(c(120, 140, 160, 110, 100, 90), popSize = 50,
                                 desiredConfidence = 0.9)

  expect_equal(dataframe, vector)

})
