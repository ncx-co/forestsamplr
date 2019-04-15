context("Forest sampling statistics calculations: systematic sample")

trainingData <- clusterBAData %>%
  filter(!is.na(bapa)) %>%
  group_by(clusterID) %>%
  sample_n(1) %>%
  ungroup() %>%
  rename(plots = clusterID)

test_that("systematic functions correctly with infiniteReplacement default", {

  systematic <- summarize_systematic(trainingData, attribute = 'bapa', popSize = 50, desiredConfidence = 0.9)
  simple <- summarize_simple_random(trainingData, attribute = 'bapa', popSize = 50, desiredConfidence = 0.9, 
                                    infiniteReplacement = F)
  expect_equal(systematic, simple)

})


test_that("systematic functions correctly with vector and data frame input", {

  dataframe <- summarize_systematic(trainingData, attribute = 'bapa', popSize = 50,
                                    desiredConfidence = 0.9)
  
  vector <- summarize_systematic(trainingData$bapa, popSize = 50,
                                 desiredConfidence = 0.9)

  expect_equal(dataframe, vector)

})
