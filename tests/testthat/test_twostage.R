context("Forest sampling statistics calculations: two stage sample")

trainingData = data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
                          bapa = c(1000, 1250, NA, 900, 1005, 1000, 1250, 950, 900, 1005, NA, 1250, 950, 900, 1005,
                                   1000, 1250, NA, 900),
                          isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))

# data from Avery and Burkhart
redData = data.frame(clusterID = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6),
                     volume = c(500, 650, 610, 490, 475, 505, 940, 825, 915, 210, 185,
                                170, 450, 300, 500, 960, 975, 890),
                     isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T))


test_that("two stage functions correctly with warning", {

  expect_equal(suppressWarnings(summarize_two_stage(trainingData, TRUE, 'bapa', desiredConfidence = 0.95))$mean, 
               1179.64, tolerance = 0.1)

})


test_that("two stage attribute name functions correctly", {

  redDataAttr <- rename(redData, attr = volume)

  expect_equal(summarize_two_stage(redDataAttr, T, attribute = 'attr',
                                                    populationClusters = 16,
                                                    populationElementsPerCluster = 160), 
               summarize_two_stage(redData, T, attribute = 'volume',
                                   populationClusters = 16,
                                   populationElementsPerCluster = 160))
  expect_equal(summarize_two_stage(redDataAttr, T, attribute = 'attr', 
                                   populationClusters = 16,
                                   populationElementsPerCluster = 160), 
               summarize_two_stage(redDataAttr, T, 
                                   populationClusters = 16,
                                   populationElementsPerCluster = 160))

})


test_that("two stage cluster input data functions correctly", {

  trainingDataCluster <- data.frame(clusterID = c(1, 2, 3, 4, 5),
                                    totClusterElements = c(5, 2, 1, 6, 5),
                                    sampledElements = c(2, 2, 2, 2, 2),
                                    isUsed = c(T, T, T, T, F),
                                    attrSumCluster = c(1000, 1250, 950, 900, 1005))

  expect_equal(summarize_two_stage(trainingDataCluster, F)$lowerLimitCI, 
               70.5, tolerance = 0.1)

})


test_that("two stage calculates values correctly", {

  # checks the function against the values produced in Avery and Burkhart's (1967) 
    # Forest Measurements, Fifth Edition
  # CI limits are different than the answers expressed in the textbook, because 
    # the true t-score was calculated in this script
  expect_equal(summarize_two_stage(redData, T, 'volume', populationClusters = 16,
                                   populationElementsPerCluster = 160), 
               data.frame(mean = 586.1, varianceB = 250188.9, varianceW = 3869.4,
                          standardError = 93.628, upperLimitCI = 782.81, 
                          lowerLimitCI = 389.40), tolerance = 0.01)

})

