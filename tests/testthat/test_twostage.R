context("Forest sampling statistics calculations: two stage sample")

trainingData = data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
                          bapa = c(1000, 1250, NA, 900, 1005, 1000, 1250, 950, 900, 1005, NA, 1250, 950, 900, 1005,
                                   1000, 1250, NA, 900),
                          isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))
attribute = 'bapa'

redData = data.frame(clusterID = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6),
                     volume = c(500, 650, 610, 490, 475, 505, 940, 825, 915, 210, 185,
                                170, 450, 300, 500, 960, 975, 890),
                     isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T))


test_that("two stage functions correctly with warning", {

  expect_equal(summarize_two_stage(trainingData, TRUE, 'bapa')$mean, 1179.64, tolerance = 0.1)

})

test_that("two stage attribute name functions correctly", {

  trainingDataAttr <- rename(trainingData, attr = bapa)

  expect_equal(summarize_two_stage(trainingDataAttr, T, 'attr'), summarize_two_stage(trainingData, T, 'bapa'))
  expect_equal(summarize_two_stage(trainingDataAttr, T, 'attr'), summarize_two_stage(trainingDataAttr, T))

})

test_that("two stage cluster input data functions correctly", {

  trainingDataCluster <- data.frame(clusterID = c(1, 2, 3, 4, 5),
                                    totClusterElements = c(5, 2, 1, 6, 5),
                                    sampledElements = c(2, 2, 2, 2, 2),
                                    isUsed = c(T, T, T, T, F),
                                    attrSumCluster = c(1000, 1250, 950, 900, 1005))

  expect_equal(summarize_two_stage(trainingDataCluster, F)$lowerLimitCI, 115.5, tolerance = 0.1)

})

test_that("two stage calculates values correctly", {

  summarize_two_stage(redData, T, 'volume', populationClusters = 16,
                      populationElementsPerCluster = 160)


})
