context("Forest sampling statistics calculations: two stage sample")

trainingData = data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
                          bapa = c(1000, 1250, NA, 900, 1005, 1000, 1250, 950, 900, 1005, NA, 1250, 950, 900, 1005,
                                   1000, 1250, NA, 900),
                          isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))
attribute = 'bapa'


test_that("two stage functions correctly with warning", {

  expect_equal(summarize_two_stage(trainingData, TRUE, 'bapa')$mean, 1179.64, tolerance = 0.1)

})

test_that("two stage attribute name functions correctly", {

  trainingDataAttr <- rename(trainingData, attr = bapa)

  expect_equal(summarize_two_stage(trainingDataAttr, T, 'attr'), summarize_two_stage(trainingData, T, 'bapa'))
  expect_equal(summarize_two_stage(trainingDataAttr, T, 'attr'), summarize_two_stage(trainingDataAttr, T))

})
