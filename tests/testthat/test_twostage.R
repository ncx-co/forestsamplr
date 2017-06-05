context("Forest sampling statistics calculations: two stage sample")

trainingData = data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
                          attr = c(1000, 1250, NA, 900, 1005, 1000, 1250, 950, 900, 1005, NA, 1250, 950, 900, 1005,
                                   1000, 1250, NA, 900),
                          isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))
attribute = 'bapa'


test_that("two stage functions correctly with warning", {

  expect_warning(summarize_two_stage(trainingData, TRUE, 'bapa'))
  expect_equal(suppressWarnings(summarize_two_stage(trainingData, TRUE, 'bapa'))$mean, 1179.64, tolerance = 0.1)

})
