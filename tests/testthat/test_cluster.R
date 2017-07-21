context("Forest sampling statistics calculations: cluster sample")


# test clusters
clusterFalse = data.frame(clusterID = c(1, 2, 3, 4, 5), clusterElements = c(4, 2, 9, 4, 10),
                     sumAttr = c(1000, 1250, 950, 900, 1005), isUsed = c(T, T, F, T, T))

clusterTrue = data.frame(clusterID = c(1, 2, 3, 4, 5), clusterElements = c(4, 2, 9, 4, 10),
                      sumAttr = c(1000, 1250, 950, 900, 1005), isUsed = c(T, T, T, T, T))

clusterNone = data.frame(clusterID = c(NA), clusterElements = c(NA),
                         sumAttr = c(NA), isUsed = c(NA))

clusterOne = data.frame(clusterID = c(1), clusterElements = c(4),
                        sumAttr = c(1000), isUsed = c(T))

clusterTwo = data.frame(clusterID = c(1, 2), clusterElements = c(4, 2),
                        sumAttr = c(1000, 1250), isUsed = c(T, T))

# test plots
plotLevelFalse <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
                        attr = c(1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005,
                                 1000, 1250, 950, 900),
                        isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))

plotLevelTrue <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
                             attr = c(1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005,
                                      1000, 1250, 950, 900),
                             isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T))

plotLevelNAOne <- data.frame(clusterID = c(NA), attr = c(NA), isUsed = c(NA))

plotLevelNAMore <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
                            attr = c(1000, 1250, 950, 900, 1005, 1000, 1250, NA, 900, 1005, 1000, 1250, 950, 900, 1005,
                                     1000, 1250, 950, 900),
                            isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T))

plotLevelAttribute <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
                              kittens = c(1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005,
                                       1000, 1250, 950, 900),
                              isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T))


test_that("cluster sample isUsed functions correctly", {

  expect_equal(summarize_cluster(clusterFalse, F)[[1]], c(30.51), tolerance = 0.1)
  expect_false(identical(summarize_cluster(clusterTrue, F), 
                         summarize_cluster(clusterFalse, F)))
  expect_false(identical(summarize_cluster(plotLevelTrue, T), 
                         summarize_cluster(plotLevelFalse, T)))

})


test_that("cluster sample handles input with no, one, or two clusters correctly", {

  expect_error(summarize_cluster(clusterNone, F))
  expect_error(summarize_cluster(clusterOne, F))
  expect_false(any(is.na(summarize_cluster(clusterTwo, F))))

})


test_that("cluster sample handles input data with NA values", {

  expect_error(summarize_cluster(plotLevelNAOne, T))
  expect_error(summarize_cluster(plotLevelNAMore, T))

})


test_that("cluster sample handles attribute", {

  expect_equal(summarize_cluster(plotLevelAttribute, T, 'kittens'),
               summarize_cluster(plotLevelTrue, T))

})

