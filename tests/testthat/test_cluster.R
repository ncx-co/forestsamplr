context("Forest sampling statistics calculations: cluster sample")

clusterClean <- clusterBaData %>%
  filter(!is.na(bapa))
# test clusters
clusterFalse <- data.frame(clusterID = c(1, 2, 3, 4, 5), clusterElements = c(4, 2, 1, 5, 4),
                     sumAttr = c(1039, 1125, 950, 1001, 1039), isUsed = c(T, T, T, T, F))

clusterTrue <- data.frame(clusterID = c(1, 2, 3, 4, 5), clusterElements = c(4, 2, 1, 5, 4),
                      sumAttr = c(1000, 1250, 950, 900, 1005), isUsed = c(T, T, T, T, T))

clusterNone <- data.frame(clusterID = c(NA), clusterElements = c(NA),
                         sumAttr = c(NA), isUsed = c(NA))

clusterOne <- data.frame(clusterID = c(1), clusterElements = c(4),
                        sumAttr = c(1000), isUsed = c(T))

clusterTwo <- data.frame(clusterID = c(1, 2), clusterElements = c(4, 2),
                        sumAttr = c(1000, 1250), isUsed = c(T, T))

# test plots
plotLevelFalse <- clusterClean

plotLevelTrue <- clusterClean %>% mutate(isUsed = TRUE)

test_that("cluster sample isUsed functions correctly", {

  expect_equal(summarize_cluster(clusterFalse, F)[[1]], c(43.8), tolerance = 0.1)
  expect_false(identical(summarize_cluster(clusterTrue, F), 
                         summarize_cluster(clusterFalse, F)))
  expect_false(identical(summarize_cluster(plotLevelTrue, T, attr = 'bapa'), 
                         summarize_cluster(plotLevelFalse, T, attr = 'bapa')))

})


test_that("cluster sample handles input with no, one, or two clusters correctly", {

  expect_error(summarize_cluster(clusterNone, F))
  expect_error(summarize_cluster(clusterOne, F))
  expect_false(any(is.na(summarize_cluster(clusterTwo, F))))

})


test_that("cluster sample handles input data with NA values", {

  expect_error(summarize_cluster(clusterBaData, T))

})


test_that("cluster sample handles attribute", {

  expect_equal(summarize_cluster(clusterClean, T, 'bapa'),
               summarize_cluster(clusterClean %>% rename(attr = bapa), T))

})

