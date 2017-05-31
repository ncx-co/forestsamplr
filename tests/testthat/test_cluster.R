context("Forest sampling statistics calculations: cluster sample")

clusterF = data.frame(clusterID = c(1, 2, 3, 4, 5), clusterElements = c(4, 2, 9, 4, 10),
                     sumAttr = c(1000, 1250, 950, 900, 1005), isUsed = c(T, T, F, T, T))

clusterT = data.frame(clusterID = c(1, 2, 3, 4, 5), clusterElements = c(4, 2, 9, 4, 10),
                      sumAttr = c(1000, 1250, 950, 900, 1005), isUsed = c(T, T, T, T, T))

clusterNone = data.frame(clusterID = c(NA), clusterElements = c(NA),
                         sumAttr = c(NA), isUsed = c(NA))

clusterOne = data.frame(clusterID = c(1), clusterElements = c(4),
                        sumAttr = c(1000), isUsed = c(T))

clusterTwo = data.frame(clusterID = c(1, 2), clusterElements = c(4, 2),
                        sumAttr = c(1000, 1250), isUsed = c(T, T))

test_that("cluster sample isUsed functions correctly" {

  expect_equal(summarize_cluster(clusterF)[[6]], c(200.22, 170.60, 204.67, 160.00), tolerance = 0.1)
  expect_false(identical(summarize_cluster(clusterT), summarize_cluster(clusterF)))

})




test_that("cluster sample handles input with no, one, or two clusters correctly" {

  expect_error(summarize_cluster(clusterNone))
  expect_error(summarize_cluster(clusterOne))
  expect_false(any(is.na(summarize_cluster(clusterTwo))))

})
