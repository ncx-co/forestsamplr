context("Forest sampling statistics calculations: all cluster samples")

dataPlot <- data.frame(clusterID = c(1, 1, 1, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5),
                       attr = c(1000, 1250, 950, 900, 1005, 1000, 1250, 950, 900, 1005, 1000,
                                1250, 950, 900, 1005, 1000, 1250, 950, 900),
                       isUsed = c(T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F, F))


test_that("all cluster function handles simple cluster sample", {
  
  expect_equal(summarize_all_cluster(dataPlot, element = TRUE, bernoulli = F), 
               summarize_cluster(dataPlot, element = TRUE), tolerance = 0.1)
  
})


test_that("all cluster function handles cluster sample for discrete attribute, bernoulli", {
  
  data <- data.frame(plots = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                     propAlive = c(0.75, 0.80, 0.80, 0.85, 0.70,
                                   0.90, 0.70, 0.75, 0.80, 0.65))
  
  expect_equal(summarize_all_cluster(data, attribute = 'propAlive', plotTot = 250, bernoulli = T), 
               summarize_cluster_discrete(data, attribute = 'propAlive', plotTot = 250), tolerance = 0.01)
  
})
