context("Forest sampling statistics calculations: cluster sample for attributes, discrete variables")

 # dataset is from the example for this sampling method in Avery and Burkhart
 data <- data.frame(plots = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                    propAlive = c(0.75, 0.80, 0.80, 0.85, 0.70,
                    0.90, 0.70, 0.75, 0.80, 0.65))

 attribute <- 'propAlive'

 plotTot <- 250


test_that("cluster discrete calculates values correctly", {
  
  expect_equal(summarize_cluster_discrete(data, attribute, plotTot)$upperLimitCI,
               0.82275, tolerance = 0.001)
  
})

test_that("cluster discrete requires a population total value", {
  
  expect_error(summarize_cluster_discrete(data, attribute))
  
})
