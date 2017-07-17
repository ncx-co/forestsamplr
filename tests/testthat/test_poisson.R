context("Forest sampling statistics calculations: poisson sample")

data <- c(2, 3, 4, 3, 4, 5, 2, 7)
desiredConfidence <- 0.95

test_that("Poisson produces output", {
  
  expect_equal(suppressWarnings(summarize_poisson(data)), data.frame('sampleSize' = 5, 'mean' = 6,
                                                                     'lambdaHat' = 6, 
                                                                     'se' = 1.09,
                                                                     'lowerBoundCI' = 3.85,
                                                                     'upperBoundCI' = 8.14),
               tolerance = 0.1)
  
})

test_that("Poisson warns against overdispersion", {
  
  expect_warning(summarize_poisson(data))
  
})
