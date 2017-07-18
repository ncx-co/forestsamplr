context("Forest sampling statistics calculations: poisson sample")

data <- c(2, 3, 4, 3, 4, 5, 2, 7)

test_that("Poisson produces output", {
  
  expect_equal(suppressWarnings(summarize_poisson(data)), data.frame('sampleSize' = 8, 'mean' = 3.75,
                                                                     'lambdaHat' = 3.75, 
                                                                     'se' = 0.68,
                                                                     'lowerBoundCI' = 2.40,
                                                                     'upperBoundCI' = 5.09),
               tolerance = 0.1)
  
})

test_that("Poisson warns against overdispersion", {
  
  expect_warning(summarize_poisson(data))
  
})
