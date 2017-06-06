context("Forest sampling statistics calculations: simple random sample for attriutes: discrete variables")

data <- data.frame(alive = c(T, T, F, T, F, F),
                   plots = c(1, 2, 3, 4, 5, 6))

attribute <- 'alive'

popTot <- 50


test_that("srs discrete calculates values correctly", {

  expect_equal(summarize_simple_random_discrete(data, attribute, popTot)$upperLimitCI,
               1.0028, tolerance = 0.001)

})

test_that("srs discrete requires a population total value", {

  expect_error(summarize_simple_random_discrete(data, attribute))

})
