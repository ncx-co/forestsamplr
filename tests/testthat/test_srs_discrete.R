context("Forest sampling statistics calculations: simple random sample for attributes, discrete variables")

data <- data.frame(
  alive = c(T, T, F, T, F, F),
  plots = c(1, 2, 3, 4, 5, 6)
)

attribute <- "alive"


test_that("srs discrete calculates values correctly", {
  expect_equal(summarize_simple_random_discrete(data, attribute, popTot = 50)$upperLimitCI,
    1.4858,
    tolerance = 0.001
  )
})


test_that("srs discrete requires a population total value", {
  expect_error(summarize_simple_random_discrete(data, attribute))
})


test_that("srs discrete generalizes attribute", {
  dataGeneral <- rename(data, attr = alive)

  expect_equal(
    summarize_simple_random_discrete(dataGeneral, "attr", popTot = 50),
    summarize_simple_random_discrete(data, "alive", popTot = 50)
  )
  expect_equal(
    summarize_simple_random_discrete(dataGeneral, "attr", popTot = 50),
    summarize_simple_random_discrete(dataGeneral, popTot = 50)
  )
})
