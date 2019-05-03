context("Forest sampling statistics calculations: simple random sample")

trainingData <- simpleRandom %>%
  group_by(plot) %>%
  summarize(bapa = sum(BAF))

attribute <- "bapa"

test_that("simple random function handles missing arguments correctly", {
  sampT <- summarize_simple_random(
    data = trainingData, attribute, popSize = 50,
    desiredConfidence = 0.9, infiniteReplacement = T
  )
  sampNA <- summarize_simple_random(
    data = trainingData, attribute, popSize = NA,
    desiredConfidence = 0.9, infiniteReplacement = T
  )
  sampNoN <- summarize_simple_random(
    data = trainingData, attribute, desiredConfidence = 0.9,
    infiniteReplacement = T
  )
  sampNoReplacement <- summarize_simple_random(trainingData, attribute,
    popSize = 50,
    desiredConfidence = 0.9
  )
  sampF <- summarize_simple_random(trainingData, attribute,
    popSize = 50,
    desiredConfidence = 0.9, infiniteReplacement = F
  )
  sampReplacementNA <- summarize_simple_random(trainingData, attribute,
    popSize = 50,
    desiredConfidence = 0.9, infiniteReplacement = NA
  )

  expect_equal(sampNA, sampT)
  expect_equal(sampNA, sampNoN)
  expect_equal(sampF, sampNoReplacement)
  expect_equal(sampF, sampReplacementNA)
  expect_error(summarize_simple_random(trainingData, attribute,
    popSize = 0,
    desiredConfidence = 0.9, infiniteReplacement = T
  ))

  expect_equal(sampF, data.frame(
    "mean" = 135, "variance" = 1172, "standardError" = 8.6,
    "upperLimitCI" = 150.5, "lowerLimitCI" = 120
  ), tolerance = 1)
})



test_that("simple random throws errors for data with few or no entries, or missing values", {
  trainingDataOne <- data.frame("bapa" = c(4), "plot" = c(10))
  trainingDataThree <- data.frame("bapa" = c(4, 5, NA), "plot" = c(10, 11, 12))
  trainingDataNone <- data.frame("bapa" = c(NA), "plot" = c(10))

  expect_warning(summarize_simple_random(trainingDataOne, attribute,
    popSize = 50,
    desiredConfidence = 0.9, infiniteReplacement = T
  ))
  expect_error(summarize_simple_random(trainingDataNone, attribute,
    popSize = 50,
    desiredConfidence = 0.9, infiniteReplacement = T
  ))
  expect_error(summarize_simple_random(trainingDataThree, attribute,
    popSize = 50,
    desiredConfidence = 0.9, infiniteReplacement = T
  ))
})



test_that("simple random processes data with at least two", {
  trainingDataTwo <- data.frame("bapa" = c(4, 5), "plot" = c(10, 11))

  sampTwo <- summarize_simple_random(trainingDataTwo, attribute,
    popSize = 50,
    desiredConfidence = 0.9, infiniteReplacement = T
  )

  expect_equal(sampTwo$mean, mean(trainingDataTwo[[1]]))
})

test_that("simple random accepts and processes vectors and dataframes equally", {
  dataframe <- summarize_simple_random(trainingData, attribute,
    popSize = 50,
    desiredConfidence = 0.9, infiniteReplacement = T
  )
  vector <- summarize_simple_random(c(
    140, 140, 180, 140, 180, 120,
    120, 80, 80, 120, 180, 140
  ),
  popSize = 50,
  desiredConfidence = 0.9, infiniteReplacement = T
  )
  expect_equal(dataframe, vector)
})
