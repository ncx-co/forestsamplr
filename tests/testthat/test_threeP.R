context("Forest sampling statistics calculations: 3P sample")

dataEstimateVBAR <- data.frame(
  plotNum = c(1, 2, 3, 4, 5),
  treeCount = c(4, 5, 6, 3, 4),
  BAF = c(10, 10, 10, 10, 10),
  avgTreeVBARSperPlot = c(9, 8, 7, 8, 2),
  estimateVBAR = c(1, 2, 3, 4, 5)
)

test_that("3P produces output", {
  expect_equal(
    summarize_threeP(
      dataEstimateVBAR,
      cvPercent = 50,
      trueNetVBAR = 10,
      height = F,
      plot = T
    )[1],
    data.frame("sampSize" = 5)
  )
})
