context("Forest sampling statistics calculations: 3P sample")

data <- data.frame(plotNum = c(5, 4, 3, 5, 4),
                   VBARS = c(9, 8, 7, 8, 2),
                   treeHeight = c(1, 2, 3, 4, 5))

test_that("3P produces output", {
  
  summarize_threeP(data, sampSize = 5, treeCount = 20, BAF = 10, cvPercent = 50, trueNetVBAR = 100, height = TRUE, desiredConfidence = 0.95)

})
