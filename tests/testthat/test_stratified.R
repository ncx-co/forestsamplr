context("Forest sampling statistics calculations: stratified sample")

trainingData <- clusterBaData %>%
  filter(!is.na(bapa)) %>%
  filter(clusterID %in% c(1, 2)) %>%
  rename(stratum = clusterID)

stratumTab <- data.frame(stratum = c(1, 2), acres = c(200, 50))

stratSum <- data.frame(
  "stratum" = c(1, 2),
  "stratMean" = c(1038.75, 1125),
  "stratVarMean" = c(5543.229, 15625),
  "stratSE" = c(74.45, 125),
  "stratPlots" = c(4, 2),
  "stratAcres" = c(200, 50)
)
totSum <- data.frame(
  "popMean" = c(1056),
  "popSE" = c(64.60),
  "popCIhalf" = c(137.7),
  "ciPct" = c(13.04)
)
finalSum <- list(
  "stratumSummaries" = data.frame(stratSum),
  "totalSummary" = data.frame(totSum)
)




test_that("stratified functions correctly", {
  strat <- summarize_stratified(
    trainingData,
    attribute = "bapa",
    stratumTab,
    desiredConfidence = 0.9,
    post = T
  )
  expect_equal(
    strat,
    finalSum,
    tolerance = 0.1
  )
  expect_equal(
    strat$stratumSummaries,
    stratSum,
    tolerance = 0.1
  )
  expect_equal(
    strat$totalSummary,
    totSum,
    tolerance = 0.1
  )
})
