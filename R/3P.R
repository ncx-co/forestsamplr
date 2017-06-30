
data <- c(estimated = 5,
          measured = 5.5,
          calculated = 6)


working <- data %>%
  mutate(corrRatio = measurement / estimate) %>%
  mutate(avgRatio = sum(corrRatio) / n) %>% # number of trees(units) that have a measured value
  mutate(totalVol = sum(estimated) * avgRatio) %>% # units must match...
  mutate(SEcombined = )