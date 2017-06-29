
Taken from Gregoire and Valentine, pg 68


data = c(y = c(1, 2, 3, 4), #y variable, value of interest
         x = c(4, 5, 6, 7), #x variable, secondary value of interest
         inclProb = c(2, 2, 3, 4) #probability of inclusion - ??? see comment in last line
         )

summary <- data %>%
  mutate(mean = 1 / (PIValue * nPop) * sum(attr)) %>% # for all sample  # muYPiRat
  mutate(sumTempYX = (y - mean(y) / mean(x) * x) ^ 2) %>%
  mutate(sumTempInclProb = (1 - inclProb) / inclProb) %>%
  varOfMean = (1 / (nPop ^ 2)) * sum(sumTempYX * sumTempInclProb)


#I guess I don't quite understand "PIk" variable. Difference between p(s) and Pi(subK)?