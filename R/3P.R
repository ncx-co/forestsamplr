
data <- c(estimated = 5,
          measured = 5.5,
          calculated = 6)


working <- data %>%
  mutate(corrRatio = measurement / estimate) %>%
  mutate(avgRatio = sum(corrRatio) / n) %>% # number of trees(units) that have a measured value
  mutate(totalVol = sum(estimated) * avgRatio) %>% # units must match...
  mutate(SEcombined = )




Jul 10, 2017

# Point - 3P (tree) method

#pre sampling
plots <- 150
treesPerPlot <- 6
treeHt <- 140 #ft
#VBAR in bd ft per sq ft
#VBAR estimated by 2 * treeHt
measurements <- 60
sumEstVBARs <- plots * treesPerPlot * (treeHt * 2)
KZ <- sumEstVBARs / measurements

# # # # # # # # # # # # # # # # HERE
# post sampling - pg 601
sampSize = 0
treeCount = 0
BAF = 0
cvPercent = 50
data <- VBARS

estPlotVol <- treeCount * BAF * avgTreeVBARSforPlot #what is a vbar?
avgVol <- avg(estPlotVol)
ratio <- trueNetVBAR / estimateVBAR
netVolume <- avgVol * ratio 
sePercent <- cvPercent / sqrt(sampSize)


  
  
  
  
  

bad:
  
  Taken from Gregoire and Valentine, pg 68

nPop = 50.0

data = data.frame(attr = c(1, 2, 3, 4), #y variable, value of interest
                  x = c(4, 5, 6, 7), #x variable, secondary value of interest
                  inclProb = c(2, 2, 3, 4) #probability of inclusion - ??? see comment in last line
)

summary <- data %>%
  mutate(mean = 1 / (PIValue * nPop) * sum(attr)) %>% # for all sample  # muYPiRat
  mutate(inclProb = mean ^ attr * (exp(1) ^ (- mean)) / factorial(attr)) %>%
  mutate(sumTempYX = (attr - mean(attr) / mean(x) * x) ^ 2) %>%
  mutate(sumTempInclProb = (1 - inclProb) / inclProb) %>%
  mutate(varOfMean = (1 / (nPop ^ 2)) * sum(sumTempYX * sumTempInclProb)) 

#mean and variance are supposed to be the same...

#I guess I don't quite understand "PIk" variable. Difference between p(s) and Pi(subK)?
____________________ ^nah



