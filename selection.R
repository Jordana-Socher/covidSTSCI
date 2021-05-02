# Model Selection Method - Classification
# Goal is to pass model.R the optimal classification model
# to predict which "n-tile" a country is in, with respect to GDP.

# The model will be specified in two stages:
# 1. Predict corruption based on the COVID data.
# 2. Use predicted corruption to predict GDP effect due to COVID


source("dataBuild.R")
load("covidCorruption.RData")
years <- unique(c(masterData$date))
num <- c(replicate(length(years), 0))

for (i in 1:length(years)){
  yearData = masterData[masterData$date == years[i]]
  
  regfit_full = regsubsets(deltaGDP~total_cases + total_deaths + corruptionRank + govRank + stabilityRank + regulationRank + lawRank + accountRank, data = yearData)
  
  #summary(regfit_full)
  
  res.sum <- summary(regfit_full)
  
  num[i] <- which.max(res.sum$adjr2)
}
mean(num)
summary(regfit_full)



