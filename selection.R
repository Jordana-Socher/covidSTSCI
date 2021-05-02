# Model Selection Method - Classification
# Goal is to pass model.R the optimal classification model
# to predict which "n-tile" a country is in, with respect to GDP.

# The model will be specified in two stages:
# 1. Predict corruption based on the COVID data.
# 2. Use predicted corruption to predict GDP effect due to COVID


source("dataBuild.R")
load("covidCorruption.RData")
masterData = masterData[masterData$date == "2020-03-19"]
regfit_full = regsubsets(deltaGDP~total_cases + total_deaths + corruptionRank + govRank + stabilityRank + regulationRank + lawRank + accountRank, data = masterData)


summary(regfit_full)
reg_summary = summary(regfit_full)

res.sum <- summary(regfit_full)

data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

regfit_full = regsubsets(deltaGDP~total_cases + total_deaths + corruptionRank + govRank + stabilityRank + regulationRank + lawRank + accountRank, data = masterData)


summary(regfit_full)
reg_summary = summary(regfit_full)

res.sum <- summary(regfit_full)

data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)
