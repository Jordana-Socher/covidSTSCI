# Model Selection Method - Classification
# Goal is to pass model.R the optimal classification model
# to predict which "n-tile" a country is in, with respect to GDP.

# The model will be specified in two stages:
# 1. Predict corruption based on the COVID data.
# 2. Use predicted corruption to predict GDP effect due to COVID


source("dataBuild.R")
load("covidCorruption.RData")
