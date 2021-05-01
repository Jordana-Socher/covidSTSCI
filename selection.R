# Model Selection Method - Classification
# Goal is to pass model.R the optimal classification model
# to predict which "n-tile" a country is in, with respect to GDP.

# The model will be specified in two stages:
# 1. Predict corruption based on the COVID data.
# 2. Use predicted corruption to predict GDP effect due to COVID


source("dataBuild.R")
load("covidCorruption.RData")
regfit_full = regsubsets(gdp50~total_cases + total_deaths + population + stringency_index + human_development_index , data = masterData)
regfit_full = regsubsets(gdp50~total_cases + total_deaths + population + stringency_index + human_development_index + corruptionRank + govRank + stabilityRank + regulationRank + lawRank + accountRank, data = masterData)


summary(regfit_full)
reg_summary = summary(regfit_full)
names(reg_summary)

res.sum <- summary(regfit_full)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- train(model.formula, data = data, method = "lda",
              trControl = train.control)
  cv$results$RMSE
}
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

model.ids <- 1:5
cv.errors <-  map(model.ids, get_model_formula, regfit_full, "gdp50") %>%
  map(get_cv_error, data = swiss) %>%
  unlist()
cv.errors

