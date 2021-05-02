rm(list = ls())
source("dataBuild.R")
load("covidCorruption.RData")

# Normalize data and split into training test for continuous regression
training =sample(1:nrow(masterData),nrow(masterData)/2, replace = FALSE)
train = masterData[training,]
train = na.omit(train)
test = masterData[-training,]
test = na.omit(test)
testing = test$gdp_per_capita

regfit_full = regsubsets(total_deaths~total_cases + population + stringency_index + human_development_index + corruptionRank + govRank + stabilityRank + regulationRank + lawRank + accountRank, data = masterData)
summary(regfit_full)
res.sum <- summary(regfit_full)

data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

#Linear Regression
r = lm(gdp_per_capita ~ total_deaths + population + stringency_index + human_development_index + corruptionRank + govRank + stabilityRank + regulationRank + lawRank + accountRank, data=masterData, subset=training)
summary(r)
r = lm(total_deaths~ stringency_index + human_development_index + corruptionRank + govRank + stabilityRank + accountRank, data=masterData, subset=training)

#Logistic Regression
r = glm(gdp_per_capita ~ total_cases + total_deaths + population + stringency_index + human_development_index + corruptionRank + govRank + stabilityRank + regulationRank + lawRank + accountRank, data=masterData, subset=training)
summary(r)
r = glm(total_cases ~ stringency_index + human_development_index + corruptionRank + govRank + stabilityRank + accountRank, data=masterData, subset=training)

lm_pred = predict(r, test)
res = testing - lm_pred
mean(res)


lda = lda(gdphalf ~ total_cases + total_deaths + population + stringency_index + human_development_index + corruptionRank, data=masterData, subset = training)
lda = lda(gdphalf ~  total_cases + total_deaths + population + stringency_index + human_development_index + corruptionRank, data=masterData, subset = training)
lda_pred = predict(lda, test)
error = mean(lda_pred$class != testing)
error

masterData = masterData[masterData$total_cases < 5000000, ]
masterData = masterData[masterData$deltaGDP > -10000, ]
loessMod10 <- loess(deltaGDP ~ total_cases , data=masterData, span=0.10) # 10% smoothing span
smoothed10 <- predict(loessMod10)
loessMod25 <- loess(deltaGDP ~ total_cases, span=0.25) # 25% smoothing span
smoothed25 <- predict(loessMod25)
loessMod50 <- loess(deltaGDP ~ total_cases, data=masterData, span=0.50) # 50% smoothing span
smoothed50 <- predict(loessMod50)
loessMod75 <- loess(deltaGDP ~ total_cases, data=masterData, span=0.75) # 50% smoothing span
smoothed75 <- predict(loessMod75)

cleaned = masterData[masterData$deltaGDP != 0.0, ]
plot(smoothed10, x=cleaned$total_cases, main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)", col = "red")
plot(smoothed25, x=cleaned$total_cases, main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)", col = "red")
plot(smoothed50, x=cleaned$total_cases, main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)", col = "red")
plot(cleaned$deltaGDP, x=cleaned$total_cases,)
par(new=TRUE)
plot(smoothed50, x=cleaned$total_cases, main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)", col = "yellow")

lm_pred = predict(r, test)
res = testing - lm_pred
mean(res)

plot(masterData$total_death, masterData$deltaGDP)