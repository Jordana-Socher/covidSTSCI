m(list = ls())
source("dataBuild.R")
load("covidCorruption.RData")

# Normalize data and split into training test
attach(masterData)
masterData = masterData[order(masterData$gdp_per_capita),]
masterData =  drop_na(c("gdp_per_capita"))

med = median(masterData$gdp_per_capita)
medGDP <- ifelse(masterData$gdp_per_capita >= med, yes = 1, no = 0)
masterData$medGDP = medGDP

training =sample(1:nrow(masterData),nrow(masterData)/2, replace = FALSE)
train = masterData[training,]
test = masterData[-training,]
testing = test$medGDP 

# How do these regressors predict how corrupt a country is? How does this compare to model excluding
# covid numbers etc. Do we need some model selection technique? Grid search?

#lda = lda(gdp_per_capita ~ total_cases + total_deaths + population + stringency_index + human_development_index, data=masterData, subset = training)
#lda = lda(gdp_per_capita ~  total_cases + total_deaths + population + stringency_index + human_development_index, data=masterData, subset = training)
#lda_pred = predict(lda, test)
#error = mean(lda_pred$class != testing)
#error


#kNN
train2 = cbind(population, stringency_index, human_development_index)[training, ]
train2[is.na(train2)] = 0
test2 = cbind(population, stringency_index, human_development_index)[-training, ]
test2[is.na(test2)] = 0
class = transpose(as.data.frame(subset(train, select = c(medGDP))))

accum = c()
for (numC in c(1, 2, 3, 5, 10, 20)){
  knn_pred = knn(train2, test2, class, k = numC)
  error = mean(knn_pred != testing)
  accum = append(accum, error)
  print(error)
}

plot(accum)

# Normalize data and split into training test
masterData = masterData[order(masterData$gdp_per_capita),]
masterData[is.na(masterData)] = 0
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

# How do these regressors predict how corrupt a country is? How does this compare to model excluding
# covid numbers etc. Do we need some model selection technique? Grid search?
r = lm(gdp_per_capita ~ total_deaths + population + stringency_index + human_development_index + corruptionRank + govRank + stabilityRank + regulationRank + lawRank + accountRank, data=masterData, subset=training)
summary(r)
r = lm(total_deaths~ stringency_index + human_development_index + corruptionRank + govRank + stabilityRank + accountRank, data=masterData, subset=training)


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


