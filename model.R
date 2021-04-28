source("dataBuild.R")
load("covidCorruption.RData")

# Normalize data and split into training test
masterData = masterData[order(masterData$gdp_per_capita),]
masterData[is.na(masterData)] = 0
training =sample(1:nrow(masterData),nrow(masterData)/2, replace = FALSE)
train = masterData[training,]
train = na.omit(train)
test = masterData[-training,]
test = na.omit(test)
testing = test$corruptionRank50

# How do these regressors predict how corrupt a country is? How does this compare to model excluding
# covid numbers etc. Do we need some model selection technique? Grid search?
lda = lda(corruptionRank50 ~ gdp_per_capita + total_cases + total_deaths + population + stringency_index + human_development_index, data=masterData, subset = training)
lda_pred = predict(lda, test)
error = mean(lda_pred$class != testing)
error
