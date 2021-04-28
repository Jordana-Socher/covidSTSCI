# STSCI Final Project 4740
library(readxl)
library(ggplot2)
library(data.table)
library(reshape)
library(tidyr)
library(gtools)
library(MASS)

#path = getwd()
#setwd(paste(path, "/Desktop/Classes/STSCI4740/final", sep = ""))
setwd("PATH")

rm(list = ls())
covidData = read_excel('COVID.xlsx')
corruptionData = read_excel('corruption.xlsx')

corruptionData = dplyr::select(corruptionData, -contains("Country Name"))
corruptionData = dplyr::select(corruptionData, -contains("Series Code"))
corruptionData = na.omit(corruptionData)
corruptionData = spread(corruptionData, "Series Name", "2019 [YR2019]")
corruptionData = corruptionData[ , grepl( "Percentile Rank" , names( corruptionData )) | grepl( "Country Code" , names( corruptionData )) ]
corruptionData = corruptionData[ , !grepl( "90%" , names( corruptionData ) ) ]

# Creates daily death count var (deathRate)
covidData = covidData[order(covidData$location, covidData$date),]
setDT(covidData)
covidData[, lag.value:=c(NA, total_deaths[-.N]), by=location]
covidData$deathRate = covidData$total_deaths- covidData$lag.value
covidData = subset(covidData, select=-c(lag.value))
covidData = covidData[covidData$date == "2020-10-19"]

# Creates daily case count var (caseRate)
covidData[, lag.value:=c(NA, total_cases[-.N]), by=location]
covidData$caseRate = covidData$total_cases- covidData$lag.value
covidData = subset(covidData, select=-c(lag.value))

# Creates cumulative death count as number of every 100,000 people in population (deathRatePerc)
covidData$deathRatePer100k= covidData$total_deaths/(covidData$population/100000)

# Creates cumulative case count as number of every 100,000 people in population (caseRatePerc)
covidData$caseRatePer100k = covidData$total_cases/(covidData$population/100000)

# Merges 2019 country corruption data onto covid data for regression analysis. Renames columns to shorten
masterData = merge(covidData, corruptionData, by.x = "iso_code", by.y = "Country Code", sort = TRUE)
rm(covidData, corruptionData)
names = names(masterData)
names[19] = 'corruptionRank'
names[20] = 'govRank'
names[21] = 'stabilityRank'
names[22] = 'regulationRank'
names[23] = 'lawRank'
names[24] = 'accountRank'
colnames(masterData) = names

# Split into "n-tiles"
masterData$corruptionRank50 =  quantcut( masterData$gdp_per_capita ,2, levels.mean=T)
masterData$corruptionRank33 =  quantcut( masterData$gdp_per_capita ,3, levels.mean=T)
masterData$corruptionRank25 =  quantcut( masterData$gdp_per_capita ,4, levels.mean=T)
masterData$corruptionRank10 =  quantcut( masterData$gdp_per_capita ,10, levels.mean=T)

dfghjk

# Summary plots - not too substantive
plot1 = ggplot(data =masterData, aes(x = caseRatePer100k, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot2 = ggplot(data =masterData, aes(x = corruptionRank, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot3 = ggplot(data =masterData, aes(x = govRank, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot4 = ggplot(data =masterData, aes(x = stabilityRank, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot5 = ggplot(data =masterData, aes(x = gdp_per_capita, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot6 = ggplot(data =masterData, aes(x = gdp_per_capita, y = corruptionRank)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot7 = ggplot(data =masterData, aes(x = gdp_per_capita, y = corruptionRank)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")

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
