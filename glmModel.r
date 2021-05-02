rm(list = ls())
source("dataBuild.R")
load("covidCorruption.RData")

x <- unique(c(masterData$date))
aics <- c(replicate(length(x), 0))

masterData = masterData[!is.na(masterData$total_deaths), ]
masterData = masterData[!is.na(masterData$stabilityRank), ]
masterData = masterData[!is.na(masterData$lawRank), ]
masterData = masterData[!is.na(masterData$accountRank), ]
masterData = masterData[masterData$total_deaths != 0.0, ]
masterData = masterData[masterData$stabilityRank != 0.0, ]
masterData = masterData[masterData$lawRank != 0.0, ]
masterData = masterData[masterData$accountRank != 0.0, ]

count = 0
for (index in 1:length(x)){
  covidData <-  masterData[masterData$date == x[index]]
  print(" ")
  #Linear Regression
  if (length(covidData$total_deaths) > 10){
    r = glm(deltaGDP ~ total_deaths + stabilityRank  + lawRank + accountRank, data=covidData)
    aics[index] <- summary(r)$aic
  }else{
    print(x[index])
    count = count + 1
  }
  
}

aics[aics == -Inf] = Inf
aics[aics == 0.0] = Inf

for (ind in 1:length(aics)){
  if (aics[ind] == min(aics)){
    print(x[ind])
  }
}

covidData <-masterData[masterData$date == "2020-03-08"]
#Linear Regression
r = glm(deltaGDP ~ total_deaths + stabilityRank  + lawRank + accountRank, data=covidData)
summary(r)

plot(covidData$lawRank, r$fitted.values, xlab="Rule of Law Rank", ylab ="GDP change prediction")
par(new=TRUE)
plot(covidData$lawRank, covidData$deltaGDP, col="blue", xlab="Rule of Law Rank", ylab ="GDP change prediction")

plot(covidData$total_deaths, r$fitted.values, xlab="Total Deaths", ylab ="GDP change prediction")
par(new=TRUE)
plot(covidData$total_deaths, covidData$deltaGDP, col="blue", xlab="Total Deaths", ylab ="GDP change prediction")

plot(covidData$stabilityRank, r$fitted.values, xlab="Political Stability Rank", ylab ="GDP change prediction")
par(new=TRUE)
plot(covidData$stabilityRank, covidData$deltaGDP, col="blue", xlab="Political Stability Rank", ylab ="GDP change prediction")

plot(covidData$accountRank, r$fitted.values, xlab="Accountability Rank", ylab ="GDP change prediction")
par(new=TRUE)
plot(covidData$accountRank, covidData$deltaGDP, col="blue", xlab="Accountability Rank", ylab ="GDP change prediction")





