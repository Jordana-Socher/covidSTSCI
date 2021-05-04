rm(list = ls())
source("dataBuild.R")
load("covidCorruption.RData")

x <- unique(c(masterData$date))
r2s <- c(replicate(length(x), 0))

masterData = masterData[!is.na(masterData$total_deaths), ]
masterData = masterData[!is.na(masterData$stabilityRank), ]
masterData = masterData[!is.na(masterData$lawRank), ]
masterData = masterData[!is.na(masterData$accountRank), ]
masterData = masterData[masterData$total_deaths != 0.0, ]
masterData = masterData[masterData$stabilityRank != 0.0, ]
masterData = masterData[masterData$lawRank != 0.0, ]
masterData = masterData[masterData$accountRank != 0.0, ]

count = 0
rse <- c(replicate(length(x), 0))
for (index in 1:length(x)){
  covidData <-  masterData[masterData$date == x[index]]
  print(" ")
  #Linear Regression
  if (length(covidData$total_deaths) > 10){
    r = lm(deltaGDP ~ total_deaths + stabilityRank  + lawRank + accountRank, data=covidData)
    rse[index]<-summary(r)$sigma
    r2s[index] <- summary(r)$r.squared
  }else{
    print(x[index])
    count = count + 1
  }
 
}
rse=na.omit(rse)
sum_rse_lm=sum(rse)
mean_rse_lm=mean(rse)

r2s[r2s == 1.0] = 0.0
for (ind in 1:length(r2s)){
  if (r2s[ind] == max(r2s)){
    print(x[ind])
  }
}

covidData <-masterData[masterData$date == "2020-03-10"]
#Linear Regression
r = lm(deltaGDP ~ total_deaths + stabilityRank  + lawRank + accountRank, data=covidData)
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




