source("dataBuild.R")
load("covidCorruption.RData")

summary(masterData$lawRank)
old.par <- par(mfrow=c(2, 5))
hist(masterData$lawRank, main="Law Rank", xlab="Law Rank")

summary(masterData$accountRank)
hist(masterData$accountRank, main="Account Rank", xlab="Account Rank")

summary(masterData$total_deaths)
hist(masterData$total_deaths, main="Total Deaths", xlab="Total Deaths")

summary(masterData$regulationRank)
hist(masterData$regulationRank, main="Regulation Rank", xlab="Regulation Rank")

summary(masterData$govRank)
hist(masterData$govRank, main="Gov Rank", xlab="Gov Rank")

summary(masterData$corruptionRank)
hist(masterData$corruptionRank, main="Corruption Rank", xlab="Corruption Rank")

summary(masterData$total_cases)
hist(masterData$total_cases, main="Total Cases", xlab="Total Cases")

summary(masterData$gdp_per_capita)
hist(masterData$gdp_per_capita, , main="GDP Per Capita", xlab="GDP Per Capita")

summary(masterData$gdp2019)
hist(masterData$gdp2019, main="GDP 2019", xlab="GDP 2019")

summary(masterData$gdp2020)
hist(masterData$gdp2020, main="GDP 2020", xlab="GDP 2020")
par(old.par)