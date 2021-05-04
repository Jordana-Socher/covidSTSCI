rm(list = ls())
source("dataBuild.R")
load("covidCorruption.RData")

# Model Loop. Iterates over days and runs loess local polynomial regression 
# with varying bandwith selections. The model regressors are from selection.R,
# using a general linear approach. Unit of analysis is country on day d.

masterData = masterData[masterData$total_cases < 5000000, ]
masterData = masterData[masterData$deltaGDP > -10000, ]
masterData = masterData[!is.na(masterData$lawRank), ]

# Begin model loop. Iterates over unique days and appends model statistics 
# to globalData. Sum of squared error is most valuable here, allowing us to 
# track the model's ability to predict the change in GDP from 2019 to 2020 over 
# time. 
x <- unique(c(masterData$date))
rse_lm25 <- c(replicate(length(x), 0))
rse_lm50 <- c(replicate(length(x), 0))
rse_lm75 <- c(replicate(length(x), 0))
globalData = data.frame()
for (i in 0:length(x)) {
  
  localData = masterData[masterData$date == x[i]]
  
  if (length(localData$Country.or.Area) !=0) {
    loessMod25 <- loess(deltaGDP ~lawRank, data=localData, span=0.25) # 25% smoothing span
    smoothed25 <- predict(loessMod25)
    rse_lm25[i]<-loessMod25$s

    loessMod50 <- loess(deltaGDP ~ lawRank, data=localData, span=0.50) # 50% smoothing span
    smoothed50 <- predict(loessMod50)
    rse_lm50[i]<-loessMod50$s
    
    loessMod75 <- loess(deltaGDP ~ lawRank, data=localData, span=0.75) # 50% smoothing span
    smoothed75 <- predict(loessMod75)
    rse_lm75[i]<-loessMod75$s
    
    localData = c(x[i], loessMod25$s, loessMod50$s, loessMod75$s)
    globalData <- rbind(globalData, localData)
  }
}

rse_lm25=na.omit(rse_lm25)
rse_lm25=rse_lm25[!is.infinite(rse_lm25)]
rse_lm25=rse_lm25[1:266]
mean(rse_lm25)

rse_lm50=na.omit(rse_lm50)
rse_lm50=rse_lm50[!is.infinite(rse_lm50)]
rse_lm50=rse_lm50[1:266]
mean(rse_lm50)

rse_lm75=na.omit(rse_lm75)
rse_lm75=rse_lm75[!is.infinite(rse_lm75)]
rse_lm75=rse_lm75[1:294]
mean(rse_lm75)

# Clean the returned data set
names(globalData)[1] = "date"
names(globalData)[2] = "mod25"
names(globalData)[3] = "mod50"
names(globalData)[4] = "mod75"
globalData$mod25 <- as.numeric(as.character(globalData$mod25))
globalData$mod50 <- as.numeric(as.character(globalData$mod50))
globalData$mod75 <- as.numeric(as.character(globalData$mod75))
globalData = globalData[!is.na(globalData$mod25), ]

# Plot the Results over time and bandwidth selections
dev.off()
plot(as.Date(globalData$date), globalData$mod25, col = "red", pch="+", 
     xlab="", ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(as.Date(globalData$date), globalData$mod50, col = "blue", pch="+",
     xlab="", ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(as.Date(globalData$date), globalData$mod75, col = "green", pch="+", 
     main="Loess Prediction Error Over Time", xlab="Date", ylab="Root MSE",
     )

#Select Best Day and Plot
bestDate = globalData$date[which(globalData$mod50 == min(globalData$mod50))]
bestDate = masterData[masterData$date == bestDate]
loessMod50 <- loess(deltaGDP ~ lawRank, data=bestDate, span=0.50) # 50% smoothing span
smoothed50 <- predict(loessMod50)
plot(bestDate$lawRank, bestDate$deltaGDP, col = "red", pch="+", xlab="", 
     ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(bestDate$lawRank, smoothed50, col = "blue", pch="+", xlab="Law Rank",
     ylab="Change in GDP", main="Best Day Model Prediction")

#Select Worst Day and Plot
worstDate = globalData$date[which(globalData$mod50 == max(globalData$mod50))]
worstDate = masterData[masterData$date == worstDate]
loessMod50 <- loess(deltaGDP ~ lawRank, data=worstDate, span=0.50) # 50% smoothing span
smoothed50 <- predict(loessMod50)
plot(worstDate$lawRank, worstDate$deltaGDP, col = "red", pch="+", xlab="", 
     ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(worstDate$lawRank, smoothed50, col = "blue", pch="+", xlab="Law Rank",
     ylab="Change in GDP", main="Worst Day Model Prediction")

rm(list = ls())