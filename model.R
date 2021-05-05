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
# to globalData. r2 is most valuable here, allowing us to 
# track the model's ability to predict the percent change in GDP from 2019 to 2020 over 
# time. 

x <- unique(c(masterData$date))
globalData = data.frame()
for (i in 0:length(x)) {
  
  localData = masterData[masterData$date == x[i],]
  localData[is.na(localData)] = 0
  
  print(x[i])
  if (length(localData$Country.or.Area) !=0) {
    
    if (mean(localData$total_deaths) != 0) {
      # Lpoly regression with subset
      np=npreg(deltaGDP ~ total_deaths + stabilityRank  + lawRank + accountRank, na.rm = TRUE, ckertype = "uniform", data=localData)
      
      # Subset Linear Model 
      lm = lm(deltaGDP ~ total_deaths + stabilityRank  + lawRank + accountRank, data=localData)
      
      # Kitchen Sink Model
      basic = lm(deltaGDP ~ total_deaths + total_cases  + stringency_index + accountRank, data=localData)
    } else{
      np=npreg(deltaGDP ~ stabilityRank  + lawRank + accountRank, na.rm = TRUE, ckertype = "uniform", data=localData)
      lm = lm(deltaGDP ~ stabilityRank  + lawRank + accountRank, data=localData)
      basic = lm(deltaGDP ~ total_cases  + stringency_index + accountRank, data=localData)
    }
    
    lm = summary(lm)$r.squared
    basic = summary(basic)$r.squared
    np = np$R2
    to_add = c(x[i],np , lm, basic)
    globalData <- rbind(globalData, to_add)
    
  }
}

names(globalData)[1] = "date"
names(globalData)[2] = "lpoly"
names(globalData)[3] = "subsetlinear"
names(globalData)[4] = "basiclinear"

plot(as.Date(globalData$date), globalData$lpoly, col = "red", pch="+", 
     xlab="", ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(as.Date(globalData$date), globalData$subsetlinear, col = "blue", pch="+", 
     xlab="", ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(as.Date(globalData$date), globalData$basiclinear, col = "green", pch="+", 
     xlab="", ylab="", xaxt='n', yaxt='n')
