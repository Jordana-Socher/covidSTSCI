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
      lm_deaths_p = unname(summary(lm)$coefficients[,4]["total_deaths"])
      
      # Kitchen Sink Model
      basic = lm(deltaGDP ~ total_deaths + total_cases  + stringency_index + accountRank, data=localData)
    } else{
      np=npreg(deltaGDP ~ stabilityRank  + lawRank + accountRank, na.rm = TRUE, ckertype = "uniform", data=localData)
      lm = lm(deltaGDP ~ stabilityRank  + lawRank + accountRank, data=localData)
      basic = lm(deltaGDP ~ total_cases  + stringency_index + accountRank, data=localData)
      lm_deaths_p = 0
    }
  
    lm_stability_p = unname(summary(lm)$coefficients[,4]["stabilityRank"])
    lm_law_p = unname(summary(lm)$coefficients[,4]["lawRank"])
    lm_account_p = unname(summary(lm)$coefficients[,4]["accountRank"])
    
    lm = summary(lm)$r.squared
    basic = summary(basic)$r.squared
    np = np$R2
    to_add = c(x[i],np , lm, basic,  lm_stability_p,  lm_law_p,  lm_account_p,  lm_deaths_p)
    globalData <- rbind(globalData, to_add)
    
  }
}

names(globalData)[1] = "date"
names(globalData)[2] = "lpoly"
names(globalData)[3] = "subsetlinear"
names(globalData)[4] = "basiclinear"
names(globalData)[5] = "lm_stability_p"
names(globalData)[6] = "lm_law_p"
names(globalData)[7] = "lm_account_p"
names(globalData)[8] = "lm_deaths_p"



plot(as.Date(globalData$date), globalData$lpoly, col = "#365D8DFF", pch="+", 
     xlab="", ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(as.Date(globalData$date), globalData$subsetlinear, col = "#47C16EFF", pch="+", 
     xlab="", ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(as.Date(globalData$date), globalData$basiclinear, col = "#443A83FF", pch="+", 
     xlab="Date", ylab="R2")


<<<<<<< HEAD

plot(as.Date(globalData$date), globalData$lm_stability_p, col = "#365D8DFF", pch="+", 
=======
globalData$lpoly=as.numeric(globalData$lpoly)
sd_lpoly=(sum(globalData$lpoly-mean(globalData$lpoly))/(length(globalData$lpoly)-1))**(1/2)
sd_lpoly

globalData$subsetlinear=as.numeric(globalData$subsetlinear)
sd_subsetlinear=(sum(globalData$subsetlinear-mean(globalData$subsetlinear))/(length(globalData$subsetlinear)-1))**(1/2)
sd_subsetlinear

globalData$basiclinear=as.numeric(globalData$basiclinear)
sd_basiclinear=(sum(globalData$basiclinear-mean(globalData$basiclinear))/(length(globalData$basiclinear)-1))**(1/2)
sd_basiclinear

plot(as.Date(globalData$date), globalData$lpoly, col = "red", pch="+", 
>>>>>>> 7e186262a296f9521bdece0dfc89d2edb41f39c4
     xlab="", ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(as.Date(globalData$date), globalData$lm_law_p, col = "#443A83FF", pch="+", 
     xlab="", ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(as.Date(globalData$date), globalData$lm_account_p, col = "#47C16EFF", pch="+", 
     xlab="", ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(as.Date(globalData$date), globalData$lm_deaths_p, col = "#3a5dae", pch="+", 
     xlab="", ylab="")



plot(as.Date(globalData$date), globalData$subsetlinear, col = "#47C16EFF", pch="+", 
     xlab="", ylab="", xaxt='n', yaxt='n')
par(new=TRUE)
plot(as.Date(globalData$date), globalData$basiclinear, col = "#443A83FF", pch="+", 
     xlab="Date", ylab="R2")


