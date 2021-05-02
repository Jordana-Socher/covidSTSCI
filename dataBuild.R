# Build data from local files. This

source("requirements.R")

path = getwd()
if (!grepl("covidSTSCI", path)){
  setwd(paste(path, "/covidSTSCI", sep = ""))
}

rm(list = ls())
covidData = read_excel('COVID.xlsx')
corruptionData = read_excel('corruption.xlsx')
gdpData  = read_excel('unGDP.xlsx')
histGDP = read.csv(file = 'histGDP.csv')

colnames(gdpData)[6] = "gdp2020"
gdpData = subset(gdpData, select = c(Country, gdp2020))
gdpData$gdp2020 <- as.numeric(as.character(gdpData$gdp2020))
gdpData$gdp2020 = gdpData$gdp2020 * 1000000000

toDelete = seq(0, length(gdpData$Country), 2)
gdpData <-  gdpData[-toDelete, ]

setDT(histGDP)
histGDP = histGDP[histGDP$Year == 2019]
colnames(histGDP)[4] = "gdp2019"
histGDP = subset(histGDP, select = c(Country.or.Area, gdp2019))

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

masterData = merge(gdpData, masterData, by.x = "Country", by.y = "location", sort = TRUE)
masterData = merge(histGDP, masterData, by.x = "Country.or.Area", by.y = "Country", sort = TRUE)
masterData$gdp2020 = masterData$gdp2020/masterData$population
masterData$deltaGDP = masterData$gdp2020 - masterData$gdp2019
attach(masterData)

masterData$corruptionRank <- as.numeric(as.character(masterData$corruptionRank))
masterData$govRank <- as.numeric(as.character(masterData$govRank))
masterData$stabilityRank <- as.numeric(as.character(masterData$stabilityRank))
masterData$regulationRank <- as.numeric(as.character(masterData$regulationRank))
masterData$lawRank <- as.numeric(as.character(masterData$lawRank))
masterData$accountRank <- as.numeric(as.character(masterData$accountRank))

# Split into "n-tiles"
masterData$gdphalf =ifelse(masterData$gdp_per_capita > mean(masterData$gdp_per_capita), yes = 1, no = 0)
masterData$gdp50 =  quantcut( masterData$gdp_per_capita ,2, levels.mean=T, na.rm=TRUE)
masterData$gdp33 =  quantcut( masterData$gdp_per_capita ,3, levels.mean=T, na.rm=TRUE)
masterData$gdp25 =  quantcut( masterData$gdp_per_capita ,4, levels.mean=T, na.rm=TRUE)
masterData$gdp10 =  quantcut( masterData$gdp_per_capita ,10, levels.mean=T, na.rm=TRUE)

save(masterData, file = "covidCorruption.RData")
