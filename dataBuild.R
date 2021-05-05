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

# Merges 2019 country corruption data onto covid data for regression analysis. Renames columns to shorten
masterData = merge(covidData, corruptionData, by.x = "iso_code", by.y = "Country Code", sort = TRUE)
rm(covidData, corruptionData)
names = names(masterData)
names[15] = 'corruptionRank'
names[16] = 'govRank'
names[17] = 'stabilityRank'
names[18] = 'regulationRank'
names[19] = 'lawRank'
names[20] = 'accountRank'
colnames(masterData) = names

masterData = merge(gdpData, masterData, by.x = "Country", by.y = "location", sort = TRUE)
masterData = merge(histGDP, masterData, by.x = "Country.or.Area", by.y = "Country", sort = TRUE)
masterData$gdp2020 = masterData$gdp2020/masterData$population
masterData$deltaGDP = (masterData$gdp2020 - masterData$gdp2019)/masterData$gdp2019

masterData$corruptionRank <- as.numeric(as.character(masterData$corruptionRank))
masterData$govRank <- as.numeric(as.character(masterData$govRank))
masterData$stabilityRank <- as.numeric(as.character(masterData$stabilityRank))
masterData$regulationRank <- as.numeric(as.character(masterData$regulationRank))
masterData$lawRank <- as.numeric(as.character(masterData$lawRank))
masterData$accountRank <- as.numeric(as.character(masterData$accountRank))


save(masterData, file = "covidCorruption.RData")
