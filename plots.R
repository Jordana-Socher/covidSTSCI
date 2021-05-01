source("dataBuild.R")
load("covidCorruption.RData")

# Summary plots below are not too substantive. Goal is to visualize the new data
# and evidence our prediction argument. Note, our project pays no attention to a
# time series, where we have isolated the data from the last day in the dataset.
# What might be worthwhile, is including (down the road) line graphs connecting
# three points, for a handful of countries, over time. For example, Mar1, June1, Oct19.

#install.packages(c("cowplot","googleway", "ggplot2", "ggrepel",
#                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", 
#                   "rgeos", readxl))         
library("readxl")
library(dplyr)
library(tidyr)
library("ggplot2")  
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)

#bringing in covid data alone to get the specific max case rate per country 
covid<-read_excel("COVID.xlsx")

#original plots
plot1 = ggplot(data =masterData, aes(x = caseRatePer100k, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot2 = ggplot(data =masterData, aes(x = corruptionRank, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot3 = ggplot(data =masterData, aes(x = govRank, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot4 = ggplot(data =masterData, aes(x = stabilityRank, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot5 = ggplot(data =masterData, aes(x = gdp_per_capita, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot6 = ggplot(data =masterData, aes(x = gdp_per_capita, y = corruptionRank)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot7 = ggplot(data =masterData, aes(x = gdp_per_capita, y = corruptionRank)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")

plot1
plot2
plot3
plot4
plot6
plot7

#adding case rate to covid data and creating a new data frame with just the countries and their max case rate
covid<-covid%>%drop_na(total_cases, population, gdp_per_capita)
covid$caseRatePer100k = covid$total_cases/(covid$population/100000)
covid_max_rate_df<-covid%>%group_by(location)%>%summarise(max(caseRatePer100k))

#using ne_countries to get map of countries 
world <- ne_countries(scale = "medium", returnclass = "sf")

#merging world df with covid_max_rate_df to plot and renaming column
world_1=merge(world, covid_max_rate_df, by.x="name", by.y="location")
names(world_1)[names(world_1) == "max(caseRatePer100k)"] <- "max_case_rate"

#merging world with masterData df to plot corruption and gdp 
world_2=merge(world, masterData, by.x="name", by.y="location")
world_2$corruptionRank=as.double(world_2$corruptionRank)

#world plot of max case rate per country
ggplot(data = world_1) +
  geom_sf(aes(fill = max_case_rate)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

#world plot of gdp per country
ggplot(data = world_2) +
  geom_sf(aes(fill = gdp_per_capita)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

#world plot of corruption per country
ggplot(data = world_2) +
  geom_sf(aes(fill = corruptionRank)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")



