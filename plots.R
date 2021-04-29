source("dataBuild.R")
load("covidCorruption.RData")

# Summary plots below are not too substantive. Goal is to visualize the new data
# and evidence our prediction argument. Note, our project pays no attention to a
# time series, where we have isolated the data from the last day in the dataset.
# What might be worthwhile, is including (down the road) line graphs connecting
# three points, for a handful of countries, over time. For example, Mar1, June1, Oct19.

plot1 = ggplot(data =masterData, aes(x = caseRatePer100k, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot2 = ggplot(data =masterData, aes(x = corruptionRank, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot3 = ggplot(data =masterData, aes(x = govRank, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot4 = ggplot(data =masterData, aes(x = stabilityRank, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot5 = ggplot(data =masterData, aes(x = gdp_per_capita, y = deathRatePer100k)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot6 = ggplot(data =masterData, aes(x = gdp_per_capita, y = corruptionRank)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
plot7 = ggplot(data =masterData, aes(x = gdp_per_capita, y = corruptionRank)) + geom_text(aes(label = location), size = 2) + geom_smooth(method = "lm")
