#-------------------------
# Compare COVID between countries
# Version: 1.0
# Created on: May 5th 2020.
# Versiona date: May 5th 2020.
# Created by: Danilo Steckelberg.
#-------------------------
# Objective: Compare COVID between countries


#### Reda Brazil database ####
# Read file and format dates
library(readxl)
library(RColorBrewer)
# setwd("/home/danilo/Documents/Projects/Doing/Coronavirus/")
source("covidFunctionsLib.R")
# https://ourworldindata.org/coronavirus-source-data

download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv","worldData.csv")
world <- read.csv(file = "worldData.csv", sep = ",", header = TRUE)

countriesSelect <- c("Brazil", "Italy", "Germany","United States", "China", "Argentina", "Russia", "Spain")
countries <- c("Brasil", "Itália", "Alemanha", "Estados Unidos", "China", "Argentina", "Russia", "Espanha")
worldSelect <- world[world$location %in% countriesSelect,]
countriesList <- subsetDataByNames(world, countriesSelect)

countriesList <- taxaLetalidade(countriesList = countriesList, offsetDays = 0)
plotTaxaLetalidade(countriesNames = countries, countriesList = countriesList, png = TRUE)

for(i in 1:length(countriesList)){
  countriesList[[i]]$taxaCrescimentoCasos <- taxaCrescimento(countriesList[[i]]$total_cases)
  countriesList[[i]]$taxaCrescimentoCasos7d <- taxaCrescimento(daysSum(countriesList[[i]]$total_cases,7))
  countriesList[[i]]$taxaCrescimentoObitos <- taxaCrescimento(countriesList[[i]]$total_deaths)
  countriesList[[i]]$taxaCrescimentoObitos7d <- taxaCrescimento(daysSum(countriesList[[i]]$total_deaths,7))
}

for(i in 1:length(countriesList)){
  assign(make.names(countriesSelect[i]), as.data.frame(countriesList[[i]]))
}

plotCasesDeaths(countriesNames = countries, countriesList = countriesList, 
                minCasesThresh = 100, daysNewCases = 7, png = TRUE, palname = "Dark2")

# plotNewCasesVsTotal(countriesNames = countries, countriesList = countriesList, 
#                     minCasesThresh = 100, daysNewCases = 7, png = FALSE, palname = "Dark2")
# 
# plotNewDeathsVsTotal(countriesNames = countries, countriesList = countriesList, 
#                      minCasesThresh = 10, daysNewCases = 7, png = FALSE, palname = "Dark2")

png(filename = "test.png", width = 900, height = 450)
par(mfrow = c(1,2), oma = c(0,0,2,0))
plotTaxaCrescimento(countriesNames = countries, countriesList = countriesList, 
                    png = FALSE, palname = "Dark2")
plotTaxaCrescimentoTrunc(countriesNames = countries, countriesList = countriesList, png = FALSE,
                         minThresCases = 100, palname = "Dark2")
title("Taxa de Crescimento", outer = TRUE)
dev.off()

timeseries <- ts(United.States$new_cases_per_million, frequency = 7)
png(filename = "USTimeSeries.png", width = 800, height = 480)
myPlot.decomposed.ts(decompose(timeseries), xlab = "Semanas", axisScale = 1.5)

dev.off()


# plotTaxaCrescimentoTrunc(countriesNames = countries, countriesList = countriesList, png = TRUE,
                         # minThresCases = 100, palname = "Dark2")

# plotTaxaCrescSatur(countriesNames = countries, countriesList = countriesList, png = TRUE,
#                    minThresCases = 50, palname = "Dark2")

plotCrescSatur(countriesNames = countries, countriesList = countriesList, png = TRUE,
                   minThresCases = 50, palname = "Dark2")

accuracy <- deathRatePrediction(wd, thres=2000, part = 0.7, breaks = 5)

timeSeriesTrend(countriesNames = countries, countriesList, freq = 7, png = FALSE, palname = "Dark2", plotwidth = 800, minThres = 100)

png(filename="Trend.png",width = 900, height = 1200)
par(mfrow = c(3,1))
timeSeriesTrend(countriesNames = countries, countriesList, freq = 7, png = FALSE, palname = "Dark2", minThres = 20)
timeSeriesTrend(countriesNames = countries, countriesList, freq = 7, png = FALSE, palname = "Dark2", minThres = 50)
timeSeriesTrend(countriesNames = countries, countriesList, freq = 7, png = FALSE, palname = "Dark2", minThres = 1000)
dev.off()


df <- trend.maxNewCases(world, freq = 7, timeSeriesCasesThres = 50)

# wd <- countriesDataAggregation(world)
# ratioCases <- wd$last.last7DaysCases/wd$max.last7DaysCases
# ratioTotCasesPerPop <- wd$max.total_cases/wd$med.population
# wd$med.smokers <- (wd$med.female_smokers+wd$med.male_smokers)/2
# plot(ratioCases,ratioTotCasesPerPop)
# points(ratioCases[wd$max.location == "Brazil"],ratioTotCasesPerPop[wd$max.location == "Brazil"], col = "yellow", pch = 16)
# points(ratioCases[wd$max.location == "Italy"],ratioTotCasesPerPop[wd$max.location == "Italy"], col = "green", pch = 16)
# points(ratioCases[wd$max.location == "United States"],ratioTotCasesPerPop[wd$max.location == "United States"], col = "blue", pch = 16)
# points(ratioCases[wd$max.location == "China"],ratioTotCasesPerPop[wd$max.location == "China"], col = "magenta", pch = 16)
# 
# BRZTS <- ts(data = Germany$new_cases_per_million, frequency = 7)
# plot(decompose(BRZTS))
# 
# ncluster = 3
# 
# 
# riskGroupsNames <- c("max.location","med.aged_65_older","med.diabetes_prevalence","med.smokers","med.hospital_beds_per_thousand")
# riskGroupsData <- wd[,colnames(wd) %in% riskGroupsNames]
# riskGroupsData <- na.omit(riskGroupsData)
# riskGroupsCountries <- riskGroupsData$max.location
# riskGroupsData <- riskGroupsData[,2:length(riskGroupsData)]
# 
# 
# # xcluster <- log(riskGroupsData$med.diabetes_prevalence)
# # ycluster <- log((riskGroupsData$med.female_smokers+riskGroupsData$med.male_smokers)/2)
# clusters <- kmeans(x = riskGroupsData, centers = ncluster,nstart = 50)
# # plot(xcluster,ycluster, col = clusters$cluster, pch = 16)
# # points(xcluster[clusters$cluster==5],ycluster[clusters$cluster==5], pch = 16, col = "red")
# 
# # riskGroupsData <- cbind(riskGroupsData,clusters$cluster)
# scaleRiskGroupsData <- as.data.frame(transfBoxCoxDF(riskGroupsData)[[2]])
# scaleRiskGroupsData$med.hospital_beds_per_100k <- ifelse(scaleRiskGroupsData$med.hospital_beds_per_100k==0,0,1/scaleRiskGroupsData$med.hospital_beds_per_100k)
# 
# scaleRiskGroupsData <- normalize(scaleRiskGroupsData)
# # scaleRiskGroupsData[,8] <- clusters$cluster
# df <- data.matrix(aggregate(scaleRiskGroupsData, by = list(clusters$cluster), FUN = mean), rownames.force = NA)
# col<- colorRampPalette(c("green", "white", "orange"))(256)
# par(oma = c(5,10,0,0))
# heatmap(df[,2:(length(df[1,]))],
#         cexRow = 0.8,
#         cexCol = 0.8,
#         col = colorRampPalette(c("green", "white", "orange"))(256))
# par(oma = c(0,0,0,0))
# wd2 <- wd[wd$max.location %in% riskGroupsCountries,]
# wd2$death_rate <- wd2$max.total_deaths/wd2$max.total_cases
# wd2$clusters <- clusters$cluster
# 
# library(lattice)
# 
# # wd3 <- riskGroupsData
# 
# splom(log(wd2[,colnames(wd2) %in% c("max.total_deaths",
#                                     "max.new_deaths",
#                                     "max.total_deaths_per_million",
#                                     "max.new_cases",
#                                     "death_rate")]), groups = wd2$clusters, 
#       auto.key = list(space = "right",title = "cluster", cex.title = 0.7))
# legend("topright", legend = c(1:ncluster), col = c(1:ncluster), pch = 1)
# 
# boxplot(log(death_rate) ~ clusters, data = wd2)
# boxplot(death_rate ~ clusters, data = wd2)
# datatest <- wd2[,colnames(wd2) %in% c("med.aged_65_older",  "med.extreme_poverty", 
#                                       "med.diabetes_prevalence" , "med.smokers" , "med.hospital_beds_per_100k" , 
#                                       "med.population_density" , "med.gdp_per_capita", "death_rate")]
# datatest <- na.omit(datatest+0.1)
# contrynamesdatatest <- wd$max.location[c(as.numeric(rownames(datatest)))]
# wd3 <- (normalize(datatest)+0.1)
# 
# mod <- glm(death_rate ~ ., data = wd3)
# plot(wd3$death_rate,predict(mod, wd3))
# lines(c(-2,1),c(-2,1), col = "red")
# text(x = 0.6, 0.3, labels = round(1 - (mod$deviance/mod$null.deviance),3))
# 
# transfBoxCox(datatest$med.aged_65_older)
# k<-(transfBoxCoxDF(datatest))
# datapred <- data.frame(k[[2]])
# 
# datapred <- normalize(log(datatest))
# mod2 <- glm(death_rate ~ ., data = datapred)
# plot(datapred$death_rate,predict(mod2, datapred))
# lines(c(-60,10),c(-60,10), col = "red")
# text(x = 0.6, y = 0.25, labels = round(1 - (mod2$deviance/mod2$null.deviance),3))
# 
# 
# out <- boxcox(datatest$med.aged_65_older ~ 1, lambda = seq(-2,2, by = 0.01))
# lambda <- out$x[which(out$y == max(out$y))]
# T_out <- (datatest$med.aged_65_older^lambda-1)/lambda



#### Machine Learning test ####

# Limiting analysis to countries that only have > 10k cases.


# printClusters(5)  

#### Comparisons ####
# Tests per 1M inhabitants
# New cases vs Total cases
# Death rates
# Cases per inhabitants