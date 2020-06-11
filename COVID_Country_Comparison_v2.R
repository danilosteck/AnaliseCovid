#-------------------------
# Compare COVID between countries
# Version: 2.0
# Created on: May 5th 2020.
# Versiona date: June 11th 2020.
# Created by: Danilo Steckelberg.
#-------------------------
# Objective: Compare COVID between countries


#### Reda Brazil database ####

# Load built-in auxiliary database
wd <- getwd()
source(paste(wd,"covidFunctionsLib.R",sep = "/"))

# Read file and format dates
# https://ourworldindata.org/coronavirus-source-data
download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv","worldData.csv")
world <- read.csv(file = "worldData.csv", sep = ",", header = TRUE)

countriesSelect <- c("Brazil", "Italy", "Germany","United States", "China", "Argentina", "Russia","Spain")
countries <- c("Brasil", "Itália", "Alemanha", "Estados Unidos", "China", "Argentina", "Russia", "Espanha")
worldSelect <- world[world$location %in% countriesSelect,]
countriesList <- subsetDataByNames(world, countriesSelect)


for(i in 1:length(countriesList)){
  countriesList[[i]]$taxaCrescimentoCasos <- taxaCrescimento(countriesList[[i]]$total_cases)
  countriesList[[i]]$taxaCrescimentoCasos7d <- taxaCrescimento(daysSum(countriesList[[i]]$total_cases,7))
  countriesList[[i]]$taxaCrescimentoObitos <- taxaCrescimento(countriesList[[i]]$total_deaths)
  countriesList[[i]]$taxaCrescimentoObitos7d <- taxaCrescimento(daysSum(countriesList[[i]]$total_deaths,7))
}

for(i in 1:length(countriesList)){
  assign(make.names(countriesSelect[i]), as.data.frame(countriesList[[i]]))
}

countriesList <- taxaLetalidade(countriesList = countriesList, offsetDays = 5)


plotCasesDeaths(countriesNames = countries, countriesList = countriesList, 
                minCasesThresh = 100, daysNewCases = 7, png = FALSE, palname = "Dark2")


png(filename = "taxaCrescCasos.png", width = 900, height = 450)
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

plotCrescSatur(countriesNames = countries, countriesList = countriesList, png = FALSE,
                   minThresCases = 50, palname = "Dark2")

timeSeriesTrend(countriesNames = countries, countriesList, freq = 7, png = FALSE, palname = "Dark2", plotwidth = 800, minThres = 50)

plotTaxaLetalidade(countriesNames = countries, countriesList = countriesList, png = TRUE, plotWidth = 800)

png(filename = "TaxaLetalVsSaturacao.png", width = 480, height = 320, pointsize = 13)
plotTaxaLetalVsTotalCasos(data=world, countriesNames = countriesSelect, minCasesThres = 10000, palname = "Dark2")
dev.off()

png(filename = "PicoVsSaturacao.png", width = 480, height = 320, pointsize = 13)
plotDiasPicoVsSat(data = world, countriesNames = countriesSelect, freq = 7, timeSeriesCasesThres = 50, countriesTotalCasesThres = 1000, palname = "Set1")
dev.off()