
library(webshot)
library(RColorBrewer)
library(plotly)
state.lookup <- data.frame(UF = c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"),
                           region = c("N","NE","N","N","NE","NE","CO","SE","CO","NE","SE","CO","CO","N","NE","NE","NE","S","SE","NE","N","N","S","S","NE","SE","N"),
                           total.cities = c(22,102,62,16,417,184,1,78,246,217,853,79,141,144,223,185,224,399,92,167,52,15,497,295,75,645,139))

state.agg.func <- function(state.data, city.data){
  state.agg.data <- aggregate(data = state.data, cbind(last_available_confirmed,last_available_deaths,estimated_population_2019)~state, FUN = max)
  state.agg.data$death_rate <- ifelse(state.agg.data$last_available_confirmed == 0,0,state.agg.data$last_available_deaths/state.agg.data$last_available_confirmed)
  state.agg.data$cases_per_100k <- state.agg.data$last_available_confirmed/state.agg.data$estimated_population_2019*1e5
  state.agg.data$deaths_per_100k <- state.agg.data$last_available_deaths/state.agg.data$estimated_population_2019*1e5
  state.agg.data$total.cities <- state.lookup$total.cities[match(x = state.agg.data$state,table = state.lookup$UF)]
  
  city.count <- aggregate(data = city.data, cbind(state,last_available_deaths)~city, FUN = max)
  city.count.deaths <- city.count[city.count$last_available_deaths > 0,]
  
  state.agg.data$cities.with.cases <- aggregate(data = city.count, city~state, FUN = length)[,2]/state.agg.data$total.cities
  state.agg.data$cities.with.deaths <- aggregate(data = city.count.deaths, city~state, FUN = length)[,2]/state.agg.data$total.cities
  state.agg.data$region <- state.lookup$region[match(state.agg.data$state,table = state.lookup$UF)]
  return(state.agg.data)
}

city.agg.func <- function(city.data){
  state.by.city <- unique(data.frame(state = city.data$state,city = city.data$city))
  
  city.agg.data <- aggregate(data = city.data, cbind(last_available_confirmed,last_available_deaths,estimated_population_2019)~city, FUN = max)
  city.agg.data$state <- state.by.city$state[match(x = city.agg.data$city,table = state.by.city$city)]
  city.agg.data$death_rate <- ifelse(city.agg.data$last_available_confirmed == 0,0,city.agg.data$last_available_deaths/city.agg.data$last_available_confirmed)
  city.agg.data$cases_per_100k <- city.agg.data$last_available_confirmed/city.agg.data$estimated_population_2019*1e5
  city.agg.data$deaths_per_100k <- city.agg.data$last_available_deaths/city.agg.data$estimated_population_2019*1e5
  city.agg.data$region <- state.lookup$region[match(city.agg.data$state,table = state.lookup$UF)]
  return(city.agg.data)
}


vectorDer <- function(x){
  der <- x*0
  for(i in 2:length(x)){
    der[i] <- x[i]-x[(i-1)]
  }
  return(der)
}

subsetDataByNames <- function(data, vectorOfStrings){
  out <- list()
  for(i in 1:length(vectorOfStrings)){
    varNames <- make.names(vectorOfStrings[i])
    locationNumber <- which(names(data)=="state")
    out[[i]] <- assign(varNames, subset(data, data[,locationNumber] == vectorOfStrings[i]))
  }
  return(out)
}

daysSum <- function(x,n){
  out <- 0*x
  for(i in 1:length(x)){
    j <- length(x)-i+1
    if(j>=n){
      out[j] <- sum(x[(j-n):j])
    }else{out[j] <- sum(x[1:j])}
  }
  return(out)
}

plotNewCasesVsTotal <- function(names, dataList, daysNewCases = 7, palname = "Set1"){

  n <- length(dataList)
  maxVecNew <- rep(0, n)
  maxVecTot <- maxVecNew
  minDate <- rep(0,n)
  maxDate <- rep(0,n)
  for(i in 1:n){
    
    dataList[[i]] <- aggregate(data = as.data.frame(dataList[[i]]),cbind(last_available_confirmed,last_available_deaths)~date, FUN = sum)

    
    dataList[[i]]$new_cases <- vectorDer(dataList[[i]]$last_available_confirmed)
    
    dataList[[i]]$total_cases <- (dataList[[i]]$last_available_confirmed)
    minDate[i] <- min((dataList[[i]]$date))
    maxDate[i] <- max((dataList[[i]]$date))
    maxVecTot[i] <- max(dataList[[i]]$total_cases)
    maxVecNew[i] <- max(daysSum(dataList[[i]]$new_cases,daysNewCases))
    
  }
  minDate <- min(minDate)
  maxDate <- max(maxDate)
  dates <- seq(minDate,maxDate, by = 1)
  cases <- matrix(0, ncol = n, nrow = (maxDate-minDate+1))
  maxCases <- max(maxVecTot)*1.05
  maxNewCases <- max(maxVecNew)*1.05
  new_cases <- cases
  for(i in 1:n){

    cases[,i] <- dataList[[i]]$total_cases[match(dates,table = (dataList[[i]]$date))]
    cases[,i][is.na(cases[,i])] <- 0
    new_cases[,i] <- vectorDer(cases[,i])
    new_cases[,i] <- daysSum(new_cases[,i],daysNewCases)
  }
  colnames(cases) <- names
  # cases[is.na(cases)] <- 0
  new_cases[is.na(new_cases)] <- 0

  colnames(new_cases) <- names
  plotData <- data.frame(date = dates, cases=cases)
  fig <- plot_ly(x = cases[,1], y = new_cases[,1], type = "scatter", mode = "line", name = names[1])
  for(i in 2:n){
   fig <- fig %>% add_lines(x = cases[,i], y = new_cases[,i], name = names[i])
  }
  fig <- fig %>% layout(fig, xaxis = list(type = "log", title = "Casos totais registrados"),
                yaxis = list(type = "log", title = paste("Casos novos nos últimos ",daysNewCases," dias")),
                title ="Casos novos versus casos totais (escala log.)",
                margin = list(l=80, t=70, b=50, r=40))
  fig
}

plotNewDeathsVsTotal <- function(countriesNames, dataList, minCasesThresh = 100, daysNewCases = 7, png = FALSE, filename = "NewDeathsVsTotal.png", plotWidth = 480, plotHeight = 480, palname = "Set1"){
  if(png == TRUE){n <- length(dataList)
  maxVecNew <- rep(0, n)
  maxVecTot <- maxVecNew
  
  for(i in 1:n){
    
    maxVecTot[i] <- max(dataList[[i]]$total_deaths)
    maxVecNew[i] <- max(daysSum(dataList[[i]]$new_deaths,7))
    
  }
  maxCases <- max(maxVecTot)*1.05
  maxNewCases <- max(maxVecNew)*1.05
  
  
  newPal <- brewer.pal(n = n, name = palname)
  colRGB <- col2rgb(newPal[1:n])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
  
  png(filename = filename, width = plotWidth, height = plotHeight)
  plot(dataList[[1]]$total_deaths[dataList[[1]]$total_deaths>minCasesThresh],daysSum(dataList[[1]]$new_deaths[dataList[[1]]$total_deaths>minCasesThresh],7), log = "xy", type = "l", ylim = c(0.1*minCasesThresh,maxNewCases), xlim = c(0.8*minCasesThresh,maxCases),
       xlab = "Óbitos acumulados",
       ylab = "Novos óbitos nos últimos 7 dias",
       main = "Óbitos acumulados x Óbitos novos (escala log.)",
       yaxt = "n", xaxt = "n",
       lwd = 4.0,
       col = "black")
  
  axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
  axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
  for(i in 2:n){
    dataPlot <- dataList[[i]][dataList[[i]]$total_deaths>minCasesThresh,]
    lines(dataPlot$total_deaths,daysSum(dataPlot$new_deaths,daysNewCases), col=colVec[i], lwd = 3)
  }
  legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, bg = NA, box.col = NA)
  dev.off()
  }else{
    n <- length(dataList)
    maxVecNew <- rep(0, n)
    maxVecTot <- maxVecNew
    
    for(i in 1:n){
      
      maxVecTot[i] <- max(dataList[[i]]$total_deaths)
      maxVecNew[i] <- max(daysSum(dataList[[i]]$new_deaths,7))
      
    }
    maxCases <- max(maxVecTot)*1.05
    maxNewCases <- max(maxVecNew)*1.05
    
    
    newPal <- brewer.pal(n = n, name = palname)
    colRGB <- col2rgb(newPal[1:n])
    colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
    plot(dataList[[1]]$total_deaths[dataList[[1]]$total_deaths>minCasesThresh],daysSum(dataList[[1]]$new_deaths[dataList[[1]]$total_deaths>minCasesThresh],7), log = "xy", type = "l", ylim = c(0.1*minCasesThresh,maxNewCases), xlim = c(0.8*minCasesThresh,maxCases),
         xlab = "Óbitos acumulados",
         ylab = "Novos óbitos nos últimos 7 dias",
         main = "Óbitos acumulados x Óbitos novos (escala log.)",
         yaxt = "n", xaxt = "n",
         lwd = 4.0,
         col = "black")
    
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    for(i in 2:n){
      dataPlot <- dataList[[i]][dataList[[i]]$total_deaths>minCasesThresh,]
      lines(dataPlot$total_deaths,daysSum(dataPlot$new_deaths,daysNewCases), col=colVec[i], lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 0.65, bg = NA, box.col = NA)
    return(p)
  }
}
