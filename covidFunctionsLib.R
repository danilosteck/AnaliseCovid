#### COVID Functions
library(data.table)
library(MASS)
library(caret); library(rpart); library(randomForest); library(rattle); library(rpart.plot)
library(RColorBrewer)
library(readxl)


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
    locationNumber <- which(names(data)=="location")
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

plotNewCasesVsTotal <- function(countriesNames, countriesList, minCasesThresh = 100, daysNewCases = 7, png = FALSE, filename = "NewCasesVsTotal.png", plotWidth = 480, plotHeight = 480, palname = "Set1"){
  if(png == TRUE){
    png(filename = filename, width = plotWidth, height = plotHeight)
    n <- length(countriesList)
    maxVecNew <- rep(0, n)
    maxVecTot <- maxVecNew
    
    for(i in 1:n){
      
      maxVecTot[i] <- max(countriesList[[i]]$total_cases)
      maxVecNew[i] <- max(daysSum(countriesList[[i]]$new_cases,7))
      
    }
    maxCases <- max(maxVecTot)*1.05
    maxNewCases <- max(maxVecNew)*1.05
    
    
    newPal <- brewer.pal(n = n, name = palname)
    colRGB <- col2rgb(newPal[1:n])
    colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
    plot(countriesList[[1]]$total_cases[countriesList[[1]]$total_cases>minCasesThresh],daysSum(countriesList[[1]]$new_cases[countriesList[[1]]$total_cases>minCasesThresh],7), log = "xy", type = "l", ylim = c(0.1*minCasesThresh,maxNewCases), xlim = c(0.8*minCasesThresh,maxCases),
         xlab = "Casos acumulados",
         ylab = "Novos casos nos últimos 7 dias",
         main = "Casos acumulados x Casos novos (escala log.)",
         yaxt = "n", xaxt = "n",
         lwd = 4.0,
         col = "black")
    
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    for(i in 2:n){
      dataPlot <- countriesList[[i]][countriesList[[i]]$total_cases>minCasesThresh,]
      lines(dataPlot$total_cases,daysSum(dataPlot$new_cases,daysNewCases), col=colVec[i], lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, bg = NA, box.col = NA)
    dev.off()
  }else{
    n <- length(countriesList)
    maxVecNew <- rep(0, n)
    maxVecTot <- maxVecNew
    
    for(i in 1:n){
      
      maxVecTot[i] <- max(countriesList[[i]]$total_cases)
      maxVecNew[i] <- max(daysSum(countriesList[[i]]$new_cases,7))
      
    }
    maxCases <- max(maxVecTot)*1.05
    maxNewCases <- max(maxVecNew)*1.05
    
    
    newPal <- brewer.pal(n = n, name = palname)
    colRGB <- col2rgb(newPal[1:n])
    colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
    plot(countriesList[[1]]$total_cases[countriesList[[1]]$total_cases>minCasesThresh],daysSum(countriesList[[1]]$new_cases[countriesList[[1]]$total_cases>minCasesThresh],7), log = "xy", type = "l", ylim = c(0.1*minCasesThresh,maxNewCases), xlim = c(0.8*minCasesThresh,maxCases),
         xlab = "Casos acumulados",
         ylab = "Novos casos nos últimos 7 dias",
         main = "Casos acumulados x Casos novos (escala log.)",
         lwd = 4.0,
         yaxt = "n", xaxt = "n",
         col = "black")
    
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    
    for(i in 2:n){
      dataPlot <- countriesList[[i]][countriesList[[i]]$total_cases>minCasesThresh,]
      lines(dataPlot$total_cases,daysSum(dataPlot$new_cases,daysNewCases), col=colVec[i], lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 0.65, bg = NA, box.col = NA)
    return(p)
  }
}

plotNewDeathsVsTotal <- function(countriesNames, countriesList, minCasesThresh = 100, daysNewCases = 7, png = FALSE, filename = "NewDeathsVsTotal.png", plotWidth = 480, plotHeight = 480, palname = "Set1"){
  if(png == TRUE){n <- length(countriesList)
  maxVecNew <- rep(0, n)
  maxVecTot <- maxVecNew
  
  for(i in 1:n){
    
    maxVecTot[i] <- max(countriesList[[i]]$total_deaths)
    maxVecNew[i] <- max(daysSum(countriesList[[i]]$new_deaths,7))
    
  }
  maxCases <- max(maxVecTot)*1.05
  maxNewCases <- max(maxVecNew)*1.05
  
  
  newPal <- brewer.pal(n = n, name = palname)
  colRGB <- col2rgb(newPal[1:n])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
  
  png(filename = filename, width = plotWidth, height = plotHeight)
  plot(countriesList[[1]]$total_deaths[countriesList[[1]]$total_deaths>minCasesThresh],daysSum(countriesList[[1]]$new_deaths[countriesList[[1]]$total_deaths>minCasesThresh],7), log = "xy", type = "l", ylim = c(0.1*minCasesThresh,maxNewCases), xlim = c(0.8*minCasesThresh,maxCases),
       xlab = "Óbitos acumulados",
       ylab = "Novos óbitos nos últimos 7 dias",
       main = "Óbitos acumulados x Óbitos novos (escala log.)",
       yaxt = "n", xaxt = "n",
       lwd = 4.0,
       col = "black")
  
  axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
  axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
  for(i in 2:n){
    dataPlot <- countriesList[[i]][countriesList[[i]]$total_deaths>minCasesThresh,]
    lines(dataPlot$total_deaths,daysSum(dataPlot$new_deaths,daysNewCases), col=colVec[i], lwd = 3)
  }
  legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, bg = NA, box.col = NA)
  dev.off()
  }else{
    n <- length(countriesList)
    maxVecNew <- rep(0, n)
    maxVecTot <- maxVecNew
    
    for(i in 1:n){
      
      maxVecTot[i] <- max(countriesList[[i]]$total_deaths)
      maxVecNew[i] <- max(daysSum(countriesList[[i]]$new_deaths,7))
      
    }
    maxCases <- max(maxVecTot)*1.05
    maxNewCases <- max(maxVecNew)*1.05
    
    
    newPal <- brewer.pal(n = n, name = palname)
    colRGB <- col2rgb(newPal[1:n])
    colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
    plot(countriesList[[1]]$total_deaths[countriesList[[1]]$total_deaths>minCasesThresh],daysSum(countriesList[[1]]$new_deaths[countriesList[[1]]$total_deaths>minCasesThresh],7), log = "xy", type = "l", ylim = c(0.1*minCasesThresh,maxNewCases), xlim = c(0.8*minCasesThresh,maxCases),
         xlab = "Óbitos acumulados",
         ylab = "Novos óbitos nos últimos 7 dias",
         main = "Óbitos acumulados x Óbitos novos (escala log.)",
         yaxt = "n", xaxt = "n",
         lwd = 4.0,
         col = "black")
    
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    for(i in 2:n){
      dataPlot <- countriesList[[i]][countriesList[[i]]$total_deaths>minCasesThresh,]
      lines(dataPlot$total_deaths,daysSum(dataPlot$new_deaths,daysNewCases), col=colVec[i], lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 0.65, bg = NA, box.col = NA)
    return(p)
  }
}

plotCasesDeaths <- function(countriesNames, countriesList, minCasesThresh = 100, daysNewCases = 7, png = FALSE, filename = "CasesAndDeaths.png", plotWidth = 960, plotHeight = 480, palname = "Set1"){
  
  n <- length(countriesList)
  NewCases <- rep(0, n)
  TotCases <- rep(0, n)
  NewDeaths <- rep(0, n)
  TotDeaths <- rep(0, n)
  
  for(i in 1:n){
    
    TotDeaths[i] <- max(countriesList[[i]]$total_deaths)
    NewDeaths[i] <- max(daysSum(countriesList[[i]]$new_deaths,7))
    TotCases[i] <- max(countriesList[[i]]$total_cases)
    NewCases[i] <- max(daysSum(countriesList[[i]]$new_cases,7))
    
  }
  maxTotDeaths <- max(TotDeaths)*1.05
  maxNewDeaths <- max(NewDeaths)*1.05
  maxTotCases  <- max(TotCases)*1.05
  maxNewCases  <- max(NewCases)*1.05


  newPal <- brewer.pal(n = n, name = palname)
  colRGB <- col2rgb(newPal[1:n])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
  if(png == TRUE){
  
    png(filename = filename, width = plotWidth, height = plotHeight)
    par(mfrow = c(1,2), oma = c(0,0,2,0))
    plot(countriesList[[1]]$total_cases[countriesList[[1]]$total_cases>minCasesThresh],daysSum(countriesList[[1]]$new_cases[countriesList[[1]]$total_cases>minCasesThresh],7), log = "xy", type = "l", ylim = c(0.1*minCasesThresh,maxNewCases), xlim = c(0.8*minCasesThresh,maxTotCases),
         xlab = "Casos acumulados",
         ylab = "Novos casos nos últimos 7 dias",
         main = "a. Casos acumulados x Casos novos (escala log.)",
         lwd = 4.0,
         yaxt = "n", xaxt = "n",
         col = "black")
    
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    
    for(i in 2:n){
      dataPlot <- countriesList[[i]][countriesList[[i]]$total_cases>minCasesThresh,]
      lines(dataPlot$total_cases,daysSum(dataPlot$new_cases,daysNewCases), col=colVec[i], lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, bg = NA, box.col = NA)
    
    plot(countriesList[[1]]$total_deaths[countriesList[[1]]$total_deaths>minCasesThresh],daysSum(countriesList[[1]]$new_deaths[countriesList[[1]]$total_deaths>minCasesThresh],7), log = "xy", type = "l", ylim = c(0.1*minCasesThresh,maxNewDeaths), xlim = c(0.8*minCasesThresh,maxTotDeaths),
         xlab = "Óbitos acumulados",
         ylab = "Novos óbitos nos últimos 7 dias",
         main = "b. Óbitos acumulados x Óbitos novos (escala log.)",
         yaxt = "n", xaxt = "n",
         lwd = 4.0,
         col = "black")
    
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    for(i in 2:n){
      dataPlot <- countriesList[[i]][countriesList[[i]]$total_deaths>minCasesThresh,]
      lines(dataPlot$total_deaths,daysSum(dataPlot$new_deaths,daysNewCases), col=colVec[i], lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, bg = NA, box.col = NA)
    title(main = "Crescimento de casos e óbitos", outer = TRUE)
    dev.off()
  }else{
    par(mfrow = c(1,2), oma = c(0,0,2,0))
    plot(countriesList[[1]]$total_cases[countriesList[[1]]$total_cases>minCasesThresh],daysSum(countriesList[[1]]$new_cases[countriesList[[1]]$total_cases>minCasesThresh],7), log = "xy", type = "l", ylim = c(0.1*minCasesThresh,maxNewCases), xlim = c(0.8*minCasesThresh,maxTotCases),
         xlab = "Casos acumulados",
         ylab = "Novos casos nos últimos 7 dias",
         main = "a. Casos acumulados x Casos novos (escala log.)",
         lwd = 4.0,
         yaxt = "n", xaxt = "n",
         col = "black")
    
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    
    for(i in 2:n){
      dataPlot <- countriesList[[i]][countriesList[[i]]$total_cases>minCasesThresh,]
      lines(dataPlot$total_cases,daysSum(dataPlot$new_cases,daysNewCases), col=colVec[i], lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 0.65, bg = NA, box.col = NA)
    
    plot(countriesList[[1]]$total_deaths[countriesList[[1]]$total_deaths>minCasesThresh],daysSum(countriesList[[1]]$new_deaths[countriesList[[1]]$total_deaths>minCasesThresh],7), log = "xy", type = "l", ylim = c(0.1*minCasesThresh,maxNewDeaths), xlim = c(0.8*minCasesThresh,maxTotDeaths),
         xlab = "Óbitos acumulados",
         ylab = "Novos óbitos nos últimos 7 dias",
         main = "b. Óbitos acumulados x Óbitos novos (escala log.)",
         yaxt = "n", xaxt = "n",
         lwd = 4.0,
         col = "black")
    
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    for(i in 2:n){
      dataPlot <- countriesList[[i]][countriesList[[i]]$total_deaths>minCasesThresh,]
      lines(dataPlot$total_deaths,daysSum(dataPlot$new_deaths,daysNewCases), col=colVec[i], lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 0.65, bg = NA, box.col = NA)
    title(main = "Crescimento de casos e óbitos", outer = TRUE)

  }
}

vectorOffset <- function(vector, offset, repVals = 0){
  newVector <- rep(repVals, length(vector))
  for(i in 1:(length(vector)-offset)){
    newVector[i+offset] <- vector[i]
  }
  return(newVector)
}

taxaLetalidade <- function(countriesList, offsetDays = 5){
  n <- length(countriesList)
  for(i in 1:n){
    newCases <- vectorOffset(countriesList[[i]]$total_cases, offsetDays)
    countriesList[[i]]$taxaLetal <- ifelse(newCases == 0,0,countriesList[[i]]$total_deaths/newCases)
  }
  return(countriesList)
}

plotTaxaLetalidade <- function(countriesNames, countriesList, png = FALSE, plotFilename = "TaxaLetalidade.png", plotWidth = 480, plotHeight = 480, palname = "Set1"){
  n <- length(countriesList)
  newPal <- brewer.pal(n = n, name = palname)
  colRGB <- col2rgb(newPal[1:n])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
  if(png == FALSE){
    plot(as.Date(countriesList[[1]]$date), countriesList[[1]]$taxaLetal,col = "black", ylim = c(0,0.20), type = "l",lwd = 4.0,
         ylab = "Taxa de letalidade",
         xlab = "",
         main = "Taxa de Letalidade por país")
    for(i in 2:n){
      lines(as.Date(countriesList[[i]]$date),countriesList[[i]]$taxaLetal,col = c("black",colVec[2:length(colVec)])[i], type = "l",lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 0.65, bg = NA, box.col = NA)
  }else{
    png(filename = plotFilename, width = plotWidth, height = plotHeight)
    plot(as.Date(countriesList[[1]]$date), countriesList[[1]]$taxaLetal,col = "black", ylim = c(0,0.20), type = "l",lwd = 4.0,
         ylab = "Taxa de letalidade",
         xlab = "",
         main = "Taxa de Letalidade por país")
    for(i in 2:n){
      lines(as.Date(countriesList[[i]]$date),countriesList[[i]]$taxaLetal,col = c("black",colVec[2:length(colVec)])[i], type = "l",lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, bg = NA, box.col = NA)
    dev.off()
  }
}

last <- function(x){
  n <- length(x)
  out <- x[n]
  return(out)
}

last7Days <- function(x){
  n <- length(x)
  if(n<=7){out <- mean(x[1:n])}else{out <- mean(x[(n-6):n])}
  return(out)
}

countriesDataAggregation <- function(worldData){

  worldData$last7DaysCases <- daysSum(worldData$new_cases, 7)
  worldData$last7DaysDeaths <- daysSum(worldData$new_deaths, 7)
  
  vecMax <- c("total_cases", "new_cases", "total_deaths", "new_deaths", "total_deaths_per_million", "total_cases_per_million", "total_tests", "last7DaysCases", "last7DaysDeaths")    
  vecLst <- c("new_cases", "new_deaths", "last7DaysCases", "last7DaysDeaths")         
  vecSum <- c("new_tests", "total_tests_per_thousand", "new_tests_per_thousand")       
  vecMed <- c("population", "population_density", "median_age", "aged_65_older", "aged_70_older", "gdp_per_capita", "extreme_poverty", "diabetes_prevalence", "female_smokers", "male_smokers", "hospital_beds_per_thousand")
  # print("step 1 OK")
  tempWorld <- worldData[,colnames(worldData) %in% vecMax]; tempWorld[is.na(tempWorld)==TRUE]<-0
  worldMax <- aggregate(worldData[,colnames(worldData) %in% vecMax], by = list(worldData$location), FUN = max)
  colnames(worldMax) <- paste("max.",c("location",vecMax), sep = "")
  # print("step 2 OK")
  
  tempWorld <- worldData[,colnames(worldData) %in% vecSum]; tempWorld[is.na(tempWorld)==TRUE]<-0
  worldSum <- aggregate(tempWorld, by = list(worldData$location), FUN = sum)
  colnames(worldSum) <- paste("sum.",c("location",vecSum), sep = "")
  
  # print("step 3 OK")
  tempWorld <- worldData[,colnames(worldData) %in% vecMed]
  worldMed <- aggregate(tempWorld, by = list(worldData$location), FUN = median)
  colnames(worldMed) <- paste("med.",c("location",vecMed), sep = "")
  # print("step 4 OK")
  tempWorld <- worldData[,colnames(worldData) %in% vecLst]
  worldLst <- aggregate(tempWorld, by = list(worldData$location), FUN = last)
  colnames(worldLst) <- paste("last.",c("location",vecLst), sep = "")
  # print("step 5 OK")
  worldAggregate <- cbind(worldMax,
                          worldSum[,(2:length(worldSum))],
                          worldMed[,(2:length(worldMed))],
                          worldLst[,(2:length(worldLst))])
  worldAggregate$med.smokers <- (worldAggregate$med.female_smokers + worldAggregate$med.male_smokers)/2
  # print("Step 6 OK")
  return(worldAggregate)
}

standardize <- function(x){
  ncols <- length(x)
  for(i in 1:ncols){
    x[,i] <- (x[,i]-mean(x[,i]))/sd(x[,i])
  }
  return(x)
}

normalize <- function(x){
  ncols <- length(x)
  for(i in 1:ncols){
    x[,i] <- (x[,i]-min(x[,i]))/(max(x[,i])-min(x[,i]))
  }
  return(x)
}

transfBoxCox <- function(x){
  
  bc <- boxcox(x ~ 1, lambda = seq(-3,3, by = 0.01))
  lambda <- bc$x[which(bc$y == max(bc$y))]
  x <- (x^lambda-1)/lambda

  pvalue <- shapiro.test(x)$p.value
  out <- list(lambda,x,pvalue)
  return(out)
}

transfBoxCoxDF <- function(x){
  ncols <- ncol(x)
  lambda <- rep(0,ncols)
  pvalue <- rep(0,ncols)
  transf <- matrix(nrow = nrow(x), ncol = ncol(x))
    for(i in 1:ncols){
      out <- transfBoxCox(x[,i])
      lambda[i] <- out[[1]]
      pvalue[i] <- out[[3]]
      transf[,i] <- out[[2]]
    }
  colnames(transf) <- colnames(x)
  out <- list(lambda,transf,pvalue)
  return(out)
}

taxaCrescimento <- function(x){
  n <- length(x)
  out <- rep(0,n)
  for(i in 2:n){
    out[i] <- ifelse(x[(i-1)] == 0,0,x[i]/x[(i-1)]-1)
  }
  return(out)
}

plotTaxaCrescimento <- function(countriesNames, countriesList, png = FALSE, plotFilename = "TaxaCrescimento.png", plotWidth = 480, plotHeight = 480, palname = "Set1"){
  n <- length(countriesList)
  newPal <- brewer.pal(n = n, name = palname)
  colRGB <- col2rgb(newPal[1:n])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
  maxVecNew <- rep(0, n)
  maxVecTot <- maxVecNew

  mindate <- min(as.Date(countriesList[[1]]$date))
  maxdate <- max(as.Date(countriesList[[1]]$date))
  for(i in 1:n){
    
    maxVecTot[i] <- max(countriesList[[i]]$taxaCrescimentoCasos)
    maxVecNew[i] <- max(countriesList[[i]]$taxaCrescimentoCasos7d)
    countriesList[[i]] <- countriesList[[i]][countriesList[[i]]$total_cases>50,]
    
  }
  MaxTax <- min(c(1.0,max(maxVecTot)*1.05))
  
  n <- length(countriesList)

  if(png == TRUE){
  
  png(filename = filename, width = plotWidth, height = plotHeight)
  plot(as.Date(countriesList[[1]]$date),countriesList[[1]]$taxaCrescimentoCasos7d, type = "l", xlim = c(mindate,maxdate), ylim = c(0,MaxTax),
       xlab = "",
       ylab = "Taxa de crescimento ao dia",
       main = "a. Crescimento a partir do primeiro caso",
       lwd = 4.0, yaxt = "n",
       col = "black")
  for(i in 2:n){
    dataPlot <- countriesList[[i]]
    lines(as.Date(dataPlot$date),dataPlot$taxaCrescimentoCasos7d, col=colVec[i], lwd = 3)
  }
  legend("topright", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, bg = NA, box.col = NA)
  axis(2, at = axTicks(2), labels = sprintf("%.0f%%",100*axTicks(2)))
  dev.off()
  }else{
    plot(as.Date(countriesList[[1]]$date),countriesList[[1]]$taxaCrescimentoCasos7d, type = "l", xlim = c(mindate,maxdate), ylim = c(0,MaxTax),
         xlab = "",
         ylab = "Taxa de crescimento ao dia",
         main = "a. Crescimento a partir do primeiro caso",
         lwd = 4.0, yaxt = "n",
         col = "black")
    for(i in 2:n){
      dataPlot <- countriesList[[i]]
      lines(as.Date(dataPlot$date),dataPlot$taxaCrescimentoCasos7d, col=colVec[i], lwd = 3)
    }
    legend("topright", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, bg = NA, box.col = NA)
    axis(2, at = axTicks(2), labels = sprintf("%.0f%%",100*axTicks(2)))
  }
}

plotTaxaCrescimentoTrunc <- function(countriesNames, countriesList, png = FALSE, plotFilename = "TaxaCrescimentoTrunc.png", plotWidth = 480, plotHeight = 480, minThresCases = 50, palname = "Set1"){
  n <- length(countriesList)
  newPal <- brewer.pal(n = n, name = palname)
  colRGB <- col2rgb(newPal[1:n])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
  maxVecNew <- rep(0, n)
  maxVecTot <- maxVecNew
  maxIndex <- length(countriesList[[1]]$date)
  for(i in 1:n){
    
    
    countriesList[[i]] <- countriesList[[i]][countriesList[[i]]$total_cases>minThresCases,]
    maxVecTot[i] <- max(countriesList[[i]]$taxaCrescimentoCasos)
    maxVecNew[i] <- max(countriesList[[i]]$taxaCrescimentoCasos7d)
  }
  MaxTax <- min(c(1,max(maxVecTot)*1.05))
  
  n <- length(countriesList)
  
  if(png == TRUE){
    
    png(filename = plotFilename, width = plotWidth, height = plotHeight)
    plot(c(1:length(countriesList[[1]]$taxaCrescimentoCasos7d)),countriesList[[1]]$taxaCrescimentoCasos7d, type = "l", xlim = c(1,maxIndex), ylim = c(0,MaxTax),
         xlab = paste("Dias após",minThresCases,"contabilizados"),
         ylab = "Taxa de crescimento ao dia",
         main = paste("b. A partir de",minThresCases,"casos"),
         lwd = 4.0, yaxt = "n",
         col = "black")
    for(i in 2:n){
      dataPlot <- countriesList[[i]]
      lines(c(1:length(dataPlot$taxaCrescimentoCasos7d)),dataPlot$taxaCrescimentoCasos7d, col=colVec[i], lwd = 3)
    }
    legend("topright", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, bg = NA, box.col = NA)
    axis(2, at = axTicks(2), labels = sprintf("%.0f%%",100*axTicks(2)))
    dev.off()
  }else{
    plot(c(1:length(countriesList[[1]]$taxaCrescimentoCasos7d)),countriesList[[1]]$taxaCrescimentoCasos7d, type = "l", xlim = c(1,maxIndex), ylim = c(0,MaxTax),
         xlab = paste("Dias após",minThresCases,"contabilizados"),
         ylab = "Taxa de crescimento ao dia",
         main = paste("b. A partir de",minThresCases,"casos"),
         lwd = 3.1, yaxt = "n",
         col = "black")
    for(i in 2:n){
      dataPlot <- countriesList[[i]]
      lines(c(1:length(dataPlot$taxaCrescimentoCasos7d)),dataPlot$taxaCrescimentoCasos7d, col=colVec[i], lwd = 3)
    }
    legend("topright", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, bg = NA, box.col = NA)
    axis(2, at = axTicks(2), labels = sprintf("%.0f%%",100*axTicks(2)))
  }
}

plotTaxaCrescSatur <- function(countriesNames, countriesList, png = FALSE, plotFilename = "TaxaCrescimentoVsSatur.png", plotWidth = 480, plotHeight = 480, minThresCases = 50, palname = "Set1"){
  
  n <- length(countriesList)
  
  newPal <- brewer.pal(n = n, name = palname)
  colRGB <- col2rgb(newPal[1:n])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
  maxVecCresc <- rep(0, n)
  minVecCresc <- rep(0, n)
  
  maxVecSat <- maxVecCresc
  minVecSat <- maxVecCresc
  shortList <- countriesList
  for(i in 1:n){

    countriesList[[i]] <- countriesList[[i]][countriesList[[i]]$total_cases>50,]
    shortList[[i]] <- data.frame(crescimento = countriesList[[i]]$taxaCrescimentoCasos7d,
                                 saturacao = daysSum(countriesList[[i]]$total_cases_per_million,7)/7)
    maxVecCresc[i] <- max(shortList[[i]]$crescimento)
    minVecCresc[i] <- min(shortList[[i]]$crescimento)
    maxVecSat[i] <- max(shortList[[i]]$saturacao)
    minVecSat[i] <- min(shortList[[i]]$saturacao)
  }
  yplotLim <- c(min(minVecSat), max(maxVecSat))
  xplotLim <- c(min(minVecCresc)+0.001, max(maxVecCresc))
  
  if(png == TRUE){
    
    png(filename = plotFilename, width = plotWidth, height = plotHeight)
    plot(x = shortList[[1]]$crescimento, y = shortList[[1]]$saturacao, type = "l", log = "xy", xlim = xplotLim, ylim = yplotLim,
         ylab = "Saturação [casos por milhão de hab.]",
         xlab = "Taxa de Crescimento de casos",
         main = "Crescimento vs. Saturação",
         xaxt = "n", yaxt = "n",
         lwd = 4.0,
         col = "black")
    
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    for(i in 2:n){
      dataPlot <- shortList[[i]]
      lines(dataPlot$crescimento,dataPlot$saturacao, col=colVec[i], lwd = 3)
    }
    legend("bottomleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, box.col = NA)
    dev.off()
  }else{
    plot(x = shortList[[1]]$crescimento, y = shortList[[1]]$saturacao, type = "l", log = "xy", xlim = xplotLim, ylim = yplotLim,
         ylab = "Saturação [casos por milhão de hab.]",
         xlab = "Taxa de Crescimento de casos",
         main = "Crescimento vs. Saturação",
         xaxt = "n", yaxt = "n",
         lwd = 4.0,
         col = "black")
    
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    for(i in 2:n){
      dataPlot <- shortList[[i]]
      lines(dataPlot$crescimento,dataPlot$saturacao, col=colVec[i], lwd = 3)
    }
    legend("bottomleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 0.7,  box.col = NA)
  }
  
}

plotCrescSatur <- function(countriesNames, countriesList, png = FALSE, plotFilename = "TaxaCrescimentok.png", plotWidth = 480, plotHeight = 480, minThresCases = 50, palname = "Set1"){
  
  n <- length(countriesList)
  
  newPal <- brewer.pal(n = n, name = palname)
  colRGB <- col2rgb(newPal[1:n])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
  maxVecCresc <- rep(0, n)
  minVecCresc <- rep(0, n)
  
  maxVecSat <- maxVecCresc
  minVecSat <- maxVecCresc
  shortList <- countriesList
  for(i in 1:n){
    
    countriesList[[i]] <- countriesList[[i]][countriesList[[i]]$total_cases>50,]
    shortList[[i]] <- data.frame(crescimento = daysSum(countriesList[[i]]$new_cases_per_million,7)/7,
                                 saturacao = daysSum(countriesList[[i]]$total_cases_per_million,7)/7)
    maxVecCresc[i] <- max(shortList[[i]]$crescimento)
    minVecCresc[i] <- min(shortList[[i]]$crescimento)
    maxVecSat[i] <- max(shortList[[i]]$saturacao)
    minVecSat[i] <- min(shortList[[i]]$saturacao)
  }
  xplotLim <- c(min(minVecSat), max(maxVecSat))
  yplotLim <- c(min(minVecCresc)+0.001, max(maxVecCresc))
  
  if(png == TRUE){
    
    png(filename = plotFilename, width = plotWidth, height = plotHeight)
    plot(x = shortList[[1]]$saturacao, y = shortList[[1]]$crescimento, type = "l", log = "xy", xlim = xplotLim, ylim = yplotLim,
         xlab = "Saturação [casos por milhão de hab.]",
         ylab = "Novos casos [por milhão de hab.]",
         main = "Crescimento vs. Saturação",
         lwd = 4.0,
         xaxt = "n",
         yaxt = "n",
         col = "black")
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    
    for(i in 2:n){
      dataPlot <- shortList[[i]]
      lines(dataPlot$saturacao,dataPlot$crescimento, col=colVec[i], lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 1, box.col = NA)
    dev.off()
  }else{
    plot(x = shortList[[1]]$saturacao, y = shortList[[1]]$crescimento, type = "l", log = "xy", xlim = xplotLim, ylim = yplotLim,
         xlab = "Saturação [casos por milhão de hab.]",
         ylab = "Novos casos [por milhão de hab.]",
         main = "Crescimento vs. Saturação",
         lwd = 4.0,
         xaxt = "n",
         yaxt = "n",
         col = "black")
    axis(1, at = axTicks(1), labels = sprintf("%g", axTicks(1)))
    axis(2, at = axTicks(2), labels = sprintf("%g", axTicks(2)))
    for(i in 2:n){
      dataPlot <- shortList[[i]]
      lines(dataPlot$saturacao,dataPlot$crescimento, col=colVec[i], lwd = 3)
    }
    legend("topleft", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 0.7,  box.col = NA)
  }
  
}

deathRatePrediction <- function(wd, thres=2000, part = 0.75, breaks = 4){
  
  wd.clean <- wd[wd$max.total_cases >thres,]
  wd.clean$output <- wd.clean$max.total_deaths_per_million/wd.clean$max.total_cases_per_million
  wd.clean <- wd.clean[,colnames(wd.clean) %in% c("med.aged_65_older",  "med.extreme_poverty", 
                                                  "med.diabetes_prevalence" , "med.smokers" , "med.hospital_beds_per_thousand" , 
                                                  "med.population_density" , "med.gdp_per_capita", "output")]
  
  wd.clean <- na.omit(wd.clean)
  wd.clean <- log(wd.clean)
  
  modFit.linearModel <- glm(output ~. , data=wd.clean);modFit.linearModel
  pred.linearModel <- predict(modFit.linearModel, wd.clean)
  lm.rsq <- 1- modFit.linearModel$deviance/modFit.linearModel$null.deviance
  
  labl <- c("A","B","C","D", "E", "F", "G", "H", "I", "J")
  labl.used <- labl[1:breaks]
  wd.clean$output <- cut((wd.clean$output), breaks = breaks, labels = labl.used,
                         include.lowest = TRUE, right = TRUE,
                         ordered_result = TRUE)
  inTrain <- createDataPartition(y = wd.clean$output,p = part, list = FALSE)
  training <- wd.clean[inTrain,]
  testing <- wd.clean[-inTrain,]
  
  modFit.decisionTree <- rpart(output~., data = training, method = "class")
  pred.decisionTree <- predict(modFit.decisionTree, testing, type = "class")
  cm.dt <- confusionMatrix(pred.decisionTree, testing$output)
  # fancyRpartPlot(modFit.decisionTree)
  # rpart.plot(modFit.decisionTree)

  
  modFit.randomForest <- randomForest(output ~. , data=training)
  pred.randomForest <- predict(modFit.randomForest, testing, type = "class")
  cm.rf <- confusionMatrix(pred.randomForest, testing$output)

  max.accuracy <- max(c(cm.dt$overall[1],cm.rf$overall[1]))
  output <- data.frame(dt.accuracy = cm.dt$overall[1], rf.accuracy = cm.rf$overall[1], lm.rsq = lm.rsq)
  return(output)
}

timeSeriesTrend <- function(countriesNames, data, freq = 7, minThres = 50, palname = "Set1", plotname = "TimeSeriesComp.png", plotwidth = 480, plotheight = 480, png = FALSE){
  out <- list()
  trendSeries <- list()
  seriesValues <- list()
  cnt <- length(countriesNames)
  maxTrend <- rep(0,cnt)
  maxDate <- rep(0,cnt)
  dataMaxTrend <- rep(0,cnt)
  for(i in 1:cnt){
    varNames <- make.names(countriesNames[i])
    time.series <- ts(data = data[[i]]$new_cases_per_million, frequency = freq)
    dec.time.series <- decompose(time.series)
    out[[i]] <- dec.time.series$trend
    
    seriesValues[[i]] <- na.omit(out[[i]][data[[i]]$total_cases>minThres])
    days <- length(seriesValues[[i]])
    time <- c(1:days)
    
    trendSeries[[i]] <- data.frame(time = c(1:days), trend = (seriesValues[[i]]))
    maxDate[i] <- max(trendSeries[[i]]$time)
    dataMaxTrend[i] <- trendSeries[[i]]$time[which(trendSeries[[i]]$trend == max(trendSeries[[i]]$trend))]
   maxTrend[i] <- max(trendSeries[[i]]$trend)
  }
  maxDF <- data.frame(countries = countriesNames,max.trend = maxTrend, max.date = dataMaxTrend)
  print(maxDF)
  newPal <- brewer.pal(n = cnt, name = palname)
  colRGB <- col2rgb(newPal[1:cnt])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)
  
  if(png == TRUE){
    png(filename = plotname, width = plotwidth, height = plotheight)
    plot(trendSeries[[1]]$time,trendSeries[[1]]$trend, type = "l", xlim = c(0,max(maxDate)), ylim = c(0,max(maxTrend)),
         ylab = "Novos casos por milhão de habitantes",
         xlab = paste("dias após",minThres,"casos"),
         main = "Novos casos por milhão de habitantes\n(removendo efeitos sazonais e aleatórios)",
         lwd = 4.0,
         col = "black")
    for(i in 2:cnt){
      dataPlot <- countriesList[[i]]
      lines(trendSeries[[i]]$time,trendSeries[[i]]$trend, col=colVec[i], lwd = 3)
    }
    legend("topright", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, bg = NA, box.col = NA)
    
    dev.off()
  }else{
    plot(trendSeries[[1]]$time,trendSeries[[1]]$trend, type = "l", xlim = c(0,max(maxDate)), ylim = c(0,max(maxTrend)),
         ylab = "Novos casos por milhão de habitantes",
         xlab = paste("dias após",minThres,"casos"),
         main = "Novos casos por milhão de habitantes\n(removendo efeitos sazonais e aleatórios)",
         lwd = 4.0,
         col = "black")
    for(i in 2:cnt){
      dataPlot <- countriesList[[i]]
      lines(trendSeries[[i]]$time,trendSeries[[i]]$trend, col=colVec[i], lwd = 3)
    }
    legend("topright", inset = 0.02, legend = countriesNames, col = c("black",colVec[2:length(colVec)]), pch = "", lwd = 2, cex = 0.7, bg = NA, box.col = NA)
    
  }
  # return(trendSeries)
}

myPlot.decomposed.ts <- function (x, mtitle = "Decomposição de série temporal", xlabel = "Tempo",axisScale = 1,...) 
{
  xx <- x$x
  if (is.null(xx)) 
    xx <- with(x, if (type == "additive") 
      random + trend + seasonal
      else random * trend * seasonal)
  plot(cbind(Observação = xx, Tendência = x$trend, Sazonalidade = x$seasonal, 
             Aleatório = x$random), main = mtitle,xlab = xlabel, cex.axis = axisScale)
}

# 
# col = c("black",colVec[2:length(colVec)]),
trend.maxNewCases <- function(data, freq = 7, timeSeriesCasesThres = 50, countriesTotalCasesThres = 10000){
  
  countriesWithTotalCaseThres <- aggregate(total_cases ~ location, data = data,FUN = max)
  countriesWithTotalCaseThres <- countriesWithTotalCaseThres$location[countriesWithTotalCaseThres$total_cases > countriesTotalCasesThres]
  data <- data[data$location %in% countriesWithTotalCaseThres,]
  
  countriesCount <- data.frame(setDT(data)[, .N, location])
  countriesWithMoreThan2Weeks <- countriesCount[countriesCount[,2]>50,1]
  data <- data[data$location %in% countriesWithMoreThan2Weeks]
  cnt <- length(countriesWithMoreThan2Weeks)
  countriesNames <- countriesWithMoreThan2Weeks
  data <- data[data$location %in% countriesNames,]
  
  out <- list()
  trendSeries <- list()
  seriesValues <- list()
  
  maxTrend <- rep(0,cnt)
  maxDate <- rep(0,cnt)
  dataMaxTrend <- rep(0,cnt)
  daysFromPeak <- rep(0,cnt)
  for(i in 1:cnt){
    current.data <- data[data$location == countriesNames[i],]
    varNames <- make.names(countriesNames[i])
    time.series <- ts(data = na.omit(current.data$new_cases_per_million), frequency = freq)
    dec.time.series <- decompose(time.series)
    out[[i]] <- dec.time.series$trend
    
    seriesValues[[i]] <- na.omit(out[[i]][current.data$total_cases>timeSeriesCasesThres])
    days <- length(seriesValues[[i]])
    time <- c(1:days)
    
    trendSeries[[i]] <- data.frame(time = c(1:days), trend = (seriesValues[[i]]))
    maxDate[i] <- max(trendSeries[[i]]$time)
    dataMaxTrend[i] <- trendSeries[[i]]$time[which(trendSeries[[i]]$trend == max(trendSeries[[i]]$trend))]
    maxTrend[i] <- max(trendSeries[[i]]$trend)
    daysFromPeak[i] <- maxDate[i]-dataMaxTrend[i]
  }
  maxDF <- data.frame(countries = countriesNames,max.trend = maxTrend, max.date = dataMaxTrend, days.from.peak = daysFromPeak, date = maxDate)
  return(maxDF)
}

plotDiasPicoVsSat <- function(data, countriesNames, freq = 7, timeSeriesCasesThres = 50, countriesTotalCasesThres = 1000, palname = "Set1"){
  
  data <- trend.maxNewCases(data, freq = 7, timeSeriesCasesThres = timeSeriesCasesThres, countriesTotalCasesThres = countriesTotalCasesThres)
  
  n <- length(countriesNames)
  newPal <- c("black",brewer.pal(n = n, name = palname))
  colRGB <- col2rgb(newPal[1:n])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.8)
  
  plot(x = data$max.date, y = data$max.trend,
       xlab = paste("Dias entre", timeSeriesCasesThres, "casos até atingir pico"),
       ylab = "Máximo de casos novos por milhão de habitantes",
       main = "Máximo de casos novos \n vs. Dias até pico",
      log = "y", lwd = 2, col = ifelse(data$days.from.peak>7,"gray","red"), cex = 1.25)
  for(i in 1:n){
    dataPlot <- data[data$countries %in% countriesNames[i],]
    points(dataPlot$max.date,dataPlot$max.trend, col=colVec[i],pch = 16, cex = 1.25)
  }
 legend("topright", inset = 0.02, legend = c(countriesNames,"","Já passaram do pico","Não chegaram no pico"), col = c(colVec[1:n],NA,"gray","red"), pch = c(rep(16,n),NA,1,1), cex = 0.8,  box.col = NA, bty = "n")
  
}

plotTaxaLetalVsTotalCasos <- function(data, countriesNames, minCasesThres = 1000, palname = "Set1"){
  n <- length(countriesNames)
  newPal <- c("black",brewer.pal(n = n, name = palname))
  colRGB <- col2rgb(newPal[1:n])
  colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.8)
  
  data <- data[data$total_cases>minCasesThres,]
  data <- data.frame(location = data$location, total_cases=data$total_cases, total_deaths=data$total_deaths, total_cases_per_million=data$total_cases_per_million)
  data.agg <- aggregate(cbind(total_cases,total_deaths,total_cases_per_million)~location, data = data, FUN = max)
  data.agg$taxaLetal <- ifelse(data.agg$total_deaths == 0,0,data.agg$total_deaths/data.agg$total_cases)
  
  plot(x = data.agg$total_cases_per_million, y = data.agg$taxaLetal, log = "x",
       ylab = "Taxa de Letalidade",
       xlab = "Total de casos por milhão de habitantes",
       main = "Saturação vs. Taxa de Letalidade",
       yaxt = "n",
       col = "gray")
  
  axis(2, at = axTicks(2), labels = sprintf("%g%%", 100*axTicks(2)))
  for(i in 1:n){
    dataPlot <- data.agg[data.agg$location %in% countriesNames[i],]
    points(dataPlot$total_cases_per_million,dataPlot$taxaLetal, col=colVec[i],pch = 16)
  }

  lines(c(1,1)*median(data.agg$total_cases_per_million),c(-1,1), lty = "dashed") 
  lines(c(10,100000),c(1,1)*median(data.agg$taxaLetal), lty = "dashed") 
  legend("topleft", inset = 0.02, legend = countriesNames, col = c(colVec[1:n]), pch = 16, cex = 0.8,  box.col = NA, bty = "n")
  
}
