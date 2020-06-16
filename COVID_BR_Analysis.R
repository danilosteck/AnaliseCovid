

# https://brasil.io/dataset/covid19/caso_full/?format=csv
wd <- getwd()
source(paste(wd,"BRZcovidFunctionsLib.R",sep = "/"))

# download.file("https://brasil.io/dataset/covid19/caso_full/?format=csv","brazilData.csv")
download.file("https://data.brasil.io/dataset/covid19/caso_full.csv.gz","brazilDataGZ.csv.gz")
BRZ.Covid.Data<-read.csv(gzfile(description = "brazilDataGZ.csv.gz"))
BRZ.Covid.Data$date <- as.Date(BRZ.Covid.Data$date)
# BRZ.Covid.Data <- read.csv(file = "brazilData.csv", sep = ",", header = TRUE)

state.data <- BRZ.Covid.Data[BRZ.Covid.Data$place_type == "state",]
city.data <- BRZ.Covid.Data[BRZ.Covid.Data$place_type != "state",]

state.agg.data <- state.agg.func(state.data, city.data)
city.agg.data <- city.agg.func(city.data)

#### Casos e óbitos por 100 mil habitantes ####
fig <- plot_ly(data = state.agg.data, x = ~cases_per_100k,y = ~deaths_per_100k,type = "scatter", mode = "markers",
        text = ~paste("Casos por 100k:",round(cases_per_100k,1),
                      "<br>Óbitos por 100k:",round(deaths_per_100k,1),"<br>Letalidade:",
                      100*round(death_rate,3),"%",
                      "<br>UF:",state),
        marker = list(
          color=~death_rate,
          colorscale='Jet',
          size=10,
          opacity=.9,
          colorbar=list(
            title='Taxa de Letalidade'
          )))
layout(fig, xaxis = list(title = "Casos por 100 mil habitantes",type = "log"),
       yaxis = list(title = "Óbitos por 100 mil habitantes", type = "log"),
       title ="Casos e óbitos por 100 mil hab. por UF",
       margin = list(l=80, t=70, b=50, r=40))

#### Porcentagem de municípios com óbitos e com casos ####
fig <- plot_ly(data = state.agg.data, x = ~cities.with.cases,y = ~cities.with.deaths,
               type = "scatter",mode="markers", color = ~region, colors = "Dark2",
               text = ~paste("Cidades com casos:",100*round(cities.with.cases,3),"%",
                             "<br>Cidades com obitos:",100*round(cities.with.deaths,3),"%","<br>Letalidade:",
                             100*round(death_rate,3),"%",
                             "<br>UF:",state))
layout(fig, xaxis = list(title = "Porcentagem de cidades com casos", range = c(0,1.05)),
       yaxis = list(title = "Porcentagem de cidades com óbitos", range = c(0,1.05)),
       title ="Taxa de cidades com casos e óbitos por UF",
       margin = list(l=80, t=70, b=50, r=40))

#### Cities #1 ####

citiesCases <- plot_ly(data = city.agg.data, x = ~cases_per_100k,y = ~deaths_per_100k,type = "scatter", mode = "markers",
               text = ~paste("Casos por 100k:",round(cases_per_100k,1),
                             "<br>Óbitos por 100k:",round(deaths_per_100k,1),"<br>Letalidade:",
                             100*round(death_rate,3),"%",
                             "<br>UF:",state),
               marker = list(
                 color=~death_rate,
                 colorscale='Jet',
                 size=10,
                 opacity=.9,
                 colorbar=list(
                   title='Taxa de Letalidade',orientation = "h"
                 )))
citiesCases <- citiesCases %>% layout(citiesCases, xaxis = list(title = "Casos por 100 mil habitantes",type = "log"),
       yaxis = list(title = "Óbitos por 100 mil habitantes", type = "log"),
       title ="Casos e óbitos por 100 mil hab. por UF",
       margin = list(l=80, t=70, b=50, r=40))

citiesCasesRegion <- plot_ly(data = city.agg.data, x = ~cases_per_100k,y = ~deaths_per_100k,type = "scatter", mode = "markers",
               color = ~region, colors = "Dark2", marker = list(opacity = 0.5, size = 10),
               text = ~paste("Casos por 100k:",round(cases_per_100k,1),
                             "<br>Óbitos por 100k:",round(deaths_per_100k,1),"<br>Letalidade:",
                             100*round(death_rate,3),"%",
                             "<br>UF:",state,"<br>Cidade:",city))
citiesCasesRegion <- citiesCasesRegion %>% layout(citiesCasesRegion, xaxis = list(title = "Casos por 100 mil habitantes",type = "log"),
       yaxis = list(title = "Óbitos por 100 mil habitantes", type = "log"),
       title ="Casos e óbitos por 100 mil hab. por UF",
       margin = list(l=80, t=70, b=50, r=40))
citiesCases
citiesCasesRegion
#### End ####

city.agg.data <- city.agg.func(city.data)
city.plot <- city.agg.data[city.agg.data$last_available_confirmed>5,]
city.plot <- city.plot[city.agg.data$last_available_deaths>0,]

plot_ly(data = city.plot, x = ~cases_per_100k,y = ~deaths_per_100k,
        color = ~death_rate, type = "scatter", text = ~paste("Casos por 100k:",round(cases_per_100k,0),
                                           "<br>Óbitos por 100k:",round(deaths_per_100k,0),
                                           "<br>Letalidade:",100*round(death_rate,3),"%",
                                           "<br>UF:",state,
                                           "<br>Cidade:",city))
layout(fig, yaxis = list(type = "log"),
       xaxis = list(type = "log"))

statesList <- subsetDataByNames(BRZ.Covid.Data,vectorOfStrings = unique(BRZ.Covid.Data$state))

plotNewCasesVsTotal(names = unique(BRZ.Covid.Data$state), dataList = statesList, daysNewCases = 14, palname = "Set1")

plot(statesList[[1]]$date,statesList[[1]]$last_available_confirmed)
fig <- plot_ly(x = a, y = b[,1], type = "scatter", mode = "line")
fig <- fig %>% add_lines(x = a, y = b[,2])
fig
palname <- "Dark2"
newPal <- brewer.pal(n = n, name = palname)
colRGB <- col2rgb(newPal[1:n])
colVec <- rgb(colRGB[1,]/255, colRGB[2,]/255, colRGB[3,]/255, alpha = 0.6)