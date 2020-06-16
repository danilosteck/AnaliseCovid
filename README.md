# Análise da COVID no Brasil

Esta análise vai o método apresentado no artigo publicado no [Medium](https://bit.ly/COVID_Analise), com algumas análises complementares. Seguem aqui as perguntas que esta comparação deve responder:
- Como tem crescido o contágio e os óbitos no Brasil e como nos comparamos em relação a outros países?
- Temos sido mais ou menos efetivos para conter o contágio e os óbitos?
- Como está nossa taxa de letalidade em relação a outros países?

Link para o artigo final publicado no Medium: [Análise do crescimento COVID-19 no Brasil (parte 2: Brasil versus outros países)](https://medium.com/@danilosteck/an%C3%A1lise-do-crescimento-covid-19-no-brasil-parte-2-brasil-versus-outros-pa%C3%ADses-155a2abf0ef0).

# Arquivos-biblioteca:
Os arquivos [covidFunctionsLib.R](https://github.com/danilosteck/AnaliseCovid/blob/master/covidFunctionsLib.R) e [BRZcovidFunctionsLib.R](https://github.com/danilosteck/AnaliseCovid/blob/master/BRZcovidFunctionsLib.R) são bibliotecas que foram criadas para as análises, e não geram nenhum resultado se são compiladas. No entanto, elas são carregadas quando os arquivos de análise são compilados, pois contêm várias funções do tipo *user defined* especificamente para esta finalidade.

# Arquivos-análise:
O arquivo [COVID_Country_Comparison_v2.R](https://github.com/danilosteck/AnaliseCovid/blob/master/COVID_Country_Comparison_v2.R) realiza as análises para gerar os gráficos para a comparação **entre países**. Para gerar um HTML com o resultado, criei o arquivo Rmarkdown [COVID_Country_Analysis.Rmd](https://github.com/danilosteck/AnaliseCovid/blob/master/COVID_Country_Analysis.Rmd). 

Similarmente, o arquivo [COVID_BR_Analysis.R](https://github.com/danilosteck/AnaliseCovid/blob/master/COVID_BR_Analysis.R) realiza as análises para gerar os gráficos para a comparação **entre UFs e municípios no Brasil**. O arquivo Rmd [BRZ_COVID_Analysis.Rmd](https://github.com/danilosteck/AnaliseCovid/blob/master/BRZ_COVID_Analysis.Rmd) gera um HTML com o resultado.

# Arquivos-resultado:
Os arquivos [COVID_Country_Analysis.html](https://github.com/danilosteck/AnaliseCovid/blob/master/COVID_Country_Analysis.html) e [BRZ_AnaliseCOVID_porEstado_porMunicipio_16062020.html](https://github.com/danilosteck/AnaliseCovid/blob/master/BRZ_AnaliseCOVID_porEstado_porMunicipio_16062020.html) apresentam, respectivamente, os resultados para as análises entre países, e entre UFs e municípios no Brasil.
