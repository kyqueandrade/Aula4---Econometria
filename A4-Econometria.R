#Econometria Avançada Aula 4

install.packages("urca") #instala o pacote urca
library("urca") #carrega o pacote urca
install.packages("readxl") #instala o pacote readxl
library(readxl) #carrega o pacote readxl
interdaay <- read_excel("G:/USJT/Econometria/A4/interdaay.xls", col_types = c("date", "numeric", "numeric", "numeric")) #carrega a tabela de excel interdaay salva no computador
View(interdaay) #vizualiza a tabela interdaay
interdaay <- interdaay[,-1] #retira a primeira coluna da tabela interdaay
colnames(interdaay)[2] <- "Variação" #altera o nome da segunda coluna
write.csv(interdaay,file = "Ibovespa.csv")

# Séries Temporais

dados_diarios <- ts(interdaay, start = 2017-01-10, frequency = 365) #cria uma série temporal com os dados da tabela que se inicia em 2017-01-10 com frequência dária
Variação <- ts(interdaay$Variação, start = 2017-01-10, frequency = 365) #cria uma série temporal Variação
Ibovespa <- ts(interdaay$Ibovespa, start = 2017-01-10, frequency = 365) #cria série temporal Ibovespa
Quantidade <- ts(interdaay$Quantidade, start = 2017-01-10, frequency = 365) #cria uma série temporal Quantidade
plot(dados_diarios, col= "blue", main="Dados do Indice Bovespa", xlab="Dias") #cria um gráfico dos dados diários
plot(Variação, main="Percentual de Variação") #cria um gráfico da Variação com título Percentual de Variação
plot(Ibovespa, main="Índice do Dia",col="red") #cria um gráfico do Ibovespa com título Índice do Dia e cor vermelha
plot(Quantidade, main="Índice do Dia", xlab="Dias", col="blue") #cria um gráfico da Quantidade com título Índice do dia descrição do eixo x como Dias e cor azul

# Teste de Dick-Fuller

TesteDF_Variação_none <- ur.df(Variação, "none",lags = 0) #teste 1 DF-DickFuller sem drift e sem tendência
TesteDF_Variação_none #não contém o valor crítico do teste
summary(TesteDF_Variação_none) #resumo estatístico do teste contém o valor crítico com 1% 5% e 10% de significância

TesteDF_Variação_drift <- ur.df(Variação, "drift", lags=0) #teste 2 com drift
TesteDF_Variação_drift #não contém o valor crítico do teste
summary(TesteDF_Variação_drift) #resumo estatístico do teste contém o valor crítico com 1% 5% e 10% de significância

TesteDF_Variação_trend <- ur.df(Variação, "trend", lags = 0) #teste 3 com tendência e com drift
TesteDF_Variação_trend #não contém o valor crítico do teste
summary(TesteDF_Variação_trend) #resumo estatístico do teste contém o valor crítico com 1% 5% e 10% de significância

TesteDF_Ibovespa_none <- ur.df(Ibovespa, "none",lags = 0) #teste 1 DF-DickFuller sem drift e sem tendência
TesteDF_Ibovespa_none #não contém o valor crítico do teste
summary(TesteDF_Ibovespa_none) #resumo estatístico do teste contém o valor crítico com 1% 5% e 10% de significância

TesteDF_Ibovespa_drift <- ur.df(Ibovespa, "drift", lags=0) #teste 2 com drift
TesteDF_Ibovespa_drift #não contém o valor crítico do teste
summary(TesteDF_Ibovespa_drift) #resumo estatístico do teste contém o valor crítico com 1% 5% e 10% de significância

TesteDF_Ibovespa_trend <- ur.df(Ibovespa, "trend", lags = 0) #teste 3 com tendência e com drift
TesteDF_Ibovespa_trend #não contém o valor crítico do teste
summary(TesteDF_Ibovespa_trend) #resumo estatístico do teste contém o valor crítico com 1% 5% e 10% de significância
