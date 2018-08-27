#Regressao Linear

#Exemplo Linear Simples

#criar base
vendedor <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
tempo.experiencia <- c(1, 3, 4, 4, 6, 8, 10, 10, 11, 13)
vendas.anuais <- c(80, 97, 92, 102, 103, 111, 119, 123, 117, 136)
base <- data.frame(vendedor,tempo.experiencia,vendas.anuais)


#Aplicar a analise de regressao linear simples no R - criar
regressao <- lm(data = base,vendas.anuais~tempo.experiencia)

#Estatistica do modelo
summary(regressao)

##-------------------------------------------------------------------##

#Exemplo slide 69

#apontar para a pasta onde está o arquivo ex. "C:/Users/BIG DATA/Desktop/"
#atentar para a barra invertida

setwd("c:/Users/ander/Desktop/")

#carregar base
vendas <- read.csv("vendas.csv", sep = ";", dec = ",")

#Executar o modelo
regressao.mult <- lm(data = vendas, "Faturamento ~ AnuncioTelevisao + AnuncioJornal")

#Estatisticas do modelo
summary(regressao.mult)
