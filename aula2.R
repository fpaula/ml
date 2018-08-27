setwd("/Users/fpaula/Projects/curso-fia")


# coisas relevantes
# regressao linear


# Coisas que devemos nos atentar:
# missing values
# outliers
# correlacao de Pearson
# relacao bivariada e multivariada
# LIB data.table pra importar csv muito grande.
#                fread("file.csv")
# Pesquisar sobre Boxplot
# Regressao linear (importante)
# Feature engineering
# Selecionar o algoritimo (tem varios, vai depender do caso)
# Criar graficos eh bacana

# Y = f(x) + E
    # Y: Define a variavel de resposta
    # f(x): Define a funcao que depende do conjunto de recursos de entrada
    # E: erro, define o erro aleatorio. Para o modelo ideal, deve ser aleat??rio e nao deve depender de nenhuma entrada

if (!require("data.table")){
  install.packages("data.table")
  print("data.table")
}
library(data.table)

if (!require("data.table")){
  install.packages("rgl")
  print("rgl")
}
library(rgl)

dados <- read.csv(file="/Users/fpaula/Projects/curso-fia/base_gastos_cartao.csv", header=TRUE, sep=",")
# dados <- fread(file="/Users/fpaula/Projects/curso-fia/base_gastos_cartao.csv")
head(dados)
# View(dados)
summary(dados)
table(dados$Segmento)
quantile(dados$Gastos_Cartao)
quantile(dados$Idade)
quantile(dados$Renda)
quantile(dados$Impostos)

vars <- names(dados)[1:4]
for(i in vars){
  par(mfrow=c(2,1))
  hist(dados[,i], breaks = 20, main=paste0("Histograma - ", i),
       xlab=i, ylab="Frequencia", col="dark blue")
  boxplot(dados[,i], main=paste0("Boxplot - ", i),
          ylab=i, col="dark red")
}

vars <- names(dados)
for(i in vars){
  cat(paste0(i, " - numero de observacoes missing: ",
             sum(is.na(dados[,i]))),"\n")
}


dados[,vars]
cor(dados[,vars])

library(car)
scatterplotMatrix(~ Gastos_Cartao + Idade + Renda + Impostos, 
                  data=dados, 
                  smooth=FALSE,
                  reg.line=FALSE, ellipse=FALSE,
                  diagonal="none", pch=16)

scatterplotMatrix(~ Gastos_Cartao + Idade + Renda +
                    Impostos, data=dados, smooth=FALSE,
                  reg.line=FALSE, ellipse=FALSE,
                  groups=as.factor(dados$Segmento), 
                  diagonal="none")

plot(dados$Idade, dados$Gastos_Cartao, pch=16, main="Grafico de dispersao",
     xlab = "Idade", ylab="Gastos Cartao", ylim=c(200,1000))

for(i in names(dados)[1:4]){
  boxplot(dados[,i] ~ as.factor(dados$Segmento), main=paste0("Boxplot - ",i),
          xlab="Segmento", ylab=i, col="dark red")
}

plot3d(dados$Gastos_Cartao, dados$Renda, dados$Idade, col="red", size=5)


vendedor <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
tempo.experiencia <- c(1, 3, 4, 4, 6, 8, 10, 10, 11, 13)
vendas.anuais <- c(80, 97, 92, 102, 103, 111, 119, 123, 117, 136)
base <- data.frame(vendedor,tempo.experiencia,vendas.anuais)

# Aplicar a analise de regressao linear simples no R - criar
regressao <- lm(data = base,vendas.anuais~tempo.experiencia)

# Estatistica do modelo
summary(regressao)

# devemos ter uma relacao linear entre x e y.
# homocedasticidade
# independencia
# p-valor (probabilidade de significancia) é importante
# r-squared mostra de o modelo tá bom
# olhar varios resultados para verificar se o modelo é bom

# o modelo no final das contas é uma formula que faz sentido naquele contexto.
# algumas variaveis podem prejudicar o modelo se elas não forem relevantes.
# sempre que adicionar uma variavel precisamos garantir que ela é relevante. Tem ferramentas que fazem essa analise.
# ferramentas: Backward ou stepwise.
# https://www.comet.ml/ otimizaçao

# 

