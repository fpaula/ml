setwd("/Users/fpaula/Projects/curso-fia")

if (!require("dplyr")){
  install.packages("dplyr", dependencies = TRUE)
  print("instalou dplyr")
}

library(dplyr)

dplyr::full_join()
a <- 1
b <- 2
rm(b)
WordWord <- 10

6 * 6
7/2
(4*(6+2))/4
x <- 4/0
pi
(sin(pi/2)+2)*3
options(digits = 10, max.print = 2000)
pi
1 < 1
!TRUE
xor(TRUE, FALSE)
TRUE == FALSE
1 == 2
minha.variavel <- 1
x = 1
y = 2
wordword <- 3

x <- 3
if (x == 1) {
  print("entrou no if")
} else if (x == 2){
  print("entrou no elseif")
} else {
  print("entrou no else")
}
for (i in 2:4){
  j <- i * 10
  print(j)
}

checklist <- c("laticinios", "frutas", "produtos de limpeza")
for(check in checklist){
  print(paste(check, "ok", sep = " - "))
}
i <- 1
while (i < 5) {
  print(i)
  i <- i + 1
}

proba <- 0.2
chances <- 0
while (runif(1) > proba ){
  chances <- chances + 1
}
print(chances)

hello_world <- function(var){
  return(paste("Hello", var, sep = " "))
}
print(hello_world("World"))

calculadora <- function(horas, valor = 150) {
  resultado <- horas * valor
  if (horas > 100) {
    resultado = resultado * 0.9
  }
  return(resultado)
}
print(paste("Total", calculadora(101, 1), sep = ": "))

# ESTRUTURA DE DADOS
# 1 dimencao: vetor atomico: lista
# 2 dimencoes: matriz
# 3 dimencoes:
vetor1 <- 1:3
print(vetor1)
print(2*vetor1)
vetor2 = c(1,2,3)
vetor3 = c(2) # cuidado ao somar listas com numero diferente de itens
print(vetor2 + vetor3)

seq(0, 10, by=2)
x <- c(5,4,9,6,8,9,23,9,9)
sort(x)
x[4] # seleciona o quarto elemento (no R o indice comeca no 1)
x[x ==9]
x[x %in% c(1,2,5, 23)]
matriz <- matrix(1:12, nrow = 3, ncol = 4)
matriz[3,4]

lista <- list(x = 1:5, y = c("a", "b"))
lista[2]
lista[[2]]
lista$x
lista$y

df = data.frame(titulo1 = 1:3, titulo2 = c("a", "b", "c"))
# variavel categorica: transformar uma categoria (string) em um numero
View(df)
df[[1]]
nrow(df)
ncol(df)
dim(df) # quantidade de linhas e colunas

# subset
x <- c(17,21,8,4,9,10,11)
x[x >= 10] <- 0
x

MyData <- read.csv(file="/Users/fpaula/Projects/curso-fia/teste.csv", header=TRUE, sep=",")
View(MyData)