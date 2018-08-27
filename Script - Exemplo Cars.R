cars <- cars

head(cars) #Mostras as 6 primeiras observacoes

scatter.smooth(x=cars$speed, 
               y=cars$dist, 
               main="Dist ~ Vel")  # Grafico de Dispersao

par(mfrow=c(1, 2))  # Dividir a area de grafico em 2
boxplot(cars$speed, main="Velocidade", sub=paste("Outliers: ", boxplot.stats(cars$speed)$out))  # box plot para a variavel 'speed'
boxplot(cars$dist, main="Distancia", sub=paste("Outliers: ", boxplot.stats(cars$dist)$out))  # box plot para 'distancia'


library(e1071)
par(mfrow=c(1, 2))  # Dividir a area de grafico em 2
plot(density(cars$speed), 
     main="Grafico de densidade: Velocidade", 
     ylab="Frequencia", 
     sub=paste("Skewness:", 
               round(e1071::skewness(cars$speed), 2)))  # grafico de densidade para 'speed'
polygon(density(cars$speed), col="blue")
plot(density(cars$dist), main="Grafico de densindade: Distancia", ylab="Frequencia", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")


cor(cars$speed, cars$dist)  # Calcular a correlaco entre velocidade e distancia


linearMod <- lm(dist ~ speed, data=cars)  # modelo de regressao linear
print(linearMod)


summary(linearMod) #resumo do modelo
