system('defaults write org.R-project.R force.LANG en_US.UTF-8')


#install.packages("mlbench", dependencies = T)
#install.packages("car", dependencies = T)
#install.packages("dplyr", dependencies = T)
#install.packages("ggplot2", dependencies = T)
#install.packages("reshape2", dependencies = T)
#install.packages("caret", dependencies = T)

library(mlbench)
library(car)
library(dplyr)
library(ggplot2)
library(reshape2)
library(caret)

data("BostonHousing")
housing <- BostonHousing

#Exercicio 2

hist(housing$medv)
summary(housing$medv)

#ou

housing %>%
  ggplot(aes(x = medv)) +
  stat_density() +
  labs(x = "Valor m??dio ($1000)", 
       y = "Densidade", 
       title = "Densidade do pre??o m??dio das casas em Boston") +
  theme_minimal()


#Exercicio 3

scatterplotMatrix(~ crim + rm + age + rad + tax + lstat + medv,
                  data=housing, smooth=FALSE,
                  reg.line=FALSE, ellipse=FALSE,
                  diagonal="none", pch=16)


#ou

housing %>%
  select(c(crim, rm, age, rad, tax, lstat, medv)) %>%
  melt(id.vars = "medv") %>%
  ggplot(aes(x = value, y = medv, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Median House Price ($1000s)") +
  theme_minimal()



#Exercicio 4
library("caret")
set.seed(123)
id_trainamento <- createDataPartition(y = housing$medv, p = 0.75, list = FALSE)
trainamento <- housing[id_trainamento,]
teste <- housing[-id_trainamento,]


#Exercicio 5
modelo1 <- lm(medv ~ crim + rm + tax + lstat + chas + nox + dis + rad + ptratio, data = trainamento)
summary(modelo1)

#Exercicio 6
Rquadrado <- summary(modelo1)$r.squared
print(paste("O primeiro modelo linear possui R-Quadrado ajustado de  ", 
            round(Rquadrado, 3), sep = ""))

par(mfrow=c(2,2))
plot(modelo1)



#Exercicio 7
modelo2 <- lm(log(medv) ~ crim + rm + tax + lstat + chas + nox + dis + rad + ptratio, 
              data = trainamento)

summary(modelo2)

#Exercicio 8
Rquadrado2 <- summary(modelo2)$r.squared
print(paste("O segundo modelo linear possui R-Quadrado ajustado de ", 
            round(Rquadrado2, 3), sep = ""))
plot(modelo2)

mean(modelo2$residuals)


predicted <- predict(modelo2, newdata = teste)
results <- data.frame(predicted = exp(predicted), original = teste$medv)


#Exercicio 10
results %>%
  ggplot(aes(x = predicted, y = original)) +
  geom_point() +
  stat_smooth() +
  labs(x = "Valores Previstos", y = "Valores Observados", title = "Previsto vs. Observado") +
  theme_minimal()


