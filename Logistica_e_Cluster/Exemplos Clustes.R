#install.packages("cluster", dependencies = T)
library(cluster)
library(dplyr)

#Definir o diretório
setwd("/Users/fpaula/Projects/curso-fia/Logistica_e_Cluster")

#Criar banco de dados
mcdonalds <- read.table("MCDONALDS.csv", sep = ";", header = T, dec = ",")
rownames(mcdonalds) <- mcdonalds[,1]
mcdonalds <- mcdonalds[,-1]


#padronizar os dados por sqrt(sum(x^2)/(n-1))
mcdonalds.padronizado <- scale(mcdonalds[,2:ncol(mcdonalds)])
head(mcdonalds.padronizado)


#calcular as distâncias da matriz utilizando a distância euclidiana
d <- dist(mcdonalds.padronizado, method = "euclidean")

#Calcular o Cluster: métodos disponíveis "average", "single", "complete" e "ward.D"
hc1 <- hclust(d, method = "ward.D" )

# Dendrograma
plot(hc1, cex = 0.6, hang = -1)


#vamos dividir o cluster em 4 grupos, tente variar a quantidade de grupos trocando o k = 4 #abaixo
sub_grp <- cutree(hc1, k = 4)
table(sub_grp)


#Criar o gráfico e destacar os grupos
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 4, border = 2:5)

#Agora altere o método do seu cluster para complete ou ward.D e compare as saídas dos #dendrogramas



######## POKEMON ##########
pokemon1 <- read.table("POKEMON1.csv", sep = ";", header = T, dec = ",")
rownames(pokemon1) <- pokemon1[,1]
pokemon1 <- pokemon1[,-1]

pokemon1.padronizado <- scale(pokemon1[,2:ncol(pokemon1)])
head(pokemon1.padronizado)
d1 <- dist(pokemon1.padronizado, method = "euclidean")
hc2 <- hclust(d1, method = "single" )
sub_grp <- cutree(hc2, k = 4)
table(sub_grp)
plot(hc2, cex = 0.6, hang = -1, main = "Cluster Pokemon 1")
rect.hclust(hc2, k = 4, border = 2:5)



#carregar base e preparar para o cluster
pokemon2 <- read.table("POKEMON2.csv", sep = ";", header = T, dec = ",")
rownames(pokemon2) <- pokemon2[,1]
pokemon2 <- pokemon2[,-1]



pokemon2.padronizado <- scale(pokemon2[,2:ncol(pokemon2)])
head(pokemon2.padronizado)
d2 <- dist(pokemon2.padronizado, method = "euclidean")
hc3 <- hclust(d2, method = "single" )
sub_grp <- cutree(hc3, k = 4)
table(sub_grp)
plot(hc3, cex = 0.6, hang = -1, main = "Cluster Pokemon 2")
rect.hclust(hc3, k = 4, border = 2:5)





#Cluster Kmeans
set.seed(5)


library(cluster)    # Algoritmos de cluster
library(factoextra) #Visualização dos dados
library(gridExtra)

#Rodar o modelo
Pokemon2.k2 <- kmeans(pokemon2.padronizado, centers = 2, nstart = 25 , iter.max = 100)


#Visualizar os clusters
fviz_cluster(Pokemon2.k2, data = pokemon2.padronizado, main = "Cluster K2")



#Agora vamos rodar de 3 a 5 centros  e visualizar qual a melhor divisão

Pokemon2.k3 <- kmeans(pokemon2.padronizado, centers = 3, nstart = 25)
Pokemon2.k4 <- kmeans(pokemon2.padronizado, centers = 4, nstart = 25)
Pokemon2.k5 <- kmeans(pokemon2.padronizado, centers = 5, nstart = 25)

#Gráficos
G1 <- fviz_cluster(Pokemon2.k2, geom = "point", data = pokemon2.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(Pokemon2.k3, geom = "point",  data = pokemon2.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(Pokemon2.k4, geom = "point",  data = pokemon2.padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(Pokemon2.k5, geom = "point",  data = pokemon2.padronizado) + ggtitle("k = 5")


#Criar uma matriz com 4 gráficos
grid.arrange(G1, G2, G3, G4, nrow = 2)

pokemon3 <- read.table("POKEMON2.csv", sep = ";", header = T, dec = ",")
pokemonfit <- data.frame(Pokemon2.k3$cluster, row.names = T)


#Agrupar cluster e base
PokemonFinal <-   cbind(pokemon3, pokemonfit)







#Resumo do modelo
mediagrupo <- PokemonFinal %>% 
  group_by(Pokemon2.k3.cluster) %>% 
  summarise(HP = mean(HP), 
            Attack = mean(Attack), 
            Defense = mean(Speed))
mediagrupo

std.dev <- PokemonFinal %>% 
  group_by(Pokemon2.k3.cluster) %>% 
  summarise(HP = sd(HP), 
            Attack = sd(Attack), 
            Defense = sd(Speed))

std.dev









################## MUNICIPIOS ##################
set.seed(5)

#install.packages("factoextra", dependencies = T)
#install.packages("gridExtra", dependencies = T)
library(cluster)    # Algoritmos de cluster
library(factoextra) #Visualização dos dados
library(gridExtra)

#carregar base municipio
municipios <- read.table("municipios.csv", sep = ";", header = T, dec = ",")
municipios <- na.omit(municipios)
rownames(municipios) <- municipios[,1]
municipios <- municipios[,-1]


#padronizar dados
municipios.padronizado <- scale(municipios[,2:ncol(municipios)])


#Agora vamos rodar de 3 a 5 centros  e visualizar qual a melhor divisão
municipios.k2 <- kmeans(municipios.padronizado, centers = 2, nstart = 25)
municipios.k3 <- kmeans(municipios.padronizado, centers = 3, nstart = 25)
municipios.k4 <- kmeans(municipios.padronizado, centers = 4, nstart = 25)
municipios.k5 <- kmeans(municipios.padronizado, centers = 5, nstart = 25)



#Gráficos
G1 <- fviz_cluster(municipios.k2, geom = "point", data = municipios.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(municipios.k3, geom = "point",  data = municipios.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(municipios.k4, geom = "point",  data = municipios.padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(municipios.k5, geom = "point",  data = municipios.padronizado) + ggtitle("k = 5")

#Criar uma matriz com 4 gráficos
grid.arrange(G1, G2, G3, G4, nrow = 2)

municipios2 <- read.table("municipios.csv", sep = ";", header = T, dec = ",")
municipiosfit <- data.frame(municipios.k4$cluster)

#Agrupar cluster e base
MunicipioFinal <-  cbind(municipios2, municipiosfit)
