

##################################################################################
#Estudo de caso 2: Cartão de crédito
##################################################################################

# Listar a biblioteca
setwd("C:/Users/Natalia/Documents/FIA LabData/Estudos de caso")

#Importar a base de dados
library(readxl)
base <- read_excel("UCI Credit Card.xlsx")

valor <- median(base$AGE)
base$AGE[base$AGE<0] <- valor
base$AGE[base$AGE>900] <- valor

base$perc = base$BILL_AMT/base$LIMIT_BAL
base$perc[base$perc<0] <- 0

head(base) #mostrar as primeiras linhas

install.packages("factoextra")
install.packages("gridExtra")
library(cluster)    # Algoritmos de cluster
library(factoextra) #Visualização dos dados
library(gridExtra)

base <- base[,-1]
base <- base[,-2]
base <- base[,-2]
base <- base[,-2]
base <- base[,-3]
base <- base[,-3]
head(base)


# padronização dos dados
dados.padronizado <- scale(base[,1:ncol(base)])
head(dados.padronizado)
set.seed(5) # Ao mudar essa semente, o agrupamento muda


dados.k2 <- kmeans(dados.padronizado, centers = 2, nstart = 25 , iter.max = 100)


#Visualizar os clusters

fviz_cluster(dados.k2, data = dados.padronizado, main = "Cluster K2")


# número de observações em cada grupo

table(dados.k2$cluster)


#Agora rodar de 3 a 5 centros e visualizar qual a melhor divisão

dados.k3 <- kmeans(dados.padronizado, centers = 3, nstart = 25)

dados.k4 <- kmeans(dados.padronizado, centers = 4, nstart = 25)

dados.k5 <- kmeans(dados.padronizado, centers = 5, nstart = 25)

#Gráficos

G1 <- fviz_cluster(dados.k2, geom = "point", data = dados.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(dados.k3, geom = "point",  data = dados.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(dados.k4, geom = "point",  data = dados.padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(dados.k5, geom = "point",  data = dados.padronizado) + ggtitle("k = 5")

#Criar uma matriz com 4 gráficos
grid.arrange(G1, G2, G3, G4, nrow = 2)


#Agrupar cluster e base - foi considerado 3 grupos

dadosFinal<- data.frame(base, dados.k4$cluster)

head(dadosFinal)


# número de observações em cada grupo

table(dados.k4$cluster)

#analise exploratória de dados


par(mfrow=c(1,3)) #coloca os gráficos lado a lado

boxplot(dadosFinal$LIMIT_BAL  ~ as.factor(dadosFinal$dados.k3.cluster), col="blue",main="Limite")

boxplot(dadosFinal$AGE ~ as.factor(dadosFinal$dados.k3.cluster), col="blue",main="Idade")

boxplot(dadosFinal$perc ~ as.factor(dadosFinal$dados.k3.cluster), col="blue", main="% uso do limite")


#analise exploratória de dados

tapply(dadosFinal $ LIMIT_BAL, dadosFinal $ dados.k3.cluster,summary)
tapply(dadosFinal $ AGE, dadosFinal $ dados.k3.cluster,summary)
tapply(dadosFinal $ perc, dadosFinal $ dados.k3.cluster,summary)


# Exportar a base de dados

write.csv(dadosFinal, file="cluster.csv")

