

##################################################################################
#Cluster
##################################################################################

#use the adjusted database by the exploratory analysis code

library(cluster)    
library(factoextra) 
library(gridExtra)

#Use only the columns that make sense
#parsimony for interpretation
names(base)
cluster <- base[,c("LIMIT_BAL","AGE","GROWTH_BILL_AMT")]

#There are outliers in GROWTH_BILL_AMT
quantile(cluster$GROWTH_BILL_AMT, c(.01, .05,.1,.9, .95,.99)) 

#put outliers in the 5th and 95th percentile
p5 <- quantile(cluster$GROWTH_BILL_AMT,c(.05))
cluster$GROWTH_BILL_AMT[cluster$GROWTH_BILL_AMT<p5] <- p5
p95 <- quantile(cluster$GROWTH_BILL_AMT,c(.95))
cluster$GROWTH_BILL_AMT[cluster$GROWTH_BILL_AMT>p95] <- p95

summary(cluster$GROWTH_BILL_AMT)
boxplot(cluster$GROWTH_BILL_AMT)

#there are still outliers, but they are not absurd values

# standardization of data, so that all variables have the same weight
std <- scale(cluster[,1:ncol(cluster)])
head(std)

set.seed(5) # Ao mudar essa semente, o agrupamento muda

#try 2, 3, 4 and 5 clusters
clusterk2 <- kmeans(std, centers = 2, nstart = 25 , iter.max = 100)
clusterk3 <- kmeans(std, centers = 3, nstart = 25, iter.max = 100)
clusterk4 <- kmeans(std, centers = 4, nstart = 25, iter.max = 100)
clusterk5 <- kmeans(std, centers = 5, nstart = 25, iter.max = 100)

G1 <- fviz_cluster(clusterk2, geom = "point", data = std) + ggtitle("k = 2")
G2 <- fviz_cluster(clusterk3, geom = "point",  data = std) + ggtitle("k = 3")
G3 <- fviz_cluster(clusterk4, geom = "point",  data = std) + ggtitle("k = 4")
G4 <- fviz_cluster(clusterk5, geom = "point",  data = std) + ggtitle("k = 5")

grid.arrange(G1, G2, G3, G4, nrow = 2)

#3 groups is the most suitable

cluster <- data.frame(cluster, clusterk3$cluster)
head(cluster)


# number of observations in each group
table(cluster$cluster)

#describing
par(mfrow=c(1,3)) #coloca os gráficos lado a lado
boxplot(cluster$LIMIT_BAL  ~ as.factor(cluster$clusterk3.cluster), col="blue",main="LIMIT_BAL")
boxplot(cluster$AGE ~ as.factor(cluster$clusterk3.cluster), col="blue",main="AGE")
boxplot(cluster$GROWTH_BILL_AMT ~ as.factor(cluster$clusterk3.cluster), col="blue", main="GROWTH_BILL_AMT")

#the first group contains people with higher limits and older
#the second group contains people with the highest growth in bill amount
#the third group contains people with lower limits and younger
