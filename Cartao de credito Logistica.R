
##################################################################################
#Estudo de caso 1: Cartão de crédito
##################################################################################

# Listar a biblioteca
setwd("C:/Users/Natalia/Documents/FIA LabData/Estudos de caso")

rm()

#Importar a base de dados
library(readxl)
base <- read_excel("UCI Credit Card.xlsx")

head(base) #mostrar as primeiras linhas

##################################################################################
#Análise exploratória univariada
##################################################################################

options(scipen = 999)
summary(base$LIMIT_BAL) #Estatísticas descritivas
hist(base$LIMIT_BAL) #Histograma
boxplot(base$LIMIT_BAL) #bloxplot

sex<-table(base$SEX) #calculando as frequencias
prop.table(sex) #os percentuais
pie(table(base$SEX),main="Sexo") 
#SEX: Gender (1=male, 2=female)
base$SEX <- as.character(base$SEX)
base$SEX[base$SEX == "1"] <- "M"
base$SEX[base$SEX == "2"] <- "F"
table(base$SEX) #calculando as frequencias

table(base$EDUCATION) #calculando as frequencias
educ<-table(base$EDUCATION,useNA="always") #calculando as frequencias com miss
educ
prop.table(educ) #os percentuais
prop.table(educ)*100 #os percentuais

cruz<-table(base$EDUCATION,base$default.payment.next.month)
prop.table(cruz,1)

table(base$MARRIAGE,useNA="always") #calculando as frequencias
prop.table(table(base$MARRIAGE)) #os percentuais
#MARRIAGE: Marital status (1=married, 2=single, 3=others)
filtro<-subset(base, base$MARRIAGE=="0")
filtro
base$MARRIAGE <- as.character(base$MARRIAGE)
base$MARRIAGE[base$MARRIAGE == "1"] <- "married and others"
base$MARRIAGE[base$MARRIAGE == "0"] <- "married and others"
base$MARRIAGE[base$MARRIAGE == "2"] <- "single"
base$MARRIAGE[base$MARRIAGE == "3"] <- "married and others"

table(base$MARRIAGE,useNA="always")



summary(base$AGE) #Estatísticas descritivas
hist(base$AGE) #Histograma
quantile(base$AGE, c(.01, .05))
quantile(base$AGE, c(.90, .95))

filtro<-subset(base, base$AGE<0)
filtro
valor <- median(base$AGE)
base$AGE[base$AGE<0] <- valor
base$AGE[base$AGE>900] <- valor
summary(base$AGE)
boxplot(base$AGE) #bloxplot



summary(base$BILL_AMT) #Estatísticas descritivas
hist(base$BILL_AMT) #Histograma
boxplot(base$BILL_AMT) #bloxplot
quantile(base$BILL_AMT, c(.01, .05))


table(base$default.payment.next.month) #calculando as frequencias
prop.table(table(base$default.payment.next.month)) #os percentuais

base$perc = base$BILL_AMT/base$LIMIT_BAL
summary(base$perc) #Estatísticas descritivas
quantile(base$perc, c(.90, .95,.99))

hist(base$perc) #Histograma
boxplot(base$perc) #bloxplot
base$perc[base$perc<0] <- 0


##################################################################################
#Análise exploratória bivariada
##################################################################################

install.packages("psych")
library(psych)
describeBy(base$LIMIT_BAL, base$default.payment.next.month) #Descritivas de Cliques por Site
boxplot(base$LIMIT_BAL~ base$default.payment.next.month)



table(base$SEX,base$default.payment.next.month)
prop.table(table(base$SEX,base$default.payment.next.month) ) # cell percentages
prop.table(table(base$SEX,base$default.payment.next.month) , 1) # row percentages 

table(base$MARRIAGE,base$default.payment.next.month)
prop.table(table(base$MARRIAGE,base$default.payment.next.month) ) # cell percentages
prop.table(table(base$MARRIAGE,base$default.payment.next.month) , 1) # row percentages 

describeBy(base$AGE , base$default.payment.next.month) #Descritivas de Cliques por Site
boxplot(base$AGE ~ base$default.payment.next.month)

describeBy(base$BILL_AMT, base$default.payment.next.month) #Descritivas de Cliques por Site

describeBy(base$perc , base$default.payment.next.month) #Descritivas de Cliques por Site
boxplot(base$perc ~ base$default.payment.next.month)


##################################################################################
#Regressão Logística 
##################################################################################

nrow(base)

base$default.payment.next.month <- as.factor(base$default.payment.next.month)

train <- base[1:21000,]
test <- base[21001:30000,]

#dt = sort(sample(nrow(mydata), nrow(mydata)*.7))
#train<-mydata[dt,]
#val<-mydata[-dt,] 

model <- glm(default.payment.next.month ~LIMIT_BAL+ SEX + MARRIAGE  + perc,
             family=binomial(link='logit'),data=train)
summary(model)

#Prediction
pred = predict(model,train, type = "response")
finaldata = cbind(train, pred)

describeBy(finaldata$pred , finaldata$default.payment.next.month) #Descritivas da probb por default
boxplot(finaldata$pred ~ finaldata$default.payment.next.month)


finaldata$response <- as.factor(ifelse(finaldata$pred>0.3, 1, 0))


#CONFUSION MATRIX
#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)
confusionMatrix(finaldata$response,finaldata$default.payment.next.month )


#Prediction
pred = predict(model,test, type = "response")
finaldata = cbind(test, pred)
finaldata$response <- as.factor(ifelse(finaldata$pred>0.3, 1, 0))


#CONFUSION MATRIX
confusionMatrix(finaldata$response,finaldata$default.payment.next.month )

