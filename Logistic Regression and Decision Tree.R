

##################################################################################
#Logistic
##################################################################################

#use the adjusted database by the exploratory analysis and cluster codes

#select cluster 1
base <- data.frame(base, clusterk3$cluster)
head(base)
cluster1 <- subset(base, base$clusterk3.cluster==1)

#Transform the response variable into factor:
cluster1$default.payment.next.month <- as.factor(cluster1$default.payment.next.month)
table(cluster1$default.payment.next.month) #not balanced data

#by the bivariate analysis:
#1. married and others have the same risk
cluster1$MARRIAGE[cluster1$MARRIAGE=='others'] <- 'married + others'
cluster1$MARRIAGE[cluster1$MARRIAGE=='married'] <- 'married + others'

#2. Education
cluster1$graduate <- 0
cluster1$graduate[cluster1$EDUCATION=='graduate school'] <- 1
cluster1$graduate <- as.factor(cluster1$graduate)

#Balance dataset
Base1 <- subset(cluster1, cluster1$default.payment.next.month==1)
set.seed(123) 
Base0 <- subset(cluster1, cluster1$default.payment.next.month ==0) 
dt = sort(sample(nrow(Base0), nrow(Base1))) 
Sample_0 <- Base0[dt,] 
balanced = rbind(Base1, Sample_0)
table(balanced$default.payment.next.month) 


#Subset database into training and test
set.seed(123)
dt = sort(sample(nrow(balanced), nrow(balanced)*.7))
train<-balanced[dt,]
test<-balanced[-dt,]

model <- glm(default.payment.next.month ~
               LIMIT_BAL+ SEX + graduate  + MARRIAGE+
               AGE+BILL_AMT6+PAY_AMT1+GROWTH_BILL_AMT,
             family=binomial(link='logit'),data=train)
summary(model)

#Remove GROWTH_BILL_AMT
model <- glm(default.payment.next.month ~
               LIMIT_BAL+ SEX + graduate  + MARRIAGE+
               AGE+BILL_AMT6+PAY_AMT1,
             family=binomial(link='logit'),data=train)
summary(model)

#Remove graduate1
model <- glm(default.payment.next.month ~
               LIMIT_BAL+ SEX   + MARRIAGE+
               AGE+BILL_AMT6+PAY_AMT1,
             family=binomial(link='logit'),data=train)
summary(model)

#multicolinearity
library(HH)
vif(model) 

#Prediction and confusion matrix
train$predlogistic = predict(model,train, type = "response")

#cutpoint
library(cutpointr)
cut <- cutpointr(train, predlogistic, default.payment.next.month,
                   method = maximize_metric, metric = accuracy)
summary(cut) 

#minimize difference sens and spec
cut <- cutpointr(train, predlogistic, default.payment.next.month,
                   method = minimize_metric, metric = abs_d_sens_spec)
summary(cut) 

train$response <- as.factor(ifelse(train$predlogistic>0.5095, 1, 0))

library(caret)
library(e1071)
confusionMatrix(train$response,train$default.payment.next.month )
#Accuracy : 0.599     

#Test
test$predlogistic = predict(model,test, type = "response")
test$response <- as.factor(ifelse(test$predlogistic>0.5095, 1, 0))
confusionMatrix(test$response,test$default.payment.next.month )
#Accuracy : 0.5948           




##################################################################################
#Decision Tree
##################################################################################

library(rpart)
tree<- rpart(default.payment.next.month ~
                 LIMIT_BAL+ SEX + graduate  + MARRIAGE+
                 AGE+BILL_AMT6+PAY_AMT1+GROWTH_BILL_AMT
               , method="class",data=train) 

par(mfrow=c(1,1))
library(rpart.plot)
rpart.plot(tree, type = 4, extra = 105)

train$predtree = predict(tree, type="class",newdata=train)
confusionMatrix(train$predtree,train$default.payment.next.month )
#Accuracy : 0.6472    
