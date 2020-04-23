
##################################################################################
#Credit Card
##################################################################################

# Library
setwd("C:\\Users\\Natalia\\Documents\\Github\\default-of-credit-card-clients-dataset")

#Import database
base <- read.csv("UCI_Credit_Card.csv",sep=",",dec=".")
names(base) #list variables

##################################################################################
#Univariate exploratory analysis
##################################################################################

options(scipen = 999) #remove scientific notation

#Transform sex, education, marriage in char variables
base$SEX <- as.character(base$SEX)
base$EDUCATION <- as.character(base$EDUCATION)
base$MARRIAGE <- as.character(base$MARRIAGE)

#Transform sex, education, marriage in factor variables
base$default.payment.next.month <- as.factor(base$default.payment.next.month)

#Quantitative variables

#select quantitative variables
nums <- unlist(lapply(base, is.numeric))
quanti <- base[ , nums]
#descriptive statistics - quanti variables
summary(quanti)

#Qualitative Variables
char <- unlist(lapply(base, is.character)) 
quali <- base[ , char]
names<-names(quali) #saves the variables names in a vector
#tables
for (i in names) {
  print(i)
  print(table(quali[,i],useNA="always"))
  print(prop.table(table(quali[,i],useNA="always")))
}

#no missing values

#Response variable
library(expss)
cro(base$default.payment.next.month)
cro_cpct(base$default.payment.next.month)

#Transform variables to better understanding of database
#SEX: Gender (1=male, 2=female). 
base$SEX[base$SEX == "1"] <- "M"
base$SEX[base$SEX == "2"] <- "F"
table(base$SEX,useNA = "always") 

#MARRIAGE: Marital status (1=married, 2=single, 3=others)
base$MARRIAGE[base$MARRIAGE == "1"] <- "married"
base$MARRIAGE[base$MARRIAGE == "2"] <- "single"
base$MARRIAGE[base$MARRIAGE == "3"] <- "others"

table(base$MARRIAGE,useNA="always")

#Put 0 as others also
base$MARRIAGE[base$MARRIAGE == "0"] <- "others"
table(base$MARRIAGE,useNA="always")


#EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)
base$EDUCATION[base$EDUCATION == "1"] <- "graduate school"
base$EDUCATION[base$EDUCATION == "2"] <- "university school"
base$EDUCATION[base$EDUCATION == "3"] <- "high school"
base$EDUCATION[base$EDUCATION == "4"] <- "others"
base$EDUCATION[base$EDUCATION == "5"] <- "unknown"
base$EDUCATION[base$EDUCATION == "6"] <- "unknown"

table(base$EDUCATION,useNA="always")

#Put 0 as unknown also
base$EDUCATION[base$EDUCATION == "0"] <- "unknown"
table(base$EDUCATION,useNA="always")

##################################################################################
#Bivariate analysis
##################################################################################

#Information value
library(Information)
base$default.payment.next.month <- as.numeric(base$default.payment.next.month)

#Response variable as 0 or 1
base$default.payment.next.month[base$default.payment.next.month == 1] <- 0
base$default.payment.next.month[base$default.payment.next.month == 2] <- 1

IV<-create_infotables(data = base, y = "default.payment.next.month", ncore = 2)
IV$Summary

#Does PAY_0 makes sense to the model? We assume that it does

#Of the PAY variables, PAY_0 has the higher IV
library(dplyr)
base <- select (base,-c(PAY_2,PAY_3,PAY_4,PAY_6,PAY_5))

#Of the PAY_AMT variables, PAY_AMT1 has the higher IV
base <- select (base,-c(PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT6,PAY_AMT5))

#Of the BILL_AMT variables, BILL_AMT6 has the higher IV
base$GROWTH_BILL_AMT <- (base$BILL_AMT1 - base$BILL_AMT6)/base$BILL_AMT6
base <- select (base,-c(BILL_AMT5,BILL_AMT4,BILL_AMT2,BILL_AMT1,BILL_AMT3))

#Recalculate IV
IV<-create_infotables(data = base, y = "default.payment.next.month", ncore = 2)
IV$Summary

#install.packages("psych")
library(psych)

#Quantitative variables
boxplot(base$PAY_0~ base$default.payment.next.month, main="PAY_0")
#The default = 1 has higher pay

boxplot(base$LIMIT_BAL~ base$default.payment.next.month)
#The default = 1 has lower limits - makes sense

boxplot(base$PAY_AMT1~ base$default.payment.next.month)
boxplot(base$GROWTH_BILL_AMT~ base$default.payment.next.month)
boxplot(base$BILL_AMT6~ base$default.payment.next.month)
#Too many outliers

boxplot(base$AGE~ base$default.payment.next.month)
#Not many diferences


#Qualitative variables
cro_rpct(base$SEX,base$default.payment.next.month)
#men are more likely to default

cro_rpct(base$MARRIAGE,base$default.payment.next.month)
#married are more likely to default

cro_rpct(base$EDUCATION,base$default.payment.next.month)
#high school and university school are more likely to default

#It is important not to focus on categories with low cases

