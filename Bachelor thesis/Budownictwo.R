library(dplyr)
library(tidyverse)
library(Hmisc)
library(MASS)
library(klaR)
library(psych)
library(caret)
library(car)
library(corrplot)
library(ggpubr)
library(moments)
library(rcompanion)
theme_set(theme_classic())





rm(list=ls())

data_cons <- read.csv(file="Dane_lic(nb_b_construction_16_year_15).csv", header=TRUE, sep=";")

data_cons1<-subset(data_cons,select=c(Type,V1,V8,V16,V20))



###Obserwacje odstaj¹ce

OutVals = boxplot(data_cons1)$out              

data_cons1<-data_cons1[-27,]
OutVals = boxplot(data_cons1)$out 


data_cons1<-data_cons1[-28,]
OutVals = boxplot(data_cons1)$out

data_cons1<-data_cons1[-25,]
OutVals = boxplot(data_cons1)$out


data_cons1<-data_cons1[-23,]
OutVals = boxplot(data_cons1)$out



data_cons1<-data_cons1[-14,]
OutVals = boxplot(data_cons1)$out

data_cons1<-data_cons1[-19,]
OutVals = boxplot(data_cons1)$out

data_cons1<-data_cons1[-17,]
OutVals = boxplot(data_cons1)$out


data_cons1<-data_cons1[-1,]
OutVals = boxplot(data_cons1)$out


data_cons1<-data_cons1[-22,]
OutVals = boxplot(data_cons1)$out


data_cons1<-data_cons1[-1,]
OutVals = boxplot(data_cons1)$out




data_cons1<-data_cons1[-11,]
OutVals = boxplot(data_cons1)$out

data_cons1<-data_cons1[-12,]
OutVals = boxplot(data_cons1)$out



data_cons1<-data_cons1[-9,]
OutVals = boxplot(data_cons1)$out


data_cons1<-data_cons1[-3,]
OutVals = boxplot(data_cons1)$out


data_cons1<-data_cons1[-17,]
OutVals = boxplot(data_cons1)$out

data_cons1<-data_cons1[-7,]
OutVals = boxplot(data_cons1)$out


###to_normality



T_tuk =
  transformTukey(data_cons1$V1,
                 plotit=FALSE)

T_tuk =
  transformTukey(data_cons1$V8,
                 plotit=FALSE)
data_cons1$V8 <- (data_cons1$V8)^0.1

T_tuk =
  transformTukey(data_cons1$V8,
                 plotit=FALSE)
data_cons1$V8 <- (data_cons1$V8)^0.8


T_tuk =
  transformTukey(data_cons1$V16,
                 plotit=FALSE)


T_tuk =
  transformTukey(data_cons1$V20,
                 plotit=FALSE)
data_cons1$V20 <- (data_cons1$V20)^0.8

##standaryzacja

data_cons1$V20 <- scale(data_cons1$V20)
data_cons1$V16 <- scale(data_cons1$V16)
data_cons1$V8 <- scale(data_cons1$V8)
data_cons1$V1 <- scale(data_cons1$V1)

data_cons_2 <-data_cons1[,2:5]

round(psych::describe(data_cons_2), 2)


TESTOWE <- read.csv(file="MODEL_TEST2.csv", header=TRUE, sep=";")  
model_test<-lm(V0~V1+V8+V16+V20,data = TESTOWE)     
vif(model_test)


model <- lda(Type~V1+V8+V16+V20, data = data_cons1)
model
plot(model)



