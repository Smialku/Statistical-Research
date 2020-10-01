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

data_manu <- read.csv(file="Dane_lic(nb_b_manufacture_16_year_15).csv", header=TRUE, sep=";")

data_manu1<-subset(data_manu,select=c(Type,V1,V8,V16,V20))


OutVals = boxplot(data_manu1)$out              


data_manu1<-data_manu1[-28,]
OutVals = boxplot(data_manu1)$out 


data_manu1<-data_manu1[-28,]
OutVals = boxplot(data_manu1)$out 

data_manu1<-data_manu1[-27,]
OutVals = boxplot(data_manu1)$out 


data_manu1<-data_manu1[-20,]
OutVals = boxplot(data_manu1)$out 



data_manu1<-data_manu1[-16,]
OutVals = boxplot(data_manu1)$out 

data_manu1<-data_manu1[-18,]
OutVals = boxplot(data_manu1)$out 


data_manu1<-data_manu1[-16,]
OutVals = boxplot(data_manu1)$out 


data_manu1<-data_manu1[-19,]
OutVals = boxplot(data_manu1)$out 

data_manu1<-data_manu1[-4,]
OutVals = boxplot(data_manu1)$out 

data_manu1<-data_manu1[-20,]
OutVals = boxplot(data_manu1)$out 

data_manu1<-data_manu1[-19,]
OutVals = boxplot(data_manu1)$out 


data_manu1<-data_manu1[-17,]
OutVals = boxplot(data_manu1)$out 

data_manu1<-data_manu1[-12,]
OutVals = boxplot(data_manu1)$out 


data_manu1<-data_manu1[-16,]
OutVals = boxplot(data_manu1)$out 

data_manu1<-data_manu1[-1,]
OutVals = boxplot(data_manu1)$out 


T_tuk =
  transformTukey(data_manu1$V1,
                 plotit=FALSE)

T_tuk =
  transformTukey(data_manu1$V8,
                 plotit=FALSE)
data_manu1$V8 <- (data_manu1$V8)^0.85

T_tuk =
  transformTukey(data_manu1$V8,
                 plotit=FALSE)


T_tuk =
  transformTukey(data_manu1$V16,
                 plotit=FALSE)


T_tuk =
  transformTukey(data_manu1$V20,
                 plotit=FALSE)
data_manu1$V20 <- (data_manu1$V20)^0.65


##standaryzacja

data_manu1$V20 <- scale(data_manu1$V20)
data_manu1$V16 <- scale(data_manu1$V16)
data_manu1$V8 <- scale(data_manu1$V8)
data_manu1$V1 <- scale(data_manu1$V1)



TESTOWE <- read.csv(file="MODEL_TEST3.csv", header=TRUE, sep=";")  
model_test<-lm(V0~V1+V8+V16+V20,data = TESTOWE)     
vif(model_test)



model <- lda(Type~V1+V8+V16+V20, data = data_manu1)
model
plot(model)
