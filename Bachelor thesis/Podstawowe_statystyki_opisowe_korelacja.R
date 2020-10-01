install.packages("klaR")
install.packages("devtools")
install.packages("mda")
install.packages("corrplot")
install.packages('car')
install.packages("Hmisc")
library(tidyverse)
library("Hmisc")
library(MASS)
library(klaR)
library(psych)
library(caret)
library(car)
library(corrplot)
theme_set(theme_classic())



rm(list=ls())

data_manu <- read.csv(file="Dane_lic(nb_b_manufacture_16_year_15).csv", header=TRUE, sep=";")
data_agri <- read.csv(file="Dane_lic(nb_b_agriculture_16_year_13).csv", header=TRUE, sep=";")
data_cons <- read.csv(file="Dane_lic(nb_b_construction_16_year_15).csv", header=TRUE, sep=";")

summary(data_manu)
summary(data_agri)
summary(data_cons)

data_manu1 <- data_manu[,2:22]
data_agri1 <- data_agri[,2:22]
data_cons1 <- data_cons[,2:22]

round(psych::describe(data_manu1), 2)
round(psych::describe(data_agri1), 2)
round(psych::describe(data_cons1), 2)

# data_manu1 <- data_manu[,2:22]
# data_agri1 <- data_agri[,2:22]
# data_cons1 <- data_cons[,2:22]


forcorrplot1<-cor(data_manu1)
forcorrplot2<-cor(data_agri1)
forcorrplot3<-cor(data_cons1)

corrplot.mixed(forcorrplot1,upper="number",lower="color",order="hclust")
corrplot.mixed(forcorrplot2,upper="number",lower="color",order="hclust")
corrplot.mixed(forcorrplot3,upper="number",lower="color",order="hclust")