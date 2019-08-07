#****************************************************************************************
#
# PROJECT: 20180601
#
# MODULE: 030 - ANALYSE - DATA DISCOVERY
#
# DESCRIPTION: Graph/Tables for top predictors to derive insights

#
#              
#         
# STEPS
# 1.Set libraries and import data
# 2. Bar plots of top variables from GBM & DRF     
#         
#
#****************************************************************************************

## 1. Set libraries and import data ####

library(lattice)
library(dplyr)
library(ggplot2)
library(gmodels)

tmp_train<-readRDS("./train.rds")


## 2. Bar plots of top variables from GBM & DRF ####
colnames(tmp_train)[1] <- "companylocation"

tmp_train$companylocation<-as.character(tmp_train$companylocation)

str1<-strsplit(tmp_train$companylocation,",")

tmp_train$country<-as.factor(sapply(strsplit(tmp_train$companylocation,","), `[`, 1))


location_analysis<-tmp_train %>%
  group_by (country,fraudulent) %>%
  dplyr::summarize(count = n())
  

ggplot(tmp_train, aes(country, ..count..)) + geom_bar(aes(fill = fraudulent), position = "dodge")



ggplot(tmp_train, aes(has_company_logo, ..count..)) + geom_bar(aes(fill = fraudulent), position = "dodge")