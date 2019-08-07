#****************************************************************************************
#
# PROJECT: 20180601
#
# MODULE: 010 - SOURCE - Import and data ETL
#
# DESCRIPTION:

#
#              
#         
# STEPS
# 1.Set libraries and import data
# 2. Data cleaning    
# 3. Partition data to test and train datasets
#
#****************************************************************************************


## 1. Set libraries and import data ####


library(data.table)
library(readr)
library(dplyr)
library(caret)
library(dummies)





scam_data<-read_csv("C:\\Users\\sheen\\Desktop\\Employment Scam Dataset\\emscad_v1.csv")

seed=1270
## 2. Data cleaning ####
# Remove duplicate rows
unique_scam_data<-unique(scam_data)

unique_scam_data$id <- seq.int(nrow(unique_scam_data))

# #remove na rows
# complete_scam_data<-na.omit(unique_scam_data)

names(unique_scam_data)[16]<-paste("co_function")

#replace missing values (NAs) with unknown for nominal variables
unique_scam_DT <- data.table(unique_scam_data)


unique_scam_DT[is.na(location),location:='UNKNOWN']
unique_scam_DT[is.na(department),department:='UNKNOWN']
unique_scam_DT[is.na(salary_range),salary_range:='UNKNOWN']



unique_scam_DT[is.na(employment_type),employment_type:='UNKNOWN']
unique_scam_DT[is.na(required_experience),required_experience:='UNKNOWN']
unique_scam_DT[is.na(required_education),required_education:='UNKNOWN']
unique_scam_DT[is.na(industry),industry:='UNKNOWN']
unique_scam_DT[is.na(co_function),co_function:='UNKNOWN']

unique_scam_DT[,fraudulent := ifelse(fraudulent =="f",0,1)]
unique_scam_DT[,telecommuting := ifelse(telecommuting =="f",0,1)]
unique_scam_DT[,has_company_logo := ifelse(has_company_logo =="f",0,1)]
unique_scam_DT[,has_questions := ifelse(has_questions =="f",0,1)]
unique_scam_DT[,in_balanced_dataset := ifelse(in_balanced_dataset =="f",0,1)]


#only retain data that is within the balanced dataset
new_unique_scam_dt<-sub <- subset(unique_scam_DT, in_balanced_dataset == '1')

names(new_unique_scam_dt)

all_cols = names(new_unique_scam_dt)

exclude_cols = c("description", "company_profile", "requirements", "benefits", "title","in_balanced_dataset","id")


tmp1<-new_unique_scam_dt[, !c(exclude_cols), with=FALSE]

required_cols <- names(tmp1)

ads1<-tmp1[,(required_cols):= lapply(.SD, as.factor), .SDcols = required_cols]


#binarise factor variables to improve understanding of variable coefficients


ads2=ads1[,-c(12)] #exclude response column
ads3 <- dummy.data.frame(ads2, sep = "_")

#convert the dummy variables to factor variables


ads3[sapply(ads3, is.numeric)] <- lapply(ads3[sapply(ads3, is.numeric)], as.factor)

#merge dataset with response variable
ads4 = cbind(ads3,ads1[,c(12)])
data = data.table(ads4)


# format column names
names(data) <- tolower(gsub('\\.', '_',(gsub('-', '', names(data)))))
names(data) <-  gsub("[[:punct:]]", "", names(data))
names(data)<-gsub(" ", '_',names(data))
names(data)<-gsub(" ", '',names(data))

#column names an issue - h2o won't run
valid_column_names <- make.names(names=names(data), unique=TRUE, allow_ = TRUE)
names(data) <- valid_column_names


## 3. partition data to test and train datasets ####
set.seed(seed)
trainIndex <- createDataPartition(data$fraudulent, p = .8, 
                                  list = FALSE, 
                                  times = 1)

head(trainIndex)
#data <- data.frame(data)
train = data[trainIndex,]
test = data[-trainIndex,]

saveRDS(train, "./train.rds")
saveRDS(test, "./test.rds")
