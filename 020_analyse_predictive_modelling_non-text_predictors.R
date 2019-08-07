#****************************************************************************************
#
# PROJECT: 20180601
#
# MODULE: 020 - ANALYSE - PREDICTIVE MODELLING OF NON-TEXT PREDICTORS
#
# DESCRIPTION:

#
#              
#         
# STEPS
# 1.Set libraries
# 2. Set up data for modelling
# 3. Run GBM
# 4. Run distributed random forest
# 5. Run elastic net glm
#****************************************************************************************


##1. Set libraries ####
library(data.table)
library(dplyr)
library(caret)
library(ggplot2)



library(h2o) #no support for java 9 yet - get errors
localH2O <- h2o.init(nthreads = -1)
 h2o.init()


 ##2. Set up data for modelling ####
 
 #decided to use non-binarised data as model accuracy was 0.1
 set.seed(seed)
 trainIndex <- createDataPartition(ads1$fraudulent, p = .8, 
                                   list = FALSE, 
                                   times = 1)
 
 head(trainIndex)
 #data <- data.frame(data)
 train = ads1[trainIndex,]
 test = ads1[-trainIndex,]
 
 saveRDS(train, "./train.rds")
 saveRDS(test, "./test.rds")

 
train$fraudulent<-ifelse(train$fraudulent==
                                          "0","No","Yes")

 
test$fraudulent<-ifelse(test$fraudulent==
                            0,"No","Yes")

train$fraudulent<-as.factor(train$fraudulent)
test$fraudulent<-as.factor(test$fraudulent)


model_train.h2o <- as.h2o(train)
model_test.h2o <- as.h2o(test)

y_dv<- which(colnames(model_train.h2o)=="fraudulent")
x_iv<-c(1:11)


#3. Run gbm ####


gbm_model <-h2o.gbm(y=y_dv, x=x_iv, training_frame = model_train.h2o, 
                    ntrees =500, max_depth = 4, distribution="bernoulli", #for 0-1 outcomes
                    learn_rate = 0.01, seed = 1234, nfolds = 5, keep_cross_validation_predictions = TRUE)

saveRDS(gbm_model, "./gbm_model.rds")

# Optional: Average the holdout AUCs
cvAUCs <- sapply(sapply(gbm_model@model$cross_validation_models, `[[`, "name"), function(x) { h2o.auc(h2o.getModel(x), valid=TRUE) })

print(cvAUCs)
mean(cvAUCs)

variable.importance.list<-as.data.frame(h2o.varimp(gbm_model))



#Predict on test set
gbm.prediction = h2o.predict(gbm_model, newdata=model_test.h2o, type='response')
gbm.prediction_prob = h2o.predict(gbm_model, newdata=model_test.h2o)[,2]

predicted_values_model<-h2o.make_metrics(gbm.prediction_prob,model_test.h2o$fraudulent)

gbm.auc = h2o.auc(h2o.performance(gbm_model, newdata=model_test.h2o))


#Produce AUC curve

fpr <- h2o.fpr( h2o.performance(gbm_model, newdata=model_test.h2o) )[['fpr']]
tpr <- h2o.tpr( h2o.performance(gbm_model, newdata=model_test.h2o) )[['tpr']]
ggplot( data.table(fpr = fpr, tpr = tpr), aes(fpr, tpr) ) + 
  geom_line() + theme_bw() + ggtitle( sprintf('AUC: %f', gbm.auc) )




#4. Run distributed random forest ####

rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = model_train.h2o,        ## the H2O frame for training
  # validation_frame = valid,      ## the H2O frame for validation (not required)
  x=x_iv,                        ## the predictor columns, by column index
  y=y_dv,                          ## the target index (what we are predicting)
  # model_id = "rf_covType_v1",    ## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 500,                  ## use a maximum of 500 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let 
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
  ##  average is within 0.001 (default) of 
  ##  the prior two 2-tree averages.
  ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 1000000)                ## Set the random seed so that this can be
##  reproduced.

## Keys to look for are validation performance
##  and variable importance
summary(rf1)                     ## View information about the model.



drf_varimp_list<-as.data.frame(h2o.varimp(rf1))

saveRDS(rf1, "./rf_model.rds")

# 5. Run elastic net glm ####
#Conclusion: Use this model best output

glm_model<- h2o.glm(y = y_dv, x = x_iv, training_frame = model_train.h2o,
                    family = "binomial", nfolds = 5, lambda_search = TRUE, alpha=0.5, keep_cross_validation_predictions = TRUE)

# lambda value 	alpha value 	Result
# lambda == 0 	alpha = any value 	No regularization. alpha is ignored.
# lambda > 0 	alpha == 0 	Ridge Regression
# lambda > 0 	alpha == 1 	LASSO
# lambda > 0 	0 < alpha < 1 	Elastic Net Penalty

glm_variable_importance_list<-as.data.frame(h2o.varimp(glm_model))
glm_predict<-h2o.predict(object=glm_model, newdata=model_test.h2o)

glm_predict_df<-as.data.frame(glm_predict)


write_csv(glm_variable_importance_list, "./glm_variable_importance_list.csv")
saveRDS(glm_model, "./glm_model.rds")


