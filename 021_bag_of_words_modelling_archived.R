#****************************************************************************************
#
# PROJECT: 20180601
#
# MODULE: 020 - ANALYSE - BAG OF WORDS MODELLING
#
# DESCRIPTION:

#
#              
#         
# STEPS
# 1.Set libraries and import data
# 2. Remove html tags from data   
# 3. define preprocessing function and tokenization functio       
# 4. Create list of tokens
# 5. Prune vocab to remove most frequent words
# 6. Modelling - GLM net
#****************************************************************************************


## 1. Split data into training and test sets####


library(data.table)
library(text2vec)
library(caret)
library(rvest)
library(glmnet)




#binarise dependent variable
unique_scam_data$cb_fraudulent<-ifelse(unique_scam_data$fraudulent==
                                         "f",0,1)
unique_scam_data$id <- seq.int(nrow(unique_scam_data))

setDT(unique_scam_data)
setkey(unique_scam_data, id)
set.seed(2017L)
all_ids = unique_scam_data$id 

#split data into training and test sets
train_ids = sample(all_ids,round(0.8*length(all_ids)))
test_ids = setdiff(all_ids, train_ids)
train = unique_scam_data[J(train_ids)]
test = unique_scam_data[J(test_ids)]


#2. Remove html tags from data ####



cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

train$job_description<-cleanFun(train$description)
train$job_description<-gsub("[\r\n]", "", train$job_description)
test$job_description<-cleanFun(test$description)
test$job_description<-gsub("[\r\n]", "", test$job_description)


# 3. define preprocessing function and tokenization function ####
prep_fun = tolower
tok_fun = word_tokenizer

# 4. Create list of tokens ####
train_tokens_description = train$job_description %>% 
  prep_fun %>% 
  tok_fun
train_job_description = itoken(train_tokens_description, 
                  ids = train$id,
                  progressbar = FALSE)



test_tokens_description = test$job_description %>% 
  prep_fun %>% 
  tok_fun 
  test_job_description = itoken(test_tokens_description, 
                           ids = test$id,
                           progressbar = FALSE)




# 2 words together
vocab_job_description = create_vocabulary(train_job_description, ngram = c(1L, 2L))








#5. Prune vocab to remove most frequent words ####

vocab_job_description = vocab_job_description %>% prune_vocabulary(term_count_min = 10, 
                                   doc_proportion_max = 0.5)

bigram_vectorizer = vocab_vectorizer(vocab_job_description)

dtm_train_job_description = create_dtm(train_job_description, bigram_vectorizer)

dtm_test_job_description = create_dtm(test_job_description, bigram_vectorizer)

dtm_train_description_l1_norm = normalize(dtm_train_job_description, "l1")
dtm_test_job_description_l1_norm = normalize(dtm_test_job_description, "l1")


#6. Modelling: GLM_net ####

#five folds of cross-validation
# lasso penalty
# interested area unded ROC curve
# 5-fold cross-validation
# again lower number iterations for faster training
# in this vignette
glmnet_classifier = cv.glmnet(x = dtm_train_description_l1_norm, y = train[['cb_fraudulent']], 
                              family = 'binomial', 
                              alpha = 1,
                              type.measure = "auc",
                              nfolds = 5,
                              thresh = 1e-3,
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))


plot(glmnet_classifier)
#model accuracy check


print(paste("max AUC =", round(max(glmnet_classifier$cvm), 5)))


# check performance on test data set
# apply vectorizer

preds = predict(glmnet_classifier, dtm_test_job_description_l1_norm, type = 'response')[,1]

glmnet:::auc(test$cb_fraudulent, preds)

#interpreting results

df<-(coef(glmnet_classifier,"lambda.min"))
head(df[order(df@x),decreasing=FALSE])

write.csv(df,"job_desc_glmnet_results.csv")
