#****************************************************************************************
#
# PROJECT: 20180601
#
# MODULE: 022   TEXT ANALYTICS JOB REQUIREMENTS
#
# DESCRIPTION:

#
#              
#         
# STEPS
# 1.Set libraries and import data
# 2. clean  text data
# 3. Perform sentiment analysis         
# 4. Word clouds
#****************************************************************************************


#1. Set libraries and import data

install.packages("devtools")
install.packages("plyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tm")
install.packages("SnowballC")


require(devtools)
require(plyr)
require(ggplot2)
require(wordcloud)
require(RColorBrewer)
require(tm)
require(SnowballC)
require(sentiment)

install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")


names(new_unique_scam_dt)



all_cols = names(new_unique_scam_dt)

exclude_non_string_cols = c("department","telecommuting", "has_company_logo", "has_questions", "employment_type", "required_experience","required_education","industry",
                            "co_function","in_balanced_dataset","id","salary_range","location")

txt_data<-new_unique_scam_dt[, !c(exclude_non_string_cols), with=FALSE]


txt_data_DT <- data.table(txt_data)


txt_data_DT[is.na(title),title:='UNKNOWN']
txt_data_DT[is.na(company_profile),company_profile:='UNKNOWN']
txt_data_DT[is.na(description),description:='UNKNOWN']
txt_data_DT[is.na(requirements),requirements:='UNKNOWN']
txt_data_DT[is.na(benefits),benefits:='UNKNOWN']

txt_data_ads<-as.data.frame(txt_data_DT)


#2. clean  text data ###

#subset data by whether or not the job ad is fraudulent
fraudulent_requirements <- txt_data_ads[ which(txt_data_ads$fraudulent==1),]
verified_requirements<-txt_data_ads[ which(txt_data_ads$fraudulent==0),]


#functions to clean html strings
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}


jd_text<-cleanFun(fraudulent_requirements$requirements)
jd_text<-gsub("[\r\n]", "", jd_text)

jd_text = gsub("[[:punct:]]", "", jd_text)
jd_text = gsub("[[:digit:]]", "", jd_text)
jd_text = gsub("http\\w+", "", jd_text)

try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

jd_text = sapply(jd_text, try.error)
jd_text = jd_text[!is.na(jd_text)]
names(jd_text) = NULL

#non-fraudulent data cleaning 

n_jd_text<-cleanFun(verified_requirements$requirements)
n_jd_text<-gsub("[\r\n]", "", n_jd_text)

n_jd_text = gsub("[[:punct:]]", "", n_jd_text)
n_jd_text = gsub("[[:digit:]]", "", n_jd_text)
n_jd_text = gsub("http\\w+", "", n_jd_text)

n_jd_text = sapply(n_jd_text, try.error)
n_jd_text = n_jd_text[!is.na(n_jd_text)]
names(n_jd_text) = NULL

#3. Perform sentiment analysis ####
#3.1 fraudulent analysis
# classifying  using a Bayesian analysis.  A polarity of positive, negative, or neutral is determined.  Finally, the comment, emotion, and polarity are combined in a single dataframe.

class_emo = classify_emotion(jd_text, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(jd_text, algorithm="bayes")
polarity = class_pol[,4]


sent_df = data.frame(text=jd_text, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# graph comments
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")


#4. word cloud ####

#prepare data
#4.1 
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = jd_text[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

#create word cloud
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE,
                 title.size = 1.5)


#4.2 non-fraudulent  analysis
class_emo = classify_emotion(n_jd_text, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(n_jd_text, algorithm="bayes")
polarity = class_pol[,4]


sent_df = data.frame(text=n_jd_text, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# graph comments
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")


#prepare data for word cloud
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = n_jd_text[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

#create word cloud
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE,
                 title.size = 1.5)

