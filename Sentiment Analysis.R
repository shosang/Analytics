library(tidytext)
library(tidyverse)
library(tm)
library(syuzhet)
pacman::p_load("tidyverse","mice","caret","readxl","ROCR","lift","glmnet","MASS","e1071","DMwR","randomForest") 

# 1 = positive
# 0 = negative


file_df <- read.delim(file.choose(), header=FALSE) %>%
  mutate(line = 1:n())
names(file_df) <- c("text", "sentiment", "line")
file_df$text <- as.character(file_df$text)

# Lexicon approach with tidytext
text_df <- file_df %>%
  unnest_tokens(output=word, input=text) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(line) %>%
  summarize(score = sum(score)) %>%
  mutate(lexicon = ifelse(score>=0, 1, 0))

# Confusion matrix - lexicon approach
text_df_join <- text_df %>%
  inner_join(file_df, by="line")
confusionMatrix(as.factor(text_df_join$sentiment),as.factor(text_df_join$lexicon))


# Lexicon approach with syuzhet
syu <- file_df 
syu$try <- NULL
for (i in 1:nrow(syu)) {
  syu$try[i] = sum(get_sentiment(get_tokens(syu$text[i])))
}

# ML Approach
# Create Corpus
docs <- Corpus(VectorSource(file_df$text))
inspect(docs)

# Preprocessing
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWhitespace)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs,stemDocument)
docs <- tm_map(docs, removeWords, stopwords("english"))

#inspect a particular document
writeLines(as.character(docs[[30]]))
writeLines(as.character(docs[[60]]))
writeLines(as.character(docs[[90]]))
writeLines(as.character(docs[[120]]))
writeLines(as.character(docs[[150]]))
writeLines(as.character(docs[[180]]))


# Document Term Matrix
dtm <- DocumentTermMatrix(docs)

# Drop sparce words
dtm_sparce <- removeSparseTerms(dtm, 0.99)
inspect(dtm_sparce)

# Create final dataset
df_dtm <- tidy(dtm_sparce) %>%
  spread(key=term, value=count)
df_dtm <- as.data.frame(df_dtm) 
df_dtm[is.na(df_dtm)] <- 0
names(df_dtm)[1] <- "line"
df_dtm$line <- as.integer(df_dtm$line)
sum(is.na(df_dtm))

df_bow <- df_dtm %>%
  inner_join(file_df[,c("line","sentiment")], by="line")
df_bow <- df_bow[complete.cases(df_bow), ]
sum(is.na(df_bow))
colnames(df_bow) <- paste(colnames(df_bow), "_c", sep = "")

# Ratio of positive to negative - nt imbalanced
prop.table(table(df_bow$sentiment))

# Split into train and test
id <- createDataPartition(df_bow$sentiment, p = 2/3,list = FALSE)
dtm_train <- df_bow[id,]
dtm_test <- df_bow[-id,]

# Random forest
model_forest <- randomForest(as.factor(sentiment_c) ~ . - line_c, data=dtm_train, 
                             importance=TRUE,proximity=TRUE, ntree=50,
                             cutoff = c(0.5, 0.5),type="classification")

# Finding predicitons
forest_probabilities<-predict(model_forest,newdata=dtm_test,type="prob") 
forest_classification<-rep("1",nrow(dtm_test))
forest_classification <- as.integer(forest_probabilities[,2] > mean(dtm_test$sentiment_c == "1"))
forest_classification<-as.factor(forest_classification)
dtm_test$ml <- forest_classification

# Confusion matrix  
confusionMatrix(forest_classification,as.factor(dtm_test$sentiment_c))

# ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], dtm_test$sentiment_c)
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") 
plot(forest_ROC) 

# AUC 
auc.tmp <- performance(forest_ROC_prediction,"auc") 
forest_auc_testing <- as.numeric(auc.tmp@y.values)
forest_auc_testing 


# Logistic Regression
model_logit<-glm(sentiment_c ~ . - line_c, data=dtm_train, family="binomial"(link="logit"))
summary(model_logit) 

# Finding predicitons
logistic_probabilities<-predict(model_logit,newdata=dtm_test,type="response") 
logistic_classification<-rep("1",nrow(dtm_test))
logistic_classification <- as.integer(logistic_probabilities > mean(dtm_test$sentiment_c == "1"))
logistic_classification<-as.factor(logistic_classification)

# Confusion matrix  
confusionMatrix(logistic_classification,as.factor(dtm_test$sentiment_c))



