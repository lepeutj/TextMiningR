#############################################
library(tm)

# Transforming, and getting all variables
corps=as.character(test$a_comment)
test.tm=cbind(corps,test$nb_proposal>1)
test.tm<-as.data.frame(test.tm)


sms_corpus <- Corpus(VectorSource(test.tm$corps))
print(sms_corpus)

clean_corpus <- tm_map(sms_corpus, content_transformer(tolower))
# remove numbers
clean_corpus <- tm_map(clean_corpus, removeNumbers)
# remove punctuation
clean_corpus <- tm_map(clean_corpus, removePunctuation)
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords(kind="fr"))
clean_corpus <- tm_map(clean_corpus, stripWhitespace)
sms_dtm <- DocumentTermMatrix(clean_corpus)
sms_dtm = removeSparseTerms(sms_dtm, 0.97)

spam_indices <- which(test.tm$V2 == "FALSE")
ok_indices <- which(test.tm$V2 == "TRUE")
#library(wordcloud)
# wordcloud(clean_corpus[ok_indices],min.freq = 200)
# wordcloud(clean_corpus[spam_indices],min.freq = 200)


sms_raw_train <- test.tm[1:3109,]
sms_raw_test <- test.tm[3109:4663,]

#### and the document-term matrix and clean corpus

sms_dtm_train <- sms_dtm[1:3109,]
sms_dtm_test <- sms_dtm[3109:4663,]
sms_corpus_train <- clean_corpus[1:3109]
sms_corpus_test <- clean_corpus[3109:4663]

spam <- subset(sms_raw_train, V2 == "FALSE")
ham <- subset(sms_raw_train, V2 == "TRUE")

five_times_words <- findFreqTerms(sms_dtm_train, 10)
length(five_times_words)


sms_train <- DocumentTermMatrix(sms_corpus_train, control=list(dictionary = five_times_words))

sms_test <- DocumentTermMatrix(sms_corpus_test, control=list(dictionary = five_times_words))
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

sms_train <- apply(sms_train, 2, convert_count)
sms_test <- apply(sms_test, 2, convert_count)

library("e1071")

sms_classifier <- naiveBayes(sms_train, factor(sms_raw_train$V2))
class(sms_classifier)

sms_test_pred <- predict(sms_classifier, newdata=sms_test)
table(sms_test_pred, sms_raw_test$V2)

#####################################
which(colnames(test)==c("fstat"))
which(colnames(test)==c("fstat"))


test<-test[,-29]
test()

train=test[1:3901,-29]
train.cont=


verif=test[3901:4663,]

 getAct<-function(activity){
    ok=NULL
     rslt=NULL
     for(j in 1:length(activity)){
       ok=strsplit(activity[j],",")[[1]]
       test=unlist(strsplit(ok,"-"))
       rslt=rbind(rslt,test[seq(2,76,2)])
     }
     return(rslt)
   }

 act=getAct(as.character(test$q_activity_choices))
act.factor=apply(act,2,as.factor)
table(act.factor[,1],act.factor[,2])
data.frame(table(act.factor[,1:38]))
factor()


wat=matrix(0,38,38)
for (i in 1:38){
  for (j in 1 : 38){
    wat[i,j]=chisq.test(table(act.factor[,i],act.factor[,j]))$p.value
  }
}
