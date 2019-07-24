# Import the text mining package tm
library("tm")

# Read the given sms messages; do not factor category variables so
# that each text message is not given its own unique factor number
sms = read.csv("sms_spam.csv",stringsAsFactors = FALSE)

# Convert the target variable into a factor
sms$type <- factor(sms$type)

# Convert sms messages into a Corpus
sms_corpus <- Corpus(VectorSource(sms$text))

# Clean Corpus by 
#  converting to lower case
#  removing numbers
#  removing stop words
#  removing punctuation
#  removing white space
p = tm_map(sms_corpus,tolower)
p = tm_map(p,removeNumbers)
p = tm_map(p,removeWords,stopwords())
p = tm_map(p,removePunctuation)
p = tm_map(p,stripWhitespace)

dtm = DocumentTermMatrix(p)

train = sms[1:4200, ]
test = sms[4201:5574, ]

dtmTrain = dtm[1:4200, ]
dtmTest = dtm[4201:5574, ]

corpusTrain = p[1:4200]
corpusTest = p[4201:5574]

round(prop.table(table(train$type))*100)
round(prop.table(table(test$type))*100)

freqTerms = findFreqTerms(dtmTrain,5)

dtm_Train = DocumentTermMatrix(corpusTrain,list(dictionary = freqTerms))
dtm_Test = DocumentTermMatrix(corpusTest,list(dictionary = freqTerms))

ncol(dtm_Train)
ncol(dtm_Test)

term_counts <- function(x) {
  x <- ifelse(x > 0,1,0)
  x <- factor(x,levels=c(0,1),labels=c("No","Yes"))
  return(x)
}
dtm_Train = apply(dtm_Train,MARGIN = 2,term_counts)
dtm_Test= apply(dtm_Test,MARGIN = 2,term_counts)

library(e1071)

sms_classifier = naiveBayes(dtm_Train,train$type)

testPredicted = predict(sms_classifier,dtm_Test)

table(testPredicted,test$type)
