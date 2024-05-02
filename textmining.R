#install.packages("tm")
library(tm)
#install.packages("snowballC")
library(SnowballC)
library(dplyr)
#install.packages('pacman')
#install.packages('qdap')
library(qdap)
#library(qdapTools)
#library(tidyverse)
#upload the dataset.

ibis=read.csv("ibishotel.csv", stringsAsFactors = FALSE)
View(ibis)
str(ibis)
head(ibis)

ibis=ibis %>%
  mutate(positive=Rating>3)
head(ibis)
View(ibis)

ibis=ibis %>%
  mutate(y=ifelse(positive==F, 0, 1))
head(ibis)
View(ibis)

fr=freq_terms(ibis$Reviews,20) #top 20 most frequent terms
fr
plot(fr)

##Build Corpus>>document

ibiscrp=Corpus(VectorSource(ibis$Reviews))
inspect(ibiscrp)

#view the review
ibiscrp[[2]]$content
content(ibiscrp[[20]])

##Pre-processing
##lower case

ibiscrp =ibiscrp %>%
  tm_map(tolower)
ibiscrp[[2]]$content

#Remove punctuation
ibiscrp =ibiscrp %>%
  tm_map(removePunctuation)
ibiscrp[[2]]$content

##Remove Stopwords
stopwords("english")   ##stopwords in English
ibiscrp= ibiscrp %>%
  tm_map(removeWords, c("hotel", "koblenz", stopwords("english")))
ibiscrp[[2]]$content

#4 Stemming 
ibiscrp=ibiscrp %>%
  tm_map(stemDocument)
ibiscrp[[2]]$content

##remove numbers
ibiscrp = ibiscrp %>%
  tm_map(removeNumbers)
ibiscrp[[2]]$content

#Strip white space 
ibiscrp = ibiscrp %>%
  tm_map(stripWhitespace)
ibiscrp[[2]]$content

fr=freq_terms(ibiscrp, 50)
plot(fr)
fr








###customers feedback on Apple's product

t=read.csv("tweets.csv", stringsAsFactors = FALSE)
##structure of data 
##
View(t)

t= t%>%
  mutate(Negative=Avg<=-1)
table(t$Negative)
View(t)
dim(t)
#term_count=freq_terms(t$Tweet)
#View(term_count)
tfreq=freq_terms(t$Tweet, 10)
plot(tfreq)
##curpus//set of documents
crps=Corpus(VectorSource(t$Tweet))

crps[[3]]$content


##preprocessing
#1 lower case
crps = crps %>%
  tm_map(tolower)
crps[[2]]$content

#2 remove the punctuations
crps=crps%>%
  tm_map(removePunctuation)
crps[[1]]$content

#3 remove stopwords
stopwords("english")

crps= crps %>%
  tm_map(removeWords, c("apple", stopwords("english")))
crps[[2]]$content

#4 Stemming 
crps=crps %>%
  tm_map(stemDocument)
crps[[2]]$content

#5 Strip white space 
crps = crps %>%
  tm_map(stripWhitespace)
crps[[2]]$content

x=freq_terms(crps, 20)
plot(x)

#Features extraction

freq=DocumentTermMatrix(crps)  #transform the words into Features
freq

inspect(freq[1:10,1:5])   ##first 10 rows of the first 5 cols
inspect(freq)

findFreqTerms(freq)
x=findFreqTerms(freq, lowfreq = 50)   ### create a sub matrix with at least 20 times mentioned
length(x)

#Take care of so many terms/columns/features with so many 0s/not mentioned many times, high sparsity
#

freq1=removeSparseTerms(freq, 0.98)  #keep terms which are mentioned in 2% or more of the reviews 100%-99%=1% 
freq1
tspar=as.data.frame(as.matrix(freq1))
head(tspar)
dim(tspar)
colnames(tspar)=make.names(colnames(tspar))
View(tspar)
sum(tspar$best)   #how many times best is mentioned
hist(tspar$iphon)



##Sentiment Analysis
#install.packages("caTools")
library(caTools)

tspar=tspar %>%
  cbind(t$Negative)
View(tspar)
dim(tspar)
sum(tspar$care)

set.seed(123)
splt=sample.split(tspar$`t$Negative`, SplitRatio = 0.7)
train=subset(tspar, splt==TRUE)
test=subset(tspar, splt==FALSE)

##
library(rpart)
library(rpart.plot)
tweetcart=rpart(`t$Negative`~., data = train, method = "class")
prp(tweetcart)
predcart=predict(tweetcart, newdata=test, type="class")
table(test$`t$Negative`, pred)




##+++++++++++++++++++++++++++++++++++++++++++++##
#                                               #
#  Sentiment Analysis with ibis Hotel data      #
#            19. April. 2023
#                                               #
##++++++++++++++++++++++++++++++++++++++++++++++#
#required packages
library(tm)
library(SnowballC)
library(dplyr)
library(qdap)

##upload the dataset 
ibis=read.csv("ibishotel.csv", stringsAsFactors = FALSE)
View(ibis)
str(ibis)

##frequency of the words in the Review column 
fr=freq_terms(ibis)
fr
plot(fr)

##Creat documents
ibiscr=Corpus(VectorSource(ibis$Reviews))
ibiscr[[2]]$content

#1. trun all words to lower case
ibiscr= ibiscr %>%
  tm_map(tolower)
ibiscr[[1]]$content

#2. Remove punctuations
ibiscr = ibiscr %>%
  tm_map(removePunctuation)
ibiscr[[2]]$content

#3. removing the stopwords 
stopwords("english")
ibiscr =ibiscr %>%
  tm_map(removeWords, c("hotel", "koblenz", "ibis", stopwords("english")))
ibiscr[[2]]$content

#4. Remove numbers
ibiscr = ibiscr %>%
  tm_map(removeNumbers)
ibiscr[[2]]$content

#5. stripping the white space
ibiscr= ibiscr %>%
  tm_map(stripWhitespace)
ibiscr[[1]]$content

#6. Stemming
ibiscr = ibiscr %>%
  tm_map(stemDocument)
ibiscr
ibiscr[[1]]$content
fr1=freq_terms(ibiscr, 30)
plot(fr1)
View(ibiscr)


#Feature Extraction 
ibisfreq=DocumentTermMatrix(ibiscr)

ibisfreq
inspect(ibisfreq)
inspect(ibisfreq)[1:4, 1:3]
findFreqTerms(ibisfreq)
l=findFreqTerms(ibisfreq, lowfreq = 8)
length(l)
##we have many features with too many zeros, high sparsity
ibissparse=removeSparseTerms(ibisfreq, 0.90)  
##keep the terms that appears only in 10% of more of the feedback
ibissparse
inspect(ibissparse)
##convert it to dataframe 
ibis_review=as.data.frame(as.matrix(ibissparse))
View(ibis_review)

###visualize the freq of terms

ibis_names=colnames(ibis_review)
View(ibis_names)

ibis_freq=c()

for (i in 1:25){
  ibis_freq[i]=sum(ibis_review[,i])
}
View(ibis_freq)
barplot(ibis_freq,
        col=rainbow(25), 
        names.arg = ibis_names)

####Sentiment analysis 
#create your sentiment variable
View(ibis)
rate=ibis$Rating
View(ibis_review)
ibis_review= ibis_review %>%
  mutate(y=ifelse(rate>3, "Positive","Negative"))
View(ibis_review)

##which terms derive positive rating
#decision tree
library(rpart)
library(rpart.plot)
ibis_sent=rpart(y~.,
                data = ibis_review,
                method = "class")
prp(ibis_sent)
rpart.plot(ibis_sent, extra = 106)

#somehow surprising!

sb=ibis_review %>%
  select(breakfast,y)

View(sb)
table(sb)
##you may use this to make prediction model