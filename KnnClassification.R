g=read.csv("grades1.csv")
head(g)

#Problem: predict whether a new student belogs to A or Not A group 
#using their midterm and Quizzes scores

###create predictor matrix and classification matrix
g.x=g[,c(3,4)]
head(g.x)

g.y=g[,8]
head(g.y)
####Split the dataset

s=sample(nrow(g), floor(0.8*nrow(g)))
g.train=g.x[s, ]
g.test=g.x[-s, ]

train.y=g.y[s]
test.y=g.y[-s]
NROW(train.y)

###Specify k
sqrt(nrow(g))  ###k=11 better to be odd
#install.packages("class")
library(class)

g.knn=knn(train = g.train, test = g.test, cl = train.y, k=11)
table(g.knn)
table(test.y)
g.pr=data.frame(g.test$MT, g.test$Q, test.y, g.knn)
View(g.pr)
##Confusion matrix
table(test.y, g.knn)

err.knn= 2/NROW(test.y)
err.knn

######################New data 
MT=c(98, 70, 60, 93, 60)
Q=c(92, 89, 78, 91, 80)
###where Knn would classify them 
pre.new=data.frame(cbind(MT, Q))

pre.new

g.knn1=knn(train = g.train, test = pre.new, cl = train.y, k=11)
g.knn1
table(g.knn1)