g=read.csv("grades1.csv")
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
#Problem
#predict the grade class A or Not A
head(g)
g_sub=data.frame(g$Q,g$Oral, g$Grade)
colnames(g_sub)=c("Q","Oral", "Grade")
head(g_sub)
##Split the dataset into train and test
s=sample(nrow(g_sub), floor(0.8*nrow(g_sub)))
g.train=g_sub[s, ]
g.test=g_sub[-s,]
#head(g.train)
#head(g.test)
fit=rpart(Grade~., data = g.train, method = "class")
rpart.plot(fit, extra = 106)
pred=predict(fit, g.test, type = "class")
head(pred)
table(g.test$Grade, pred) #confusion matrix
Err.rate=4/NROW(g.test)
Err.rate #16%
####New Dataset
Q=c(85, 80, 30)
Oral=c(88, 70, 90)
new.st=data.frame(cbind(Q, Oral))
pred.new=predict(fit, new.st, type = "class")
pred.new
