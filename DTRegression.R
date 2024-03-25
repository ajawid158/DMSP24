###Regression Tree
library(ggplot2)
library(rpart)
library(rpart.plot)


#Linear Regression 
x=read.csv("employee.csv")
names(x)

cor(x$Salary, x$Spending)

plot(x$Salary, x$Spending)
scatter.smooth(x$Salary, x$Spending)


#Decission Tree Regression
d=read.csv("tips.csv")
names(d)
cor(d$total_bill, d$tip)

plot(d$total_bill, d$tip)
scatter.smooth(d$total_bill, d$tip)
abline(v=20, col="red")

names(d)
d.sub=d[,c(2,3)]

head(d.sub)
s=sample(nrow(d.sub), floor(0.7*nrow(d.sub)))
g.train=d.sub[s, ]
g.test=d.sub[-s,]
mean(g.train$tip)

plot(g.train$total_bill, g.train$tip)
abline(v=29, col="red", lty=2)
abline(v=14, col="blue", lty=3)
abline(v=18, col="green", lty=3)
abline(v=42, col="darkgreen", lty=3)

fit=rpart(tip~., data = g.train, method = "anova")   #DTR

rpart.plot(fit)
summary(fit)

predict_test=predict(fit, g.test)
View(predict_test)


g.test=cbind(g.test, predict_test)
View(g.test)
er=abs(g.test$tip-g.test$predict_test)
sum(er)


