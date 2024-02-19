
#Supervised Learning methods
#1.Regression and Prediction############################

x=read.csv("employee.csv")
View(x)
names(x)

dim(x)

##Probem: we want to predict Employee's Spending using a vector of 
#relevant predictors X, such as Salary, Gender, Working Hrs, etc.


#Step 1: check their correlation 
x.sub=data.frame(x$Spending, x$Salary, x$WrH, x$GenCode)
head(x.sub)
names(x.sub)=c("Spending", "Salary", "WH", "Gender")
View(x.sub)

x.cor=cor(x.sub)   #correlation matrix
x.cor

library(corrplot)

corrplot(x.cor, method = "pie", 
         type="lower")


##Step 2: split the dataset into training and test 

nrow(x)
0.7*30

s=sample(nrow(x), 21)
s
x.train=x[s,]
View(x.train)

x.test=x[-s,]
View(x.test)
head(x.train)

#Step 3: Construct the prediction models using x.train 
#Step 3.1. Model 0
#Regression and Prediction
names(x.train)
#Predict Spending using Salary
cor(x.train$Spending, x.train$Salary)
plot(x.train$Salary, x.train$Spending, 
     col='red')

#Model0
#Y=Spending
#X=Salary

lm0=lm(Spending~Salary, data = x.train)

#Generat the predicted values for Spending using the model
y.pred=lm0$fitted.values   #we made predictions 
y.pred

plot(x.train$Salary, y.pred, 
     col='red')

#put them together

##Probem: we want to predict Employee's Spending using a vector of 
#relevant variables such as Salary, Gender, Working Hrs

#Step 1: check their correlation 
x.sub=data.frame(x$Spending, x$Salary, x$WrH, x$GenCode)
names(x.sub)=c("Spending", "Salary", "WH", "Gender")
View(x.sub)
x.cor=cor(x.sub)
x.cor

library(corrplot)
corrplot(x.cor, method = "pie", 
         type="lower")
##Step 2: split the dataset into trainig and test 
library(dplyr)
nrow(x)
0.8*30

s=sample(nrow(x), 24)
s
x.train=x[s,]
x.test=x[-s,]

##Step 3: Construct the prediction models using x.train 
##Step 3.1. Model 0
###Regression and Prediction
names(x.train)
lm0=lm(Spending~Salary, data = x.train)
y.pred=lm0$fitted.values


all.y=data.frame(x.train$Spending, y.pred)
View(all.y)


#use the model to predict Spending using Salary in the test dataset
#We expose our lm0 to NEW DATA and see how it does?

lm0.test=predict(lm0, x.test)
lm0.test  #these are the predicted spendings of the 9 objects in the test dataset using lm0, the model we have constructed earlier using trainin dataset


y.test=data.frame(x.test$Spending, lm0.test,x.test$Spending-lm0.test)
View(y.test)

sum(y.test$x.test.Spending...lm0.test)
###########################
##Sum of Squared Error

SSE0=sum((x.test$Spending- lm0.test)^2)
SSE0   #36.6

#Step 4: WrHr
#Add another variable

lm1=lm(Spending~Salary+WrH, data = x.train)

lm1.test=predict(lm1, x.test)
lm1.test
SSE1=sum((x.test$Spending- lm1.test)^2)
SSE1  #37.6


##Model 2
#Add another predictor: Gender

lm2=lm(Spending~Salary+WrH+GenCode, data = x.train)
lm2.test=predict(lm2, x.test)
lm2.test
SSE2=sum((x.test$Spending- lm2.test)^2)
SSE2  #40

#Step 5: Make a decision which model to go for?
#choose the best Model Based on SSE
#Hence Model0

#Model0:lm0 we use Salary to predict spending

#Next, make predictions using model 0
##Make prediction with the model
View(x)
# New employees are introduced 
#Spending of these three new employees are NOT observed
new.emp=data.frame(Salary = c(34, 50, 30))
names(new.emp)="Salary"
new.emp
pspending.new.empl=predict(lm0,new.emp)
pspending.new.empl

##the new epm spends about 21600

#Y the amount of purchase one makes when they visit an online shop
#X the time the spend on the website, Location, how many time per week they visit

#LmBest

lm0.test=predict(lm0, x.test)
y.test=data.frame(x.test$Spending, lm0.test)
View(y.test)
###########################
SSE0=sum((x.test$Spending- lm0.test)^2)
SSE0

#Step 4:
lm1=lm(Spending~Salary+WrH, data = x.train)
lm1.test=predict(lm1, x.test)
SSE1=sum((x.test$Spending- lm1.test)^2)
SSE1

##Model 2

lm2=lm(Spending~Salary+WrH+GenCode, data = x.train)
lm2.test=predict(lm2, x.test)
SSE2=sum((x.test$Spending- lm2.test)^2)
SSE2

#Step 5: choose the best Model  Model 0 entailed the lowest error
##Make prediction with the model
View(x)
# a New employee comes with salary of 34 T
new.emp=data.frame(Salary = c(34, 50, 30))

names(new.emp)="Salary"
predict(lm0,new.emp)
##the new epm spends about 21600