#Random Forest Model
#Load the packages
install.packages("randomForest")
library(dplyr)
library(randomForest)
library(ggplot2)

####+++++++++++++
#1. random forest classification

rfdata=iris
str(rfdata)

#Y: Species
#X: the rest

#Train and test
s=sample(nrow(rfdata), floor(0.7*nrow(rfdata)))

#Training and test
rftrain=rfdata[s,]
rftest=rfdata[-s,]

names(rftrain)
#random forest model
rfm=randomForest(Species~., data = rftrain)

#Make prediction 
spredict=predict(rfm, rftest)

#Confusion matrix
cm=table(rftest$Species, spredict)
cm
cac=sum(diag(cm))/sum(cm)
cac


####+++++++++++++
#2. Random forest Regression
tip=read.csv("tips.csv")
head(tip)
#RF Model 
rfrm=randomForest(x=tip[2],
                  y=tip$tip, 
                  ntree = 500)
plot(rfrm)
min(rfrm$mse)
#Predict
y_pre=predict(rfrm, newdata = data.frame(total_bill=30))
y_pre
#plot using ggplot2
x_grid=seq(min(tip$total_bill), max(tip$total_bill), 0.01)

g=ggplot()+
  geom_point(aes(x=tip$total_bill, y=tip$tip), colour = "red")+
  geom_line(aes(x=x_grid, y=predict(rfrm, newdata =data.frame(total_bill=x_grid))), colour = 'blue')+
  ggtitle("tota_bill vs Tip")+
  ylab("Tip")+
  xlab("total_bill")
g





