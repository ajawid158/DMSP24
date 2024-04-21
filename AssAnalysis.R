##+++++++++++++++++++++++++++++++++++++++++++++##
#                                               #
#                Association Analysis           #
#                                               #
##++++++++++++++++++++++++++++++++++++++++++++++#
#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)
m=read.csv("market.csv")
View(m)
q=as.matrix(m)   ##csv to matrix 
q=as(q, "transactions")
##View it 
inspect(q)
summary(q)   #there is 1 person bought all 5 items
#there are 6 customers bought 4 items together
###Check the items support::::m/n, eg. TP=5/31=0.16
itemFrequency(q)
itemFrequency(q[,4])
itemFrequencyPlot(q)
itemFrequencyPlot(q, support=0.6)
#Conf(X-->Y)=Pr(Y|X) :: the prob of Y Given X=Pr(X,Y)/Pr(X)=sup(X,Y)/sup(X)
####confidence Use the default
m1=apriori(q)
m1
inspect(m1)
q.rules=apriori(q, parameter = list(supp=0.1, conf=0.4, minlen=3))

#minlen=is the number of items
#summary(q.rules)
inspect(q.rules)


#Conf(TP-->Butter)=100%m Lik of Butter given that TP is already there =100%
#Conf(OR,Ap-->Bread)=86% that is the lik of Bread being bought given that 
#Orange and Apri are already there is =86%