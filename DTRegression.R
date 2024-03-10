###Regression Tree
library(rpart)
library(rpart.plot)
d=read.csv("tips.csv")
names(d)
d.sub=d[,c(2,3,4)]
head(d.sub)
s=sample(nrow(d.sub), floor(0.7*nrow(d.sub)))
g.train=d.sub[s, ]
g.test=d.sub[-s,]

fit=rpart(tip~., data = g.train, method = "anova")
rpart.plot(fit)
fit
