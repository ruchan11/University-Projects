setwd("C:/Users/macho/Desktop/sfsu/spr22/math448")
gold=read.csv("daily_gold_rate.csv",header=T,na.strings="?")

library(glmnet)
attach(gold)

gold$year=substr(gold$Date,1,4)
gold$month=substr(gold$Date,6,7)

set=c("USD","year","month")
data=gold[,set]

attach(data)

actual=data$USD[6:nrow(data)]
month=data$month[6:nrow(data)]
year=data$year[6:nrow(data)]
lag1=data$USD[5:(nrow(data)-1)]
lag2=data$USD[4:(nrow(data)-2)]
lag3=data$USD[3:(nrow(data)-3)]
lag4=data$USD[2:(nrow(data)-4)]
lag5=data$USD[1:(nrow(data)-5)]




df = data.frame(actual, lag1, lag2, lag3, lag4, lag5, month, year)
df$month=as.factor(as.character(df$month))
df$year=as.numeric(as.character(df$year))

#train and validation setup
x=model.matrix(actual~.,df)[,-1]
y=df$actual
set.seed(1)
train=sample(1:nrow(df),0.8*nrow(df))
df.train=df[train,]
df.test=df[-train,]
x.train=x[train,]
x.test=x[-train,]
y.train=y[train]
y.test=y[-train]

#Linear Regression
ols = lm(actual~., data = df.train)# all predictors
ols = lm(actual~lag1+ lag4+ lag5+ month +year, data = df.train) #significant predictors
summary(ols)
y.pred=predict(ols,newdata=df.test)
mean((y.test-y.pred)^2) #test MSE

#Ridge Regression
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x.train,y.train,alpha=0,lambda=grid, thresh=1e-12)

cv.out=cv.glmnet(x.train,y.train,alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x.test)
mean((ridge.pred-y.test)^2) #test MSE

#Lasso Regression
lasso.mod=glmnet(x.train,y.train,alpha=1,lambda=grid)

cv.out=cv.glmnet(x.train,y.train,alpha=1)
plot(cv.out)#CV error plot
bestlam=cv.out$lambda.min 
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x.test)
mean((lasso.pred-y.test)^2) #test MSE

#PCR
library(pls)

pcr.fit=pcr(actual~., data=df.train,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP") #MSE plotted
pcr.pred=predict(pcr.fit,df.test,ncomp=17)
mean((pcr.pred-y.test)^2) #test MSE

#PLS
pls.fit=plsr(actual~., data=df.train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP") #plot MSE
pls.pred=predict(pls.fit,df.test,ncomp=12)
mean((pls.pred-y.test)^2) #test MSE