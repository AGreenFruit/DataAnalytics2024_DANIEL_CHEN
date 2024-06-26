library(e1071)
set.seed(1)

x = matrix(rnorm(20*2),ncol=2)
y = c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x,col=(3-y))

dat <- data.frame(x=x,y=as.factor(y))
svmfit <- svm(y~.,data=dat,kernel='linear',cost=10,scale=FALSE)
plot(svmfit, dat)

svmfit$index
summary(svmfit)

svmfit <- svm(y~.,data=dat,kernel='linear',cost=.1,scale=FALSE)
plot(svmfit, dat)
svmfit$index

set.seed(1)
tune.out <- tune(svm,y~.,data=dat,kernel='linear',ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

xtest = matrix(rnorm(20*2),ncol=2)
ytest = sample(c(-1,1),20,rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat = data.frame(x=xtest,y=as.factor(ytest))

ypred = predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)

svmfit <- svm(y~.,data=dat,kernel="linear",cost=.01,scale=FALSE)
ypred = predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y)

x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

dat = data.frame(x=x,y=as.factor(y))
svmfit <- svm(y~.,data=dat,kernel="linear",cost=1e5)
summary(svmfit)
plot(svmfit,dat)

svmfit <- svm(y~.,data=dat,kernel="linear",cost=1)
summary(svmfit)
plot(svmfit,dat)

library(e1071)
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)

dat <- data.frame(x=Khan$xtrain,y = as.factor(Khan$ytrain))
out <- svm(y ~.,data=dat,kernel="linear",cost=10)
summary(out)

dat.te=data.frame(x=Khan$xtest , y = as.factor(Khan$ytest ))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)