set.seed(12345)
par(mar=rep(0.2,4))
data_Matrix<-matrix(rnorm(400),nrow=40)
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])
par(mar=rep(0.2,4))
heatmap(data_Matrix)

set.seed(678910)
for(i in 1:40){
  coin_Flip <- rbinom(1,size=1,prob=.5)
  if(coin_Flip){
    data_Matrix[i,]<-data_Matrix[i,]+rep(c(0,3),each=5)
  }
}
par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

par(mar=rep(0.2, 4))
heatmap(data_Matrix)


hh<-hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1, ,xlab="The Row Mean",ylab="Row",pch=19)
plot(colMeans(data_Matrix_Ordered), xlab="Column",ylab="Column Mean",pch=19)




abalone <- read.csv("C:/Users/chend17/Downloads/DataAnalytics/DataAnalytics2024_DANIEL_CHEN/Labs/abalone.csv")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight','rings' )
summary(abalone)
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)
z <- abalone
aba <- abalone
aba$sex <- NULL
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)
library(class)
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred




library(ggplot2)
head(iris)
sapply(iris[,-5], var)
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()
set.seed(300)
k.max <- 12
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)


library(titanic)
library(rpart)
library(rpart.plot)
library(party)
library(plyr)
library(readr)

data(titanic)
View(Titanic)
titanicTree <- rpart(Survived ~ ., titanic_train,method = "class")
rpart.plot(titanicTree)
tree<-ctree(Survived ~ ., data=Titanic)
plot(tree)