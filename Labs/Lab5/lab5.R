library(ISLR)
library(dplyr)
head(Hitters)
dim(Hitters)
is.na(Hitters)

data <- na.omit(Hitters)
dim(data)
head(data)

SalaryPredictModel1 <- lm(Salary ~., data = data)
summary(SalaryPredictModel1)


cooks = cooks.distance(SalaryPredictModel1)
influential = cooks[(cooks > (3*mean(cooks,na.rm=TRUE)))]
influential

names <- names(influential)
names
outliers <- data[names,]
data_no_outliers <- data %>% anti_join(outliers)

SalaryPredictModel2 <- lm(Salary ~., data=data_no_outliers)
summary(SalaryPredictModel2)



set.seed(10)
data1 <- rnorm(50)

set.seed(30)
data2 <- rnorm(50)

shapiro.test(data1)
hist(data1, col='green')

shapiro.test(data2)
hist(data2,col='blue')

set.seed(0)
data3 <- rnorm(100)
shapiro.test(data3)

set.seed(0)
data4 <- rpois(n=100,lambda=3)
shapiro.test(data4)
hist(data4, col='pink')

library(nortest)
set.seed(1)

x <- rnorm(100, 0, 1)
ad.test(x)


set.seed(1)

x <- runif(100,0,1)
ad.test(x)